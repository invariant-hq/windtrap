(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type instrumented_file = {
  filename : string;
  points : int array;
  counts : int array;
}

type coverage = (string, instrumented_file) Hashtbl.t

let file_identifier = "WINDTRAP-COVERAGE-1"

(* ───── Serialization ───── *)

let write_int buffer i =
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer (string_of_int i)

let write_string buffer s =
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer (string_of_int (String.length s));
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer s

let write_array write_element buffer a =
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer (string_of_int (Array.length a));
  Array.iter (write_element buffer) a

let write_list write_element buffer l =
  Buffer.add_char buffer ' ';
  Buffer.add_string buffer (string_of_int (List.length l));
  List.iter (write_element buffer) l

let write_instrumented_file buffer { filename; points; counts } =
  write_string buffer filename;
  write_array write_int buffer points;
  write_array write_int buffer counts

let serialize_files files =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer file_identifier;
  write_list write_instrumented_file buffer files;
  Buffer.contents buffer

let values tbl = Hashtbl.fold (fun _ file acc -> file :: acc) tbl []
let serialize coverage = serialize_files (values coverage)

(* ───── Deserialization ───── *)

let junk channel = try ignore (input_char channel) with End_of_file -> ()

let read_int buffer channel =
  Buffer.clear buffer;
  let rec loop () =
    match input_char channel with
    | exception End_of_file -> ()
    | ' ' -> ()
    | c ->
        Buffer.add_char buffer c;
        loop ()
  in
  loop ();
  int_of_string (Buffer.contents buffer)

let read_string buffer channel =
  let length = read_int buffer channel in
  let s = really_input_string channel length in
  junk channel;
  s

let read_array read_element buffer channel =
  let length = read_int buffer channel in
  Array.init length (fun _ -> read_element buffer channel)

let read_list read_element buffer channel =
  read_array read_element buffer channel |> Array.to_list

let read_instrumented_file buffer channel =
  let filename = read_string buffer channel in
  let points = read_array read_int buffer channel in
  let counts = read_array read_int buffer channel in
  if Array.length points <> Array.length counts then
    failwith "points/counts length mismatch";
  { filename; points; counts }

let read_file path =
  let channel = try Some (open_in_bin path) with Sys_error _ -> None in
  match channel with
  | None -> []
  | Some channel -> (
      try
        let id_len = String.length file_identifier in
        let magic = really_input_string channel id_len in
        if magic <> file_identifier then begin
          close_in_noerr channel;
          []
        end
        else begin
          junk channel;
          let buffer = Buffer.create 4096 in
          let result = read_list read_instrumented_file buffer channel in
          close_in_noerr channel;
          result
        end
      with
      | End_of_file | Failure _ ->
          close_in_noerr channel;
          []
      | exn ->
          close_in_noerr channel;
          raise exn)

(* ───── Accumulated Visit Counts ───── *)

let coverage : coverage Lazy.t = lazy (Hashtbl.create 17)
let data () = Lazy.force coverage

let reset_counters () =
  Hashtbl.iter
    (fun _ { counts; _ } ->
      let n = Array.length counts in
      if n > 0 then Array.fill counts 0 n 0)
    (data ())

(* ───── File Output ───── *)

let prng = Random.State.make_self_init ()

let random_filename ~prefix =
  prefix
  ^ string_of_int (abs (Random.State.int prng 1_000_000_000))
  ^ ".coverage"

let default_coverage_file = ref "windtrap"
let default_log_file = ref "windtrap-coverage.log"
let set_output_prefix path = default_coverage_file := path

let full_path fname =
  if Filename.is_implicit fname then
    Filename.concat Filename.current_dir_name fname
  else fname

let rec mkdir_p d =
  if d = "" || d = "." then ()
  else if Sys.file_exists d then ()
  else begin
    mkdir_p (Filename.dirname d);
    try Unix.mkdir d 0o770 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let log_error =
  lazy
    (match Sys.getenv_opt "WINDTRAP_COVERAGE_LOG" with
    | Some s when String.uppercase_ascii s = "SILENT" -> fun _ -> ()
    | Some s when String.uppercase_ascii s = "STDERR" ->
        fun msg -> prerr_endline (" *** " ^ msg)
    | opt ->
        let path = match opt with Some p -> p | None -> !default_log_file in
        let oc =
          lazy
            (let oc = open_out_bin (full_path path) in
             at_exit (fun () -> close_out_noerr oc);
             oc)
        in
        fun msg -> Printf.fprintf (Lazy.force oc) " *** %s\n" msg)

let log_error msg = (Lazy.force log_error) msg

let file_channel () =
  let prefix =
    full_path
      (match Sys.getenv_opt "WINDTRAP_COVERAGE_FILE" with
      | Some p -> p
      | None -> !default_coverage_file)
  in
  let dir = Filename.dirname prefix in
  mkdir_p dir;
  let rec create_file () =
    let filename = random_filename ~prefix in
    try
      let fd = Unix.(openfile filename [ O_WRONLY; O_CREAT; O_EXCL ] 0o644) in
      let channel = Unix.out_channel_of_descr fd in
      Some channel
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> create_file ()
    | Unix.Unix_error (code, _, _) ->
        log_error
          (Printf.sprintf "Unable to create coverage file: %s: %s"
             (Unix.error_message code) filename);
        None
  in
  create_file ()

let dump () =
  let cov = data () in
  match values cov with
  | [] -> ()
  | files -> (
      match file_channel () with
      | None -> ()
      | Some channel ->
          (try
             output_string channel (serialize_files files);
             flush channel
           with _ -> log_error "Unable to write coverage file");
          close_out_noerr channel)

let coverage_written = ref false

let dump_at_exit () =
  if not !coverage_written then begin
    coverage_written := true;
    dump ()
  end

let register_dump : unit Lazy.t = lazy (at_exit dump_at_exit)

(* ───── Merging ───── *)

let saturating_add x y = if x > max_int - y then max_int else x + y

let elementwise_saturating_add xs ys =
  let longer, shorter =
    if Array.length xs >= Array.length ys then (xs, ys) else (ys, xs)
  in
  let result = Array.copy longer in
  shorter |> Array.iteri (fun i v -> result.(i) <- saturating_add v result.(i));
  result

let merge tbl files =
  List.iter
    (fun (file : instrumented_file) ->
      match Hashtbl.find_opt tbl file.filename with
      | None -> Hashtbl.replace tbl file.filename file
      | Some prev ->
          let counts = elementwise_saturating_add file.counts prev.counts in
          Hashtbl.replace tbl file.filename { prev with counts })
    files

(* ───── Reporting ───── *)

type summary = { visited : int; total : int }

type file_report = {
  filename : string;
  summary : summary;
  uncovered_offsets : int list;
  uncovered_lines : int list;
  source_available : bool;
}

let file_summary { counts; _ } =
  let total = Array.length counts in
  let visited =
    Array.fold_left (fun acc c -> if c > 0 then acc + 1 else acc) 0 counts
  in
  { visited; total }

let summarize cov =
  Hashtbl.fold
    (fun _ file acc ->
      let s = file_summary file in
      { visited = acc.visited + s.visited; total = acc.total + s.total })
    cov { visited = 0; total = 0 }

let summarize_per_file cov =
  Hashtbl.fold
    (fun _ (file : instrumented_file) acc ->
      (file.filename, file_summary file) :: acc)
    cov []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let percentage { visited; total } =
  if total > 0 then float_of_int visited *. 100. /. float_of_int total else 100.

let coverage_style s =
  let pct = percentage s in
  if pct >= 80. then `Green else if pct >= 60. then `Yellow else `Red

let uncovered_offsets { points; counts; _ } =
  let n = min (Array.length points) (Array.length counts) in
  let rec loop i acc =
    if i = n then List.rev acc
    else
      let acc = if counts.(i) = 0 then points.(i) :: acc else acc in
      loop (i + 1) acc
  in
  loop 0 []

let normalize_source_paths = function
  | [] -> [ Filename.current_dir_name ]
  | xs -> xs

let dedup_preserving_order paths =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | path :: rest when List.mem path seen -> loop seen acc rest
    | path :: rest -> loop (path :: seen) (path :: acc) rest
  in
  loop [] [] paths

let source_candidates ~source_paths filename =
  filename
  :: List.map
       (fun source_path -> Filename.concat source_path filename)
       source_paths
  |> dedup_preserving_order

let with_open_in_bin path f =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> f channel)

let read_file_contents path =
  try
    Some
      (with_open_in_bin path (fun channel ->
           really_input_string channel (in_channel_length channel)))
  with Sys_error _ -> None

let find_readable_source ~source_paths filename =
  source_candidates ~source_paths filename
  |> List.find_map (fun path ->
      if Sys.file_exists path then
        match read_file_contents path with
        | Some source -> Some source
        | None -> None
      else None)

let line_numbers_of_offsets source offsets =
  let offsets =
    offsets
    |> List.filter (fun offset -> offset >= 0)
    |> List.sort_uniq Int.compare
  in
  let length = String.length source in
  let rec advance pos line target =
    if pos >= target then (pos, line)
    else
      let line = if source.[pos] = '\n' then line + 1 else line in
      advance (pos + 1) line target
  in
  let rec loop pos line offsets acc =
    match offsets with
    | [] -> List.rev acc
    | offset :: rest ->
        let target = if offset <= length then offset else length in
        let pos, line = advance pos line target in
        loop pos line rest (line :: acc)
  in
  loop 0 1 offsets [] |> List.sort_uniq Int.compare

let file_report ~source_paths file =
  let summary = file_summary file in
  let uncovered_offsets = uncovered_offsets file in
  match find_readable_source ~source_paths file.filename with
  | None ->
      {
        filename = file.filename;
        summary;
        uncovered_offsets;
        uncovered_lines = [];
        source_available = false;
      }
  | Some source ->
      {
        filename = file.filename;
        summary;
        uncovered_offsets;
        uncovered_lines = line_numbers_of_offsets source uncovered_offsets;
        source_available = true;
      }

let reports ?(source_paths = []) cov =
  let source_paths = normalize_source_paths source_paths in
  Hashtbl.fold (fun _ file acc -> file_report ~source_paths file :: acc) cov []
  |> List.sort (fun a b -> String.compare a.filename b.filename)

let collapse_ranges lines =
  let rec loop acc range_start range_end = function
    | [] -> List.rev ((range_start, range_end) :: acc)
    | line :: rest ->
        if line = range_end + 1 then loop acc range_start line rest
        else loop ((range_start, range_end) :: acc) line line rest
  in
  match lines with [] -> [] | first :: rest -> loop [] first first rest

let format_ranges ranges =
  ranges
  |> List.map (fun (s, e) ->
      if s = e then string_of_int s else Printf.sprintf "%d-%d" s e)
  |> String.concat ", "

(* ───── Terminal Coloring ───── *)

let is_tty = lazy (Unix.isatty Unix.stdout)

let ansi_of_coverage_style = function
  | `Green -> "\027[32m"
  | `Yellow -> "\027[33m"
  | `Red -> "\027[31m"

let colorize s style =
  if Lazy.force is_tty then ansi_of_coverage_style style ^ s ^ "\027[0m" else s

let bold s = if Lazy.force is_tty then "\027[1m" ^ s ^ "\027[0m" else s
let dim s = if Lazy.force is_tty then "\027[2m" ^ s ^ "\027[0m" else s

(* ───── Summary Printing ───── *)

let print_summary ~per_file ~skip_covered ?(source_paths = []) cov =
  let fmt_pct s =
    let pct = percentage s in
    Printf.sprintf "%6.2f%%" pct
  in
  let overall = summarize cov in
  if per_file then begin
    let file_reports =
      let source_paths = normalize_source_paths source_paths in
      reports ~source_paths cov
    in
    let file_reports =
      if skip_covered then
        List.filter (fun r -> r.summary.visited < r.summary.total) file_reports
      else file_reports
    in
    let digits i = String.length (string_of_int i) in
    let vw =
      List.fold_left
        (fun m r -> max m (digits r.summary.visited))
        (digits overall.visited) file_reports
    in
    let tw =
      List.fold_left
        (fun m r -> max m (digits r.summary.total))
        (digits overall.total) file_reports
    in
    let pct_width = 7 in
    let indent = String.make (pct_width + 3 + vw + 3 + tw + 3) ' ' in
    List.iter
      (fun r ->
        let pct_str = colorize (fmt_pct r.summary) (coverage_style r.summary) in
        Printf.printf "%s   %*d / %-*d   %s\n" pct_str vw r.summary.visited tw
          r.summary.total r.filename;
        if r.uncovered_lines <> [] then begin
          let ranges = collapse_ranges r.uncovered_lines in
          Printf.printf "%s%s\n" indent
            (dim (Printf.sprintf "Lines: %s" (format_ranges ranges)))
        end)
      file_reports;
    let rule_width = pct_width + 3 + vw + 3 + tw + 3 + 5 in
    Printf.printf "%s\n" (String.make rule_width '-');
    let pct_str = colorize (fmt_pct overall) (coverage_style overall) in
    Printf.printf "%s   %d / %d   Total\n%!" pct_str overall.visited
      overall.total
  end
  else begin
    let pct_str =
      colorize
        (Printf.sprintf "%.2f%%" (percentage overall))
        (coverage_style overall)
    in
    Printf.printf "Coverage: %d/%d (%s)\n%!" overall.visited overall.total
      pct_str
  end

(* ───── Source Snippet Printing ───── *)

let print_uncovered ?(context = 1) ?(source_paths = []) cov =
  let source_paths = normalize_source_paths source_paths in
  let file_reports = reports ~source_paths cov in
  let file_reports =
    List.filter
      (fun r -> r.uncovered_lines <> [] && r.source_available)
      file_reports
  in
  let is_first = ref true in
  List.iter
    (fun r ->
      match find_readable_source ~source_paths r.filename with
      | None -> ()
      | Some source ->
          let lines = String.split_on_char '\n' source in
          let lines = Array.of_list lines in
          let total_lines = Array.length lines in
          let uncovered_set =
            let tbl = Hashtbl.create (List.length r.uncovered_lines) in
            List.iter (fun l -> Hashtbl.replace tbl l ()) r.uncovered_lines;
            tbl
          in
          let ranges = collapse_ranges r.uncovered_lines in
          if not !is_first then print_newline ();
          is_first := false;
          let pct = percentage r.summary in
          Printf.printf "%s (%s, %d/%d)\n" (bold r.filename)
            (colorize (Printf.sprintf "%.2f%%" pct) (coverage_style r.summary))
            r.summary.visited r.summary.total;
          let line_num_width =
            List.fold_left
              (fun m (_, e) ->
                max m
                  (String.length
                     (string_of_int (min total_lines (e + context)))))
              1 ranges
          in
          let is_first_region = ref true in
          List.iter
            (fun (range_start, range_end) ->
              let ctx_start = max 1 (range_start - context) in
              let ctx_end = min total_lines (range_end + context) in
              if not !is_first_region then
                Printf.printf "%s\n"
                  (dim (String.make (line_num_width + 4) '.'));
              is_first_region := false;
              for line = ctx_start to ctx_end do
                let text =
                  if line <= total_lines then lines.(line - 1) else ""
                in
                if Hashtbl.mem uncovered_set line then
                  Printf.printf "%s %*d | %s\n" (colorize ">" `Red)
                    line_num_width line (colorize text `Red)
                else Printf.printf "  %*d | %s\n" line_num_width line (dim text)
              done)
            ranges)
    file_reports;
  let overall = summarize cov in
  if file_reports <> [] then print_newline ();
  let pct_str =
    colorize
      (Printf.sprintf "%.2f%%" (percentage overall))
      (coverage_style overall)
  in
  Printf.printf "Coverage: %d/%d (%s)\n%!" overall.visited overall.total pct_str

(* ───── Registration ───── *)

let register_file ~filename ~points =
  Lazy.force register_dump;
  let coverage = data () in
  let counts =
    match Hashtbl.find_opt coverage filename with
    | Some existing -> existing.counts
    | None ->
        let counts = Array.make (Array.length points) 0 in
        Hashtbl.add coverage filename { filename; points; counts };
        counts
  in
  `Visit
    (fun index ->
      let current_count = counts.(index) in
      if current_count < max_int then counts.(index) <- current_count + 1)
