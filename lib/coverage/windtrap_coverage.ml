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
    (fun _ file acc -> (file.filename, file_summary file) :: acc)
    cov []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let percentage { visited; total } =
  if total > 0 then float_of_int visited *. 100. /. float_of_int total else 100.

let coverage_style s =
  let pct = percentage s in
  if pct >= 80. then `Green else if pct >= 60. then `Yellow else `Red

(* ───── Terminal Coloring ───── *)

let is_tty = lazy (Unix.isatty Unix.stdout)

let ansi_of_coverage_style = function
  | `Green -> "\027[32m"
  | `Yellow -> "\027[33m"
  | `Red -> "\027[31m"

let colorize s style =
  if Lazy.force is_tty then ansi_of_coverage_style style ^ s ^ "\027[0m" else s

(* ───── Summary Printing ───── *)

let print_summary ~per_file ~skip_covered cov =
  let fmt_pct s =
    let pct = percentage s in
    Printf.sprintf "%6.2f%%" pct
  in
  let overall = summarize cov in
  if per_file then begin
    let stats = summarize_per_file cov in
    let stats_to_show =
      if skip_covered then List.filter (fun (_, s) -> s.visited < s.total) stats
      else stats
    in
    let digits i = String.length (string_of_int i) in
    let vw =
      List.fold_left
        (fun m (_, s) -> max m (digits s.visited))
        (digits overall.visited) stats_to_show
    in
    let tw =
      List.fold_left
        (fun m (_, s) -> max m (digits s.total))
        (digits overall.total) stats_to_show
    in
    List.iter
      (fun (name, s) ->
        let pct_str = colorize (fmt_pct s) (coverage_style s) in
        Printf.printf "%s   %*d / %-*d   %s\n" pct_str vw s.visited tw s.total
          name)
      stats_to_show;
    let rule_width = 7 + 3 + vw + 3 + tw + 3 + 5 in
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
