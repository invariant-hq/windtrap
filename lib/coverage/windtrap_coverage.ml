(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_instr

(* Points *)

type point = { start_ofs : int; end_ofs : int }

let magic = "windtrap-coverage-v3"

(* The constants Windtrap_instr's shared plumbing is parameterized by:
   this format's magic line, its on-disk home, and the words its error
   messages use. *)
let format =
  {
    Instr.magic;
    kind = "coverage";
    dir = "_coverage";
    ext = "coverage";
    remedy =
      "delete the stale files under _build/_coverage (or run dune clean), \
       then re-run the instrumented tests";
    who = "Windtrap_coverage";
  }

let points_equal a b =
  Array.length a = Array.length b
  && Array.for_all2
       (fun p q -> p.start_ofs = q.start_ofs && p.end_ofs = q.end_ofs)
       a b

let validate ~file points counts =
  if Array.length points <> Array.length counts then
    invalid_arg
      (Printf.sprintf "Windtrap_coverage: %s: %d points but %d counts" file
         (Array.length points) (Array.length counts));
  Array.iter
    (fun p ->
      if p.start_ofs < 0 || p.end_ofs < p.start_ofs then
        invalid_arg
          (Printf.sprintf "Windtrap_coverage: %s: invalid extent %d-%d" file
             p.start_ofs p.end_ofs))
    points;
  Array.iter
    (fun c ->
      if c < 0 then
        invalid_arg
          (Printf.sprintf "Windtrap_coverage: %s: negative count" file))
    counts

(* Collections *)

type error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }
  | Point_mismatch of { file : string }

(* The first three cases are the shared plumbing's; Point_mismatch is
   coverage's own - merging produces it and parsing reports it - which is
   why the public type cannot simply re-export Windtrap_instr.error. The
   hint asymmetry is deliberate: re-running never removes a
   foreign-*named* file, so Unknown_format instructs deletion;
   Point_mismatch self-heals under a full instrumented re-run, so dune
   clean is only the fallback for orphaned files. *)
let error_of_instr = function
  | Instr.Unknown_format { path; header } -> Unknown_format { path; header }
  | Instr.Unreadable { path; reason } -> Unreadable { path; reason }
  | Instr.Corrupt { path; reason } -> Corrupt { path; reason }

let pp_error ppf = function
  | Unknown_format { path; header } ->
      Instr.pp_error format ppf (Instr.Unknown_format { path; header })
  | Unreadable { path; reason } ->
      Instr.pp_error format ppf (Instr.Unreadable { path; reason })
  | Corrupt { path; reason } ->
      Instr.pp_error format ppf (Instr.Corrupt { path; reason })
  | Point_mismatch { file } ->
      Format.fprintf ppf
        "%s: coverage point tables disagree across coverage files (executables \
         built from different sources?); re-run all the instrumented tests \
         together (dune build @cover --instrument-with ppx_windtrap); dune \
         clean only if orphaned files remain"
        file

module File_map = Map.Make (String)

type entry = { points : point array; counts : int array }
type t = entry File_map.t

let empty = File_map.empty
let is_empty = File_map.is_empty
let saturating_add x y = if x > max_int - y then max_int else x + y

let add t ~file ~points ~counts =
  validate ~file points counts;
  match File_map.find_opt file t with
  | None ->
      Ok
        (File_map.add file
           { points = Array.copy points; counts = Array.copy counts }
           t)
  | Some entry ->
      if not (points_equal entry.points points) then
        Error (Point_mismatch { file })
      else
        let counts = Array.map2 saturating_add entry.counts counts in
        Ok (File_map.add file { entry with counts } t)

let merge a b =
  File_map.fold
    (fun file entry acc ->
      Result.bind acc (fun t ->
          add t ~file ~points:entry.points ~counts:entry.counts))
    b (Ok a)

let filter keep t = File_map.filter (fun file _ -> keep file) t

(* In-Process Registry *)

(* Registrations keep the generated code's live counts arrays; [snapshot]
   copies. [register] guarantees same-file registrations carry equal point
   tables, which is what makes the [add] in [snapshot] infallible. *)

let registrations : (string * point array * int array) list ref = ref []

let snapshot () =
  List.fold_left
    (fun t (file, points, counts) ->
      match add t ~file ~points ~counts with
      | Ok t -> t
      | Error _ -> assert false (* register enforced table agreement *))
    empty !registrations

let visit counts index =
  let count = counts.(index) in
  if count < max_int then counts.(index) <- count + 1

(* Serialization *)

type identity = Instr.identity = { exe : string; digest : string }

let to_string ?identity t =
  let buffer = Buffer.create 1024 in
  Instr.add_header format buffer identity;
  Printf.bprintf buffer "%d\n" (File_map.cardinal t);
  File_map.iter
    (fun file { points; counts } ->
      Printf.bprintf buffer "%d %s\n" (String.length file) file;
      Printf.bprintf buffer "%d\n" (Array.length points);
      Array.iteri
        (fun i p ->
          Printf.bprintf buffer "%d %d %d\n" p.start_ofs p.end_ofs counts.(i))
        points)
    t;
  Buffer.contents buffer

exception Conflicting_entry of error

let of_string ?(path = "<string>") s =
  match Instr.start format ~path s with
  | Error e -> Error (error_of_instr e)
  | Ok c -> (
      try
        let identity = Instr.read_identity c in
        let file_count = Instr.read_count c "file count" in
        let result = ref empty in
        for _ = 1 to file_count do
          let file = Instr.read_name c "file name" in
          let point_count = Instr.read_count c "point count" in
          let points = Array.make point_count { start_ofs = 0; end_ofs = 0 } in
          let counts = Array.make point_count 0 in
          for i = 0 to point_count - 1 do
            let start_ofs = Instr.read_nat c "extent start" in
            let end_ofs = Instr.read_nat c "extent end" in
            if end_ofs < start_ofs then
              Instr.parse_fail "inverted extent %d-%d in %s" start_ofs end_ofs
                file;
            let count = Instr.read_nat c "count" in
            points.(i) <- { start_ofs; end_ofs };
            counts.(i) <- count
          done;
          match add !result ~file ~points ~counts with
          | Ok t -> result := t
          | Error e -> raise (Conflicting_entry e)
        done;
        Instr.finish c;
        Ok (!result, identity)
      with
      | Instr.Parse_error reason -> Error (Corrupt { path; reason })
      | Conflicting_entry e -> Error e)

let load path =
  match Instr.read_file path with
  | Ok contents -> of_string ~path contents
  | Error e -> Error (error_of_instr e)

(* Output Path and Identity *)

let build_root = Instr.build_root
let exe_identity = Instr.exe_identity
let output_file ~exe = Instr.output_file format ~exe

(* At-Exit Dump *)

let dump_path : string option ref = ref None
let dump_exe : string option ref = ref None
let dumped = ref false
let dump_destination () = !dump_path

let warn fmt =
  Printf.ksprintf (fun m -> Printf.eprintf "windtrap coverage: %s\n%!" m) fmt

(* The identity digests the running executable's bytes (a few
   milliseconds for a typical test binary, off the test path at exit):
   the reporting command re-digests the file at the recorded path, and
   any difference means the executable on disk is not the one that wrote
   the dump — mtimes cannot say that (dune's shared cache restores
   artifacts with their original timestamps). Best-effort: no identity
   is recorded when the executable cannot be read back. *)
let dump_identity () =
  match !dump_exe with
  | None -> None
  | Some exe ->
      Option.map
        (fun digest -> { exe; digest })
        (Instr.file_digest Sys.executable_name)

let dump () =
  if not !dumped then begin
    dumped := true;
    match !dump_path with
    | None -> ()
    | Some path -> (
        let t = snapshot () in
        if not (is_empty t) then
          try Instr.write_file path (to_string ?identity:(dump_identity ()) t)
          with e -> warn "cannot write %s: %s" path (Printexc.to_string e))
  end

let resolve_dump_path () =
  match Sys.getenv_opt "WINDTRAP_COVERAGE_FILE" with
  | Some path when path <> "" -> Instr.absolute path
  | _ -> output_file ~exe:Sys.executable_name

let register ~file ~points ~counts =
  validate ~file points counts;
  match List.find_opt (fun (f, _, _) -> String.equal f file) !registrations with
  | Some (_, prior, _) when not (points_equal prior points) ->
      (* Two incompatible instrumentations of one source file are linked into
         this executable — stale build artifacts, most likely. Registration
         runs at module load inside the user's program, so it must not raise
         (coverage never changes what programs mean): warn loudly and drop
         this registration, keeping the snapshot invariant that same-file
         registrations carry equal tables. *)
      warn
        "%s: conflicting instrumentation tables in one executable (stale build \
         artifacts? try dune clean); ignoring one module's data"
        file
  | _ ->
      (match !registrations with
      | [] ->
          (try
             dump_path := Some (resolve_dump_path ());
             dump_exe := Some (exe_identity ~exe:Sys.executable_name)
           with e ->
             warn "cannot determine output file: %s" (Printexc.to_string e));
          at_exit dump
      | _ :: _ -> ());
      registrations := (file, points, counts) :: !registrations

(* Summaries *)

type summary = { visited : int; total : int }

let file_summary entry =
  {
    total = Array.length entry.counts;
    visited =
      Array.fold_left (fun n c -> if c > 0 then n + 1 else n) 0 entry.counts;
  }

let summary t =
  File_map.fold
    (fun _ entry acc ->
      let s = file_summary entry in
      { visited = acc.visited + s.visited; total = acc.total + s.total })
    t { visited = 0; total = 0 }

let percentage { visited; total } =
  if total = 0 then 100. else 100. *. float_of_int visited /. float_of_int total

let style s =
  let pct = percentage s in
  if pct >= 80. then `Green else if pct >= 60. then `Yellow else `Red

let pp_summary ppf s =
  Format.fprintf ppf "%.1f%% (%d/%d points)" (percentage s) s.visited s.total

(* Extent -> Line Mapping *)

(* Byte offsets at which each line starts, excluding the phantom line a
   trailing newline would open. Empty source has no lines. *)
let line_starts source =
  let n = String.length source in
  if n = 0 then [||]
  else begin
    let starts = ref [ 0 ] in
    for i = 0 to n - 2 do
      if source.[i] = '\n' then starts := (i + 1) :: !starts
    done;
    Array.of_list (List.rev !starts)
  end

(* 1-based line containing byte [ofs]; offsets past the end clamp to the
   last line. [starts] is non-empty. *)
let line_of starts ofs =
  let rec search lo hi =
    if lo >= hi then lo + 1
    else
      let mid = (lo + hi + 1) / 2 in
      if starts.(mid) <= ofs then search mid hi else search lo (mid - 1)
  in
  search 0 (Array.length starts - 1)

let lines_of_extents ~source extents =
  let starts = line_starts source in
  if Array.length starts = 0 then []
  else
    List.concat_map
      (fun p ->
        let first = line_of starts p.start_ofs in
        let last = line_of starts (max p.start_ofs (p.end_ofs - 1)) in
        List.init (last - first + 1) (fun i -> first + i))
      extents
    |> List.sort_uniq Int.compare

let collapse_ranges lines =
  let rec loop acc range_start range_end = function
    | [] -> List.rev ((range_start, range_end) :: acc)
    | line :: rest ->
        if line <= range_end + 1 then
          loop acc range_start (max range_end line) rest
        else loop ((range_start, range_end) :: acc) line line rest
  in
  match lines with [] -> [] | first :: rest -> loop [] first first rest

let format_ranges ranges =
  ranges
  |> List.map (fun (s, e) ->
      if s = e then string_of_int s else Printf.sprintf "%d-%d" s e)
  |> String.concat ", "

(* Per-File Reports *)

type file_report = {
  file : string;
  summary : summary;
  uncovered_extents : point list;
  uncovered_lines : int list;
  source : string option;
  stale : bool;
}

let uncovered_extents entry =
  let acc = ref [] in
  for i = Array.length entry.points - 1 downto 0 do
    if entry.counts.(i) = 0 then acc := entry.points.(i) :: !acc
  done;
  !acc

let read_source path =
  match
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  with
  | contents -> Some contents
  | exception (Sys_error _ | End_of_file) -> None

let find_source ~roots file =
  file :: List.map (fun root -> Filename.concat root file) roots
  |> List.find_map (fun path ->
      match Sys.is_directory path with
      | true -> None
      | false -> read_source path
      | exception Sys_error _ -> None)

(* The data cannot describe this source: some extent ends past its last
   byte (a consistent extent's [end_ofs] is at most the length), so the
   source changed since the run. Mapping stale extents to lines would
   paint the wrong code; the report says so instead. Edits that keep the
   file at least as long as the extents are undetectable — best effort. *)
let stale_source entry source =
  let len = String.length source in
  Array.exists (fun p -> p.end_ofs > len) entry.points

let file_reports ?(source_roots = [ Filename.current_dir_name ]) t =
  File_map.fold
    (fun file entry acc ->
      let uncovered_extents = uncovered_extents entry in
      let source, stale =
        match find_source ~roots:source_roots file with
        | None -> (None, false)
        | Some source when stale_source entry source -> (None, true)
        | Some source -> (Some source, false)
      in
      let uncovered_lines =
        match source with
        | None -> []
        | Some source -> lines_of_extents ~source uncovered_extents
      in
      {
        file;
        summary = file_summary entry;
        uncovered_extents;
        uncovered_lines;
        source;
        stale;
      }
      :: acc)
    t []
  |> List.rev

(* Excerpts *)

type excerpt_line = { number : int; text : string; uncovered : bool }

(* One entry per line, mirroring [line_starts]: an empty source has no
   lines, and a trailing newline opens no phantom line. *)
let source_lines source =
  if String.length source = 0 then [||]
  else
    let lines = String.split_on_char '\n' source in
    let lines =
      if source.[String.length source - 1] = '\n' then
        match List.rev lines with "" :: rest -> List.rev rest | _ -> lines
      else lines
    in
    Array.of_list lines

let excerpts ?(context = 1) ~source uncovered =
  let lines = source_lines source in
  let total = Array.length lines in
  let uncovered =
    List.sort_uniq Int.compare uncovered
    |> List.filter (fun l -> l >= 1 && l <= total)
  in
  let uncovered_set = Hashtbl.create 16 in
  List.iter (fun l -> Hashtbl.replace uncovered_set l ()) uncovered;
  let windows =
    collapse_ranges uncovered
    |> List.map (fun (s, e) -> (max 1 (s - context), min total (e + context)))
  in
  let rec merge_windows = function
    | (s1, e1) :: (s2, e2) :: rest when s2 <= e1 + 1 ->
        merge_windows ((s1, max e1 e2) :: rest)
    | window :: rest -> window :: merge_windows rest
    | [] -> []
  in
  merge_windows windows
  |> List.map (fun (s, e) ->
      List.init
        (e - s + 1)
        (fun i ->
          let number = s + i in
          {
            number;
            text = lines.(number - 1);
            uncovered = Hashtbl.mem uncovered_set number;
          }))
