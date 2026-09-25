(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type point = { start_ofs : int; end_ofs : int }

let format =
  {
    Instr.magic = "windtrap-coverage-v3";
    kind = "coverage";
    dir = "coverage";
    ext = "coverage";
    remedy =
      "delete the stale coverage files, then re-run the instrumented tests";
    who = "Windtrap_runtime.Coverage";
  }

(* The runtime links no core, so its messages skip the report's escape of
   control bytes; what they print is build paths. *)
let warn fmt =
  Printf.ksprintf (fun m -> Printf.eprintf "windtrap: warning: %s\n%!" m) fmt

(* Collections *)

type error = Data of Instr.error | Point_mismatch of { file : string }

(* A full instrumented re-run heals a mismatch, so its hint deletes files
   only as a fallback; a file of another format survives every re-run, and
   the hint of [Instr.pp_error] deletes it. *)
let pp_error ppf = function
  | Data e -> Instr.pp_error format ppf e
  | Point_mismatch { file } ->
      Format.fprintf ppf
        "%s: coverage point tables disagree across coverage files (executables \
         built from different sources?); re-run every instrumented test \
         executable from one build, then merge again; delete the coverage \
         files only if leftovers remain"
        file

module File_map = Map.Make (String)

(* [counts.(i)] counts the visits of [points.(i)]. *)
type entry = { points : point array; counts : int array }
type t = entry File_map.t

let empty = File_map.empty

let points_equal a b =
  Array.length a = Array.length b
  && Array.for_all2
       (fun p q -> p.start_ofs = q.start_ofs && p.end_ofs = q.end_ofs)
       a b

let saturating_add x y = if x > max_int - y then max_int else x + y

let add t ~file entry =
  match File_map.find_opt file t with
  | None -> Ok (File_map.add file entry t)
  | Some prior when not (points_equal prior.points entry.points) ->
      Error (Point_mismatch { file })
  | Some prior ->
      let counts = Array.map2 saturating_add prior.counts entry.counts in
      Ok (File_map.add file { prior with counts } t)

let merge a b =
  File_map.fold
    (fun file entry acc -> Result.bind acc (fun t -> add t ~file entry))
    b (Ok a)

let files t = List.map fst (File_map.bindings t)

(* Dumps *)

type identity = Instr.identity = { exe : string; digest : string }

(* The records after the header, one item to a line, every number in
   decimal:

     <file count>
     <byte length> <file>             for each file, in the order of names
     <point count>
     <start_ofs> <end_ofs> <count>    for each point, in table order

   A file name is length-prefixed, so it may hold any byte. [load] reads
   this grammar and nothing else. *)
let to_string ~identity t =
  let b = Buffer.create 1024 in
  Instr.add_header format b identity;
  Printf.bprintf b "%d\n" (File_map.cardinal t);
  File_map.iter
    (fun file { points; counts } ->
      Printf.bprintf b "%d %s\n%d\n" (String.length file) file
        (Array.length points);
      Array.iteri
        (fun i p ->
          Printf.bprintf b "%d %d %d\n" p.start_ofs p.end_ofs counts.(i))
        points)
    t;
  Buffer.contents b

let load path =
  let read_entry c file =
    let read_point _ =
      let start_ofs = Instr.read_nat c "extent start" in
      let end_ofs = Instr.read_nat c "extent end" in
      if end_ofs < start_ofs then
        Instr.parse_fail "inverted extent %d-%d in %s" start_ofs end_ofs file;
      ({ start_ofs; end_ofs }, Instr.read_nat c "count")
    in
    let rows = Array.init (Instr.read_count c "point count") read_point in
    let points, counts = Array.split rows in
    { points; counts }
  in
  let rec read_files c t n =
    if n = 0 then begin
      Instr.finish c;
      Ok t
    end
    else
      let file = Instr.read_name c "file name" in
      Result.bind
        (add t ~file (read_entry c file))
        (fun t -> read_files c t (n - 1))
  in
  match Result.bind (Instr.read_file path) (Instr.start format ~path) with
  | Error e -> Error (Data e)
  | Ok c -> (
      try
        let identity = Instr.read_identity c in
        let file_count = Instr.read_count c "file count" in
        Result.map (fun t -> (t, identity)) (read_files c empty file_count)
      with Instr.Parse_error reason ->
        Error (Data (Instr.Corrupt { path; reason })))

(* Instrumentation *)

(* Each registered file's point table, and the live counts arrays of the
   registrations that share it. *)
let registry : (point array * int array list) File_map.t ref =
  ref File_map.empty

let snapshot () =
  File_map.map
    (fun (points, live) ->
      let zero = Array.make (Array.length points) 0 in
      { points; counts = List.fold_left (Array.map2 saturating_add) zero live })
    !registry

(* Where the dump lands: the one file WINDTRAP_COVERAGE_FILE names,
   replaced on every run, or a fresh file in this executable's own
   directory, where every run keeps its own. *)
type target = File of string | Dir of string

(* The directory belongs to this executable, and a dump in it named after
   another digest was written by a build this one replaced, which the
   reporting command would exclude with a warning on every merge. A [.tmp]
   file is not a dump. *)
let remove_predecessors dir ~prefix =
  match Sys.readdir dir with
  | exception Sys_error _ -> ()
  | names ->
      Array.iter
        (fun name ->
          if
            Filename.check_suffix name ("." ^ format.Instr.ext)
            && not (String.starts_with ~prefix name)
          then
            try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
        names

(* The identity digests the executable's bytes, since dune's shared cache
   restores artifacts with their original mtimes; an executable that cannot
   be read back leaves the dump without one. A dump in the directory is
   named after that digest, so the directory tells builds apart unread. *)
let dump target ~exe () =
  let t = snapshot () in
  let identity =
    Option.bind exe (fun exe ->
        Option.map
          (fun digest -> { exe; digest })
          (Instr.file_digest Sys.executable_name))
  in
  let data = to_string ~identity t in
  match target with
  | File path -> (
      try Instr.write_file path data
      with e ->
        warn "cannot write coverage file %s: %s" path (Printexc.to_string e))
  | Dir dir -> (
      let prefix =
        match identity with
        | None -> ""
        | Some { digest; _ } ->
            let prefix = digest ^ "-" in
            remove_predecessors dir ~prefix;
            prefix
      in
      try ignore (Instr.write_new_file dir ~prefix ~ext:format.Instr.ext data)
      with e ->
        warn "cannot write a coverage file under %s: %s" dir
          (Printexc.to_string e))

(* A relative executable path under an unreadable current directory
   leaves the target known and the identity not, and the dump is then
   written without one. *)
let install_dump () =
  let determined f =
    match f () with
    | v -> Some v
    | exception e ->
        warn "cannot determine the coverage output file: %s"
          (Printexc.to_string e);
        None
  in
  let target () =
    match Sys.getenv_opt "WINDTRAP_COVERAGE_FILE" with
    | Some path when path <> "" -> File (Instr.absolute path)
    | Some _ | None -> Dir (Instr.output_dir format ~exe:Sys.executable_name)
  in
  match determined target with
  | None -> ()
  | Some target ->
      let exe =
        determined (fun () -> Instr.exe_identity ~exe:Sys.executable_name)
      in
      at_exit (dump target ~exe)

let validate ~file points counts =
  let err fmt =
    Printf.ksprintf invalid_arg ("Windtrap_runtime.Coverage: %s: " ^^ fmt) file
  in
  if Array.length points <> Array.length counts then
    err "%d points but %d counts" (Array.length points) (Array.length counts);
  Array.iter
    (fun p ->
      if p.start_ofs < 0 || p.end_ofs < p.start_ofs then
        err "invalid extent %d-%d" p.start_ofs p.end_ofs)
    points;
  if Array.exists (fun c -> c < 0) counts then err "negative count"

let register ~file ~points ~counts =
  validate ~file points counts;
  match File_map.find_opt file !registry with
  | Some (table, _) when not (points_equal table points) ->
      (* Registration runs at module load in the user's program, whose
         meaning coverage never changes, so it warns instead of raising. *)
      warn
        "%s: conflicting instrumentation tables in one executable (stale build \
         artifacts? rebuild from clean); ignoring one module's coverage data"
        file
  | registered ->
      if File_map.is_empty !registry then install_dump ();
      let table, live = Option.value registered ~default:(points, []) in
      registry := File_map.add file (table, counts :: live) !registry

let visit counts index =
  let count = counts.(index) in
  if count < max_int then counts.(index) <- count + 1

(* Summaries *)

type summary = { visited : int; total : int }

let file_summary entry =
  let visited =
    Array.fold_left (fun n c -> if c > 0 then n + 1 else n) 0 entry.counts
  in
  { visited; total = Array.length entry.counts }

let summary t =
  File_map.fold
    (fun _ entry acc ->
      let s = file_summary entry in
      { visited = acc.visited + s.visited; total = acc.total + s.total })
    t { visited = 0; total = 0 }

(* Reports *)

(* The byte offset at which each line starts. A final newline ends the
   last line and opens none. *)
let line_starts source =
  let starts = ref [ 0 ] in
  for i = 0 to String.length source - 2 do
    if source.[i] = '\n' then starts := (i + 1) :: !starts
  done;
  Array.of_list (List.rev !starts)

(* The index of the line holding byte [ofs], which is the last line for an
   offset past the end. *)
let line_index starts ofs =
  let rec search lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi + 1) / 2 in
      if starts.(mid) <= ofs then search mid hi else search lo (mid - 1)
  in
  search 0 (Array.length starts - 1)

(* A line's hits are the fewest visits of any point touching it, so an
   unvisited arm marks its line even beside visited code. An empty extent
   touches the line of its start, and an empty source has no line. *)
let line_hits ~source entry =
  if source = "" then []
  else
    let starts = line_starts source in
    let hits = Array.make (Array.length starts) None in
    Array.iteri
      (fun i p ->
        let count = entry.counts.(i) in
        let first = line_index starts p.start_ofs in
        let last = line_index starts (max p.start_ofs (p.end_ofs - 1)) in
        for line = first to last do
          match hits.(line) with
          | Some h when h <= count -> ()
          | Some _ | None -> hits.(line) <- Some count
        done)
      entry.points;
    Array.to_list hits
    |> List.mapi (fun line h -> Option.map (fun h -> (line + 1, h)) h)
    |> List.filter_map Fun.id

let uncovered_extents entry =
  List.filteri (fun i _ -> entry.counts.(i) = 0) (Array.to_list entry.points)

(* A directory can open as a file, with a length it cannot be read to. *)
let find_source ~roots file =
  file :: List.map (fun root -> Filename.concat root file) roots
  |> List.find_map (fun path ->
      match Sys.is_directory path with
      | false -> Result.to_option (Instr.read_file path)
      | true | (exception Sys_error _) -> None)

(* A source shorter than an extent changed since the run, and its lines
   would paint the wrong code. An edit that keeps the file long enough goes
   unseen. *)
let is_stale entry source =
  Array.exists (fun p -> p.end_ofs > String.length source) entry.points

type file_report = {
  file : string;
  summary : summary;
  uncovered_extents : point list;
  uncovered_lines : int list;
  line_hits : (int * int) list;
  source : string option;
  stale : bool;
}

let file_reports ?(source_roots = [ Filename.current_dir_name ]) t =
  let report (file, entry) =
    let source, stale =
      match find_source ~roots:source_roots file with
      | Some source when is_stale entry source -> (None, true)
      | source -> (source, false)
    in
    let line_hits =
      match source with None -> [] | Some source -> line_hits ~source entry
    in
    {
      file;
      summary = file_summary entry;
      uncovered_extents = uncovered_extents entry;
      uncovered_lines =
        List.filter_map
          (fun (line, hits) -> if hits = 0 then Some line else None)
          line_hits;
      line_hits;
      source;
      stale;
    }
  in
  List.map report (File_map.bindings t)
