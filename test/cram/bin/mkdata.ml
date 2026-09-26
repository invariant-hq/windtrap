(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Writes the dumps and verdict files that the sessions merge, so that every
   count a report prints is stated in the session that plants it:

   mkdata coverage OUT [--exe EXE] FILE=COUNTS...
     One point per count of the comma-separated COUNTS, point i spanning
     line i + 1 of a source of 10-byte lines. FILE is read with OCaml's
     string escapes, so a session can name any byte.

   mkdata mutants OUT [--exe EXE] FILE:LINE:COL:REWRITE:BEFORE:AFTER=VERDICT...
     VERDICT is killed, not_evaluated, outside_tests, unreached or
     survived:TESTS, where TESTS are ';'-separated paths of '/'-separated
     names.

   --exe records the identity of the executable at EXE as it is now. *)

module Coverage = Windtrap_runtime.Coverage
module Instr = Windtrap_runtime.Instr
module Verdicts = Windtrap_runtime.Verdicts

let die fmt =
  Printf.ksprintf
    (fun s ->
      prerr_endline ("mkdata: " ^ s);
      exit 2)
    fmt

let split_last c s =
  match String.rindex_opt s c with
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> die "%S: no '%c'" s c

let identity exe =
  match Instr.file_digest exe with
  | Some digest -> { Instr.exe = Instr.exe_identity ~exe; digest }
  | None -> die "%s: cannot be read" exe

(* The records are in the grammar [Coverage.load] reads: the runtime itself
   writes them only in the dump of an exiting process. *)
let coverage ~identity out records =
  let b = Buffer.create 256 in
  Instr.add_header Coverage.format b identity;
  Printf.bprintf b "%d\n" (List.length records);
  List.iter
    (fun record ->
      let file, counts = split_last '=' record in
      let file = Scanf.unescaped file in
      let counts =
        if counts = "" then []
        else List.map int_of_string (String.split_on_char ',' counts)
      in
      Printf.bprintf b "%d %s\n%d\n" (String.length file) file
        (List.length counts);
      List.iteri
        (fun i count ->
          Printf.bprintf b "%d %d %d\n" (i * 10) ((i * 10) + 9) count)
        counts)
    records;
  Instr.write_file out (Buffer.contents b)

let verdict = function
  | "killed" -> Verdicts.Killed
  | "not_evaluated" -> Verdicts.Not_evaluated
  | "outside_tests" -> Verdicts.Outside_tests
  | "unreached" -> Verdicts.Unreached
  | v -> (
      match String.split_on_char ':' v with
      | [ "survived"; tests ] ->
          Verdicts.survived
            (List.map (String.split_on_char '/')
               (String.split_on_char ';' tests))
      | _ -> die "%S: not a verdict" v)

let record spec =
  let mutant, v = split_last '=' spec in
  match String.split_on_char ':' mutant with
  | [ file; line; col; rewrite; before; after ] ->
      let id =
        {
          Windtrap_runtime.Mutate.file;
          line = int_of_string line;
          col = int_of_string col;
          rewrite;
        }
      in
      { Verdicts.id; before; after; verdict = verdict v }
  | _ -> die "%S: not FILE:LINE:COL:REWRITE:BEFORE:AFTER" mutant

let mutants ~identity out specs =
  Verdicts.save ?identity out
    (List.fold_left Verdicts.add Verdicts.empty (List.map record specs))

let () =
  match List.tl (Array.to_list Sys.argv) with
  | kind :: out :: args -> (
      let identity, args =
        match args with
        | "--exe" :: exe :: args -> (Some (identity exe), args)
        | args -> (None, args)
      in
      match kind with
      | "coverage" -> coverage ~identity out args
      | "mutants" -> mutants ~identity out args
      | kind -> die "%S: not coverage or mutants" kind)
  | _ -> die "usage: mkdata (coverage | mutants) OUT [--exe EXE] RECORD..."
