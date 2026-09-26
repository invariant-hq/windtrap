(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Verdicts *)

type reaching_test = string list

type verdict =
  | Killed
  | Survived of { first : reaching_test; others : reaching_test list }
  | Not_evaluated
  | Outside_tests
  | Unreached

(* A collection builds each survivor it holds here, so its reaching tests
   are sorted and without duplicates. *)
let survived ts =
  match List.sort_uniq (List.compare String.compare) ts with
  | [] ->
      invalid_arg
        "Windtrap_runtime.Verdicts.survived: a survivor names at least one test"
  | first :: others -> Survived { first; others }

(* The order in which verdicts outrank one another in a merge. *)
let rank = function
  | Killed -> 4
  | Not_evaluated -> 3
  | Survived _ -> 2
  | Outside_tests -> 1
  | Unreached -> 0

(* The verdict of a mutant that one executable saw as [a] and another as
   [b]. Both are held by a collection, so a survivor is already sorted. *)
let merge_verdict a b =
  match (a, b) with
  | Survived x, Survived y ->
      survived (x.first :: y.first :: (x.others @ y.others))
  | ( (Killed | Not_evaluated | Survived _ | Outside_tests | Unreached),
      (Killed | Not_evaluated | Survived _ | Outside_tests | Unreached) ) ->
      if rank a >= rank b then a else b

(* Collections *)

type record = {
  id : Mutate.id;
  before : string;
  after : string;
  verdict : verdict;
}

let record_of_mutant (m : Mutate.mutant) verdict =
  { id = m.id; before = m.before; after = m.after; verdict }

module Id_map = Map.Make (struct
  type t = Mutate.id

  let compare = Mutate.compare_id
end)

(* Each record is bound under its own [id]. *)
type t = record Id_map.t

let empty = Id_map.empty

(* Two records of one mutant disagree on its rendering only across builds of
   one source, and nothing says which build the reader has open. The smaller
   pair wins, which keeps [add] and [merge] commutative and associative. *)
let combine a b =
  let kept =
    if compare (a.before, a.after) (b.before, b.after) <= 0 then a else b
  in
  { kept with verdict = merge_verdict a.verdict b.verdict }

let add t r =
  let r =
    match r.verdict with
    | Survived s -> { r with verdict = survived (s.first :: s.others) }
    | Killed | Not_evaluated | Outside_tests | Unreached -> r
  in
  Id_map.update r.id
    (function None -> Some r | Some prior -> Some (combine prior r))
    t

let records t = List.map snd (Id_map.bindings t)
let merge a b = Id_map.union (fun _ a b -> Some (combine a b)) a b

(* Verdict files *)

let format =
  {
    Instr.magic = "windtrap-mutants-v3";
    kind = "verdict";
    dir = "mutants";
    ext = "mutants";
    remedy = "delete the stale verdict files, then re-run the mutation tests";
    who = "Windtrap_runtime.Verdicts";
  }

type error = Instr.error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }

let pp_error ppf e = Instr.pp_error format ppf e

type identity = Instr.identity = { exe : string; digest : string }

(* The digest, not the modification time, tells that the executable on disk
   is not the writer: dune's shared cache restores an artifact with its
   original timestamps. *)
let writer_identity ~exe =
  Option.map
    (fun digest -> { exe = Instr.exe_identity ~exe; digest })
    (Instr.file_digest exe)

let output_file ~exe = Instr.output_file format ~exe

(* The records after the header. Every number is in decimal, and a name is
   [<byte length> <bytes>], so it may hold any byte:

     <record count>
     <file> <line> <col> <rewrite> <before> <after> <verdict>

   with one such line for each record, in [Mutate.compare_id] order.
   [<verdict>] is [unreached], [outside_tests], [killed],
   [not_evaluated], or [survived <n>] and then [n] reaching tests, each [<k>] and then its [k]
   names. [load] reads this grammar and nothing else, and refuses a rewrite
   outside [Mutate.rewrites], which no report could render. *)
let load path =
  let read_test c =
    List.init (Instr.read_count c "test path length") (fun _ ->
        Instr.read_name c "test name")
  in
  let read_verdict c =
    match Instr.read_word c "verdict" with
    | "unreached" -> Unreached
    | "killed" -> Killed
    | "not_evaluated" -> Not_evaluated
    | "outside_tests" -> Outside_tests
    | "survived" ->
        let n = Instr.read_count c "reaching test count" in
        if n = 0 then
          Instr.parse_fail
            "a survivor names no test (survived is not unreached)";
        survived (List.init n (fun _ -> read_test c))
    | word -> Instr.parse_fail "unknown verdict %S" word
  in
  let read_id c =
    let file = Instr.read_name c "file name" in
    if file = "" then Instr.parse_fail "empty file name";
    let line = Instr.read_nat c "line" in
    if line < 1 then Instr.parse_fail "line %d is not 1-based" line;
    let col = Instr.read_nat c "column" in
    let rewrite = Instr.read_name c "rewrite" in
    if not (List.mem rewrite Mutate.rewrites) then
      Instr.parse_fail "unknown rewrite %S" rewrite;
    { Mutate.file; line; col; rewrite }
  in
  let rec read_records c t n =
    if n = 0 then t
    else
      let id = read_id c in
      if Id_map.mem id t then
        Instr.parse_fail "duplicate record for %s" (Mutate.id_to_string id);
      let before = Instr.read_name c "before" in
      let after = Instr.read_name c "after" in
      let verdict = read_verdict c in
      read_records c (add t { id; before; after; verdict }) (n - 1)
  in
  match Result.bind (Instr.read_file path) (Instr.start format ~path) with
  | Error e -> Error e
  | Ok c -> (
      try
        let identity = Instr.read_identity c in
        let t = read_records c empty (Instr.read_count c "record count") in
        Instr.finish c;
        Ok (t, identity)
      with Instr.Parse_error reason -> Error (Corrupt { path; reason }))

let save ?identity path t =
  let add_name b s = Printf.bprintf b "%d %s" (String.length s) s in
  let add_test b names =
    Printf.bprintf b "%d" (List.length names);
    List.iter (Printf.bprintf b " %a" add_name) names
  in
  let add_verdict b = function
    | Unreached -> Buffer.add_string b "unreached"
    | Killed -> Buffer.add_string b "killed"
    | Not_evaluated -> Buffer.add_string b "not_evaluated"
    | Outside_tests -> Buffer.add_string b "outside_tests"
    | Survived s ->
        let tests = s.first :: s.others in
        Printf.bprintf b "survived %d" (List.length tests);
        List.iter (Printf.bprintf b " %a" add_test) tests
  in
  let b = Buffer.create 1024 in
  Instr.add_header format b identity;
  Printf.bprintf b "%d\n" (Id_map.cardinal t);
  Id_map.iter
    (fun (id : Mutate.id) r ->
      Printf.bprintf b "%a %d %d %a %a %a %a\n" add_name id.file id.line id.col
        add_name id.rewrite add_name r.before add_name r.after add_verdict
        r.verdict)
    t;
  Instr.write_file path (Buffer.contents b)
