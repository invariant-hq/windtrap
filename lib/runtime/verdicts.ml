(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module M = Mutate

(* The constants Instr's shared plumbing is parameterized by:
   this format's magic line, its on-disk home, and the words its error
   messages use. *)
let format =
  {
    Instr.magic = "windtrap-mutants-v3";
    kind = "verdict";
    dir = "mutants";
    ext = "mutants";
    remedy = "delete the stale verdict files, then re-run the mutation tests";
    who = "Windtrap_runtime.Verdicts";
  }

(* The rewrite vocabulary is the runtime's, and the parser checks against
   it: a rewrite name nobody can render is a report nobody can act on, so
   an unknown one is refused where it enters, never carried. *)
let is_rewrite r = List.exists (String.equal r) M.rewrites

(* Verdicts *)

type witness = string list

type verdict =
  | Killed
  | Survived of { witness : witness; others : witness list }
  | Unreached

let compare_witness = List.compare String.compare
let sorted_witnesses ws = List.sort_uniq compare_witness ws

(* A survivor names at least one test: a mutant no test reached is
   [Unreached] and is never forked, so the empty case is a caller error
   rather than a verdict. Every path into [Survived] goes through here, so
   the witnesses are sorted and duplicate-free by construction and the
   report's count is the number of tests that ran the line. *)
let survived ws =
  match sorted_witnesses ws with
  | [] ->
      invalid_arg
        "Windtrap_runtime.Verdicts.survived: a survivor names at least one test"
  | witness :: others -> Survived { witness; others }

let merge_verdict a b =
  match (a, b) with
  | Killed, Killed -> Killed
  | Killed, (Survived _ | Unreached) -> a
  | (Survived _ | Unreached), Killed -> b
  | Survived x, Survived y ->
      survived (x.witness :: y.witness :: (x.others @ y.others))
  | Survived s, Unreached | Unreached, Survived s ->
      survived (s.witness :: s.others)
  | Unreached, Unreached -> Unreached

(* Collections *)

(* The shared plumbing's error type, re-exported with its constructors: a
   verdict file fails in exactly the three ways both formats share. *)
type error = Instr.error =
  | Unknown_format of { path : string; header : string }
  | Unreadable of { path : string; reason : string }
  | Corrupt of { path : string; reason : string }

let pp_error ppf e = Instr.pp_error format ppf e

module Id_map = Map.Make (struct
  type t = M.id

  let compare = M.compare_id
end)

type record = { id : M.id; before : string; after : string; verdict : verdict }

let record_of_mutant (m : M.mutant) verdict =
  { id = m.M.id; before = m.M.before; after = m.M.after; verdict }

(* The rendering a record carries beside its verdict. Stored apart from
   the identifier because the identifier is the map's key: a value that
   repeated it could disagree with it. *)
type rendering = { r_before : string; r_after : string }

(* Two files describing one mutant are expected to agree here, and can
   disagree only across builds of one source - where the data says
   nothing about which build the reader is looking at. So the choice is
   made for determinism: a total order, smaller wins, which is what keeps
   [add] and [merge] commutative and associative. *)
let compare_rendering a b =
  let c = String.compare a.r_before b.r_before in
  if c <> 0 then c else String.compare a.r_after b.r_after

type t = (rendering * verdict) Id_map.t

let empty = Id_map.empty

let record_of id (r, verdict) =
  { id; before = r.r_before; after = r.r_after; verdict }

let add t r =
  let verdict =
    match r.verdict with
    | Survived s -> survived (s.witness :: s.others)
    | Killed | Unreached -> r.verdict
  in
  let rendering = { r_before = r.before; r_after = r.after } in
  Id_map.update r.id
    (function
      | None -> Some (rendering, verdict)
      | Some (prior, prior_verdict) ->
          Some
            ( (if compare_rendering prior rendering <= 0 then prior
               else rendering),
              merge_verdict prior_verdict verdict ))
    t

let records t = List.map (fun (id, v) -> record_of id v) (Id_map.bindings t)
let merge a b = Id_map.fold (fun id v acc -> add acc (record_of id v)) b a

(* Serialization *)

type identity = Instr.identity = { exe : string; digest : string }

let add_witness buffer w =
  Printf.bprintf buffer "%d" (List.length w);
  List.iter
    (fun part -> Printf.bprintf buffer " %d %s" (String.length part) part)
    w

let add_verdict buffer = function
  | Unreached -> Buffer.add_string buffer "unreached"
  | Killed -> Buffer.add_string buffer "killed"
  | Survived s ->
      let ws = s.witness :: s.others in
      Printf.bprintf buffer "survived %d" (List.length ws);
      List.iter
        (fun w ->
          Buffer.add_char buffer ' ';
          add_witness buffer w)
        ws

(* The records after the header. Every number is in decimal, and every
   string is length-prefixed as [<byte length> <bytes>], so it may hold any
   byte, a line feed included:

     <record count>
     <file> <line> <col> <rewrite> <before> <after> <verdict>

   with one such line for each record, in [Mutate.compare_id] order.
   [<verdict>] is [unreached], [killed], or [survived <n>] and then [n]
   reaching tests, each written as [<k>] and then its [k] names.
   [of_string] reads this grammar and nothing else. *)
let to_string ?identity t =
  let buffer = Buffer.create 1024 in
  Instr.add_header format buffer identity;
  Printf.bprintf buffer "%d\n" (Id_map.cardinal t);
  Id_map.iter
    (fun (id : M.id) (r, verdict) ->
      Printf.bprintf buffer "%d %s %d %d %d %s %d %s %d %s "
        (String.length id.M.file) id.M.file id.M.line id.M.col
        (String.length id.M.rewrite)
        id.M.rewrite (String.length r.r_before) r.r_before
        (String.length r.r_after) r.r_after;
      add_verdict buffer verdict;
      Buffer.add_char buffer '\n')
    t;
  Buffer.contents buffer

let of_string ?(path = "<string>") s =
  match Instr.start format ~path s with
  | Error e -> Error e
  | Ok c -> (
      let read_witness () =
        let n = Instr.read_count c "test path length" in
        let acc = ref [] in
        for _ = 1 to n do
          acc := Instr.read_name c "test name" :: !acc
        done;
        List.rev !acc
      in
      let read_verdict () =
        match Instr.read_word c "verdict" with
        | "unreached" -> Unreached
        | "killed" -> Killed
        | "survived" ->
            let n = Instr.read_count c "witness count" in
            if n = 0 then
              Instr.parse_fail
                "a survivor names no test (survived is not unreached)";
            let acc = ref [] in
            for _ = 1 to n do
              acc := read_witness () :: !acc
            done;
            survived !acc
        | word -> Instr.parse_fail "unknown verdict %S" word
      in
      try
        let identity = Instr.read_identity c in
        let record_count = Instr.read_count c "record count" in
        let result = ref empty in
        for _ = 1 to record_count do
          let file = Instr.read_name c "file name" in
          if file = "" then Instr.parse_fail "empty file name";
          let line = Instr.read_nat c "line" in
          if line < 1 then Instr.parse_fail "line %d is not 1-based" line;
          let col = Instr.read_nat c "column" in
          let rewrite = Instr.read_name c "rewrite" in
          if not (is_rewrite rewrite) then
            Instr.parse_fail "unknown rewrite %S" rewrite;
          let id = { M.file; line; col; rewrite } in
          if Id_map.mem id !result then
            Instr.parse_fail "duplicate record for %s" (M.id_to_string id);
          let before = Instr.read_name c "before" in
          let after = Instr.read_name c "after" in
          let verdict = read_verdict () in
          result := add !result { id; before; after; verdict }
        done;
        Instr.finish c;
        Ok (!result, identity)
      with Instr.Parse_error reason -> Error (Corrupt { path; reason }))

let load path =
  match Instr.read_file path with
  | Ok contents -> of_string ~path contents
  | Error e -> Error e

(* Output Path and Identity *)

let build_root = Instr.build_root
let exe_identity = Instr.exe_identity
let output_file ~exe = Instr.output_file format ~exe

(* Digesting the executable's bytes is what makes a stale verdict
   detectable: the reporting command re-digests the file at the recorded
   path, and any difference means the executable on disk is not the one
   that wrote the file - mtimes cannot say that, because dune's shared
   cache restores artifacts with their original timestamps. *)
let writer_identity ~exe =
  Option.map
    (fun digest -> { exe = exe_identity ~exe; digest })
    (Instr.file_digest exe)

(* Atomic Write *)

let save ?identity path t = Instr.write_file path (to_string ?identity t)
