(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Instr = Windtrap_instr

(* Identity *)

type id = { file : string; line : int; col : int; rewrite : string }

let magic = "windtrap-mutants-v3"

(* The constants Windtrap_instr's shared plumbing is parameterized by:
   this format's magic line, its on-disk home, and the words its error
   messages use. *)
let format =
  {
    Instr.magic;
    kind = "verdict";
    dir = "_mutants";
    ext = "mutants";
    remedy =
      "delete the stale files under _build/_mutants, then re-run the mutation \
       tests";
    who = "Windtrap_mutate";
  }

(* The closed rewrite vocabulary. Both the site table and every parser
   check against it: a rewrite name nobody can render is a report nobody
   can act on, so an unknown one is refused where it enters, never
   carried. *)
let rewrites =
  [
    "not";
    "lt";
    "le";
    "gt";
    "ge";
    "eq";
    "neq";
    "add";
    "sub";
    "fadd";
    "fsub";
    "and";
    "or";
    "drop";
  ]

let is_rewrite r = List.exists (String.equal r) rewrites

let id_to_string { file; line; col; rewrite } =
  Printf.sprintf "%s:%d:%d:%s" file line col rewrite

let pp_id ppf id = Format.pp_print_string ppf (id_to_string id)

let compare_id a b =
  let c = String.compare a.file b.file in
  if c <> 0 then c
  else
    let c = Int.compare a.line b.line in
    if c <> 0 then c
    else
      let c = Int.compare a.col b.col in
      if c <> 0 then c else String.compare a.rewrite b.rewrite

(* Sites and the Registry *)

type site = {
  line : int;
  col : int;
  rewrite : string;
  before : string;
  after : string;
  dismissed : string option;
}

type mutant = {
  id : id;
  before : string;
  after : string;
  dismissed : string option;
}

let compare_mutant a b = compare_id a.id b.id

(* One registration: the file's site table plus the three per-site arrays
   the guard closure owns. [armed_index] is this file's local index of the
   armed site, [-1] when none - a per-file cell rather than a global
   comparison against an absolute id, so the guard's armed test is one
   dereference and one integer compare. *)
type entry = {
  file : string;
  sites : site array;
  reach : int array;
  epoch : int array;
  base : int array; (* reach count when the current epoch's first hit landed *)
  armed_index : int ref;
}

let registry : entry list ref = ref []

(* Epochs start at 1 and only ever increase, so a fresh [epoch] array
   (all zeroes) marks every site as unseen and no bump can collide with a
   recorded value. *)
let current_epoch = ref 1
let dirty : (entry * int) list ref = ref []
let armed_mutant : mutant option ref = ref None
let armed_slots : (entry * int) list ref = ref []
let runaway_budget = ref max_int
let saturating_add x y = if x > max_int - y then max_int else x + y

exception Runaway of { id : id; hits : int; budget : int }

let warn fmt =
  Printf.ksprintf (fun m -> Printf.eprintf "windtrap mutate: %s\n%!" m) fmt

let validate ~file sites =
  Array.iteri
    (fun i s ->
      let bad fmt =
        Printf.ksprintf
          (fun m ->
            invalid_arg
              (Printf.sprintf "Windtrap_mutate: %s: site %d: %s" file i m))
          fmt
      in
      if s.line < 1 then bad "line %d is not 1-based" s.line;
      if s.col < 0 then bad "negative column %d" s.col;
      if not (is_rewrite s.rewrite) then bad "unknown rewrite %S" s.rewrite)
    sites

let site_equal a b =
  a.line = b.line && a.col = b.col
  && String.equal a.rewrite b.rewrite
  && String.equal a.before b.before
  && String.equal a.after b.after
  &&
  match (a.dismissed, b.dismissed) with
  | None, None -> true
  | Some x, Some y -> String.equal x y
  | Some _, None | None, Some _ -> false

let sites_equal a b =
  Array.length a = Array.length b && Array.for_all2 site_equal a b

let site_mutant entry i =
  let s = entry.sites.(i) in
  {
    id = { file = entry.file; line = s.line; col = s.col; rewrite = s.rewrite };
    before = s.before;
    after = s.after;
    dismissed = s.dismissed;
  }

(* The guard of a dropped registration: it must still be a total
   [int -> bool] because the generated code binds it unconditionally. *)
let inert (_ : int) = false

(* WINDTRAP_MUTATE_ONLY: which files this process has mutants in at all.

   Applied at REGISTRATION, not at reporting, and the difference is the
   whole point. A mutation run forks once per mutant, so a scope that
   only narrowed the report would still spend the afternoon; narrowing
   the registry narrows the work, leaves the guard inert for everything
   out of scope (so not even reaches are counted for code nobody is
   mutating), and makes an executable with nothing in scope
   indistinguishable from an uninstrumented one — which is what lets a
   fixture keep a controlled catalogue inside a tree whose own core is
   instrumented.

   Read once, at the first registration, because registrations run at
   module load and a value that changed halfway through would give one
   executable two different mutation surfaces. Stdlib only: this library
   must not pull the windtrap core in, so it cannot use Env. *)
let scope_variable = "WINDTRAP_MUTATE_ONLY"

let scope =
  lazy
    (match Sys.getenv_opt scope_variable with
    | None | Some "" -> []
    | Some value ->
        String.split_on_char ',' value
        |> List.map String.trim
        |> List.filter (fun s -> s <> ""))

let in_scope file =
  match Lazy.force scope with
  | [] -> true
  | prefixes ->
      List.exists (fun prefix -> String.starts_with ~prefix file) prefixes

let register ~file ~sites =
  validate ~file sites;
  if not (in_scope file) then inert
  else
  match List.find_opt (fun e -> String.equal e.file file) !registry with
  | Some prior when not (sites_equal prior.sites sites) ->
      (* Two incompatible instrumentations of one source file are linked
         into this executable - stale build artifacts, most likely.
         Registration runs at module load inside the user's program, so it
         must not raise; warn loudly and hand back an inert guard, keeping
         the invariant that same-file entries carry equal tables (which is
         what lets [arm] set them all). *)
      warn
        "%s: conflicting instrumentation tables in one executable (stale build \
         artifacts? try dune clean); ignoring one module's sites"
        file;
      inert
  | _ ->
      let n = Array.length sites in
      let entry =
        {
          file;
          sites;
          reach = Array.make n 0;
          epoch = Array.make n 0;
          base = Array.make n 0;
          armed_index = ref (-1);
        }
      in
      registry := entry :: !registry;
      let reach = entry.reach
      and epoch = entry.epoch
      and base = entry.base
      and armed_index = entry.armed_index in
      fun i ->
        let hits =
          let c = reach.(i) in
          if c = max_int then c else c + 1
        in
        reach.(i) <- hits;
        if epoch.(i) <> !current_epoch then begin
          epoch.(i) <- !current_epoch;
          base.(i) <- hits - 1;
          dirty := (entry, i) :: !dirty
        end;
        if i <> !armed_index then false
        else if hits > !runaway_budget then
          raise
            (Runaway
               { id = (site_mutant entry i).id; hits; budget = !runaway_budget })
        else true

let mutants_of entry =
  let acc = ref [] in
  for i = Array.length entry.sites - 1 downto 0 do
    acc := site_mutant entry i :: !acc
  done;
  !acc

let catalogue () =
  List.sort_uniq compare_mutant (List.concat_map mutants_of !registry)

(* Arming *)

type arm_error =
  | Malformed of { spec : string; reason : string }
  | Uncatalogued of { id : id }
  | Unmatched of { id : id; candidates : mutant list }
  | Ambiguous of { id : id; candidates : mutant list }

let pp_candidates ppf mutants =
  let shown = 6 in
  let rec loop i = function
    | [] -> ()
    | rest when i = shown ->
        Format.fprintf ppf "@\n    (and %d more)" (List.length rest)
    | m :: rest ->
        Format.fprintf ppf "@\n    %a" pp_id m.id;
        loop (i + 1) rest
  in
  loop 0 mutants

let pp_arm_error ppf = function
  | Malformed { spec; reason } ->
      Format.fprintf ppf
        "%S is not a mutant identifier: %s; expected \
         <file>:<line>:<col>:<rewrite>"
        spec reason
  | Uncatalogued { id } ->
      Format.fprintf ppf
        "%a: not this executable's mutant; it catalogues no site in %s (if you \
         expected one, is the library under test built with --instrument-with \
         ppx_windtrap.mutate?)"
        pp_id id id.file
  | Unmatched { id; candidates } ->
      Format.fprintf ppf "%a: no such mutation site; %s has these:%a" pp_id id
        id.file pp_candidates candidates
  | Ambiguous { id; candidates } ->
      Format.fprintf ppf
        "%a: names %d mutation sites, so no identifier can tell them apart (a \
         rewriter duplicating locations?); dismiss the expression with \
         [@mutate off] or exclude the file:%a"
        pp_id id (List.length candidates) pp_candidates candidates

let arm_variable = "WINDTRAP_MUTATE_ARM"
let is_digit = function '0' .. '9' -> true | _ -> false

(* Strict decimal: [int_of_string_opt] would also accept ["0x10"],
   ["1_0"] and ["+5"], none of which any spelling of an identifier
   contains. *)
let parse_nat s =
  if s = "" || not (String.for_all is_digit s) then None
  else int_of_string_opt s

let id_of_string spec =
  let malformed fmt =
    Printf.ksprintf (fun reason -> Error (Malformed { spec; reason })) fmt
  in
  let after s i = String.sub s (i + 1) (String.length s - i - 1) in
  match String.rindex_opt spec ':' with
  | None -> malformed "no ':' separator"
  | Some i -> (
      let rewrite = after spec i and rest = String.sub spec 0 i in
      if not (is_rewrite rewrite) then
        malformed "unknown rewrite %S (expected one of %s)" rewrite
          (String.concat ", " rewrites)
      else
        match String.rindex_opt rest ':' with
        | None -> malformed "no position before the rewrite"
        | Some j -> (
            let tail = after rest j and head = String.sub rest 0 j in
            match parse_nat tail with
            | None -> malformed "invalid column %S" tail
            | Some col -> (
                match String.rindex_opt head ':' with
                | None -> malformed "no line number"
                | Some k -> (
                    let text = after head k and file = String.sub head 0 k in
                    match parse_nat text with
                    | None -> malformed "invalid line %S" text
                    | Some _ when file = "" -> malformed "empty file name"
                    | Some line when line >= 1 ->
                        Ok { file; line; col; rewrite }
                    | Some _ -> malformed "line numbers are 1-based"))))

let matches (id : id) entry i =
  let s = entry.sites.(i) in
  String.equal entry.file id.file
  && s.line = id.line && s.col = id.col
  && String.equal s.rewrite id.rewrite

let matching id =
  List.concat_map
    (fun entry ->
      let acc = ref [] in
      for i = Array.length entry.sites - 1 downto 0 do
        if matches id entry i then acc := (entry, i) :: !acc
      done;
      !acc)
    !registry

let disarm () =
  List.iter (fun (entry, _) -> entry.armed_index := -1) !armed_slots;
  armed_slots := [];
  armed_mutant := None;
  runaway_budget := max_int

let armed () = !armed_mutant

(* The guard counts hits whether or not it answers [true], so the armed
   site's count is on the same arrays the reach map reads. Summed over the
   slots because the same source compiled into two modules arms together,
   and each copy counts its own evaluations. *)
let armed_hits () =
  List.fold_left
    (fun acc (entry, i) -> saturating_add acc entry.reach.(i))
    0 !armed_slots

let arm ?budget id =
  (match budget with
  | Some n when n <= 0 ->
      invalid_arg "Windtrap_mutate.arm: budget must be positive"
  | Some _ | None -> ());
  (* Disarm first, and unconditionally: a refused arming must never leave
     the previous mutant live, which would attribute the next run's
     verdict to the wrong site. *)
  disarm ();
  let slots = matching id in
  (* One entry carries one [armed_index], so two matching sites in one
     file cannot both be armed - and no identifier separates them, since
     they agree on position and rewrite. That is an ambiguity, not an
     arming: silently arming one would leave the other live and report a
     false survivor for code reached through it. A repeat across entries
     is the opposite case and is required: the same source compiled into
     two modules must arm together. *)
  let rec twice_in_one_entry seen = function
    | [] -> None
    | (entry, _) :: rest ->
        if List.memq entry seen then Some entry
        else twice_in_one_entry (entry :: seen) rest
  in
  match
    ( twice_in_one_entry [] slots,
      List.sort_uniq compare_mutant
        (List.map (fun (entry, i) -> site_mutant entry i) slots) )
  with
  | _, [] -> (
      (* The two ways an identifier can name nothing here, which are not
         the same failure and must not carry the same name. A file this
         executable catalogues no site in is a file it was not built
         from: the identifier is about some other binary, and there is
         nothing in this one to hide. A file it DOES catalogue, at a
         position no site occupies, is a wrong or stale identifier - the
         caller believes it named a mutant of this binary and it did
         not. Only the registry can tell them apart, so it is told here,
         in the answer, rather than left for a caller to re-derive from
         an empty candidate list. *)
      match
        List.filter (fun m -> String.equal m.id.file id.file) (catalogue ())
      with
      | [] -> Error (Uncatalogued { id })
      | candidates -> Error (Unmatched { id; candidates }))
  | None, [ mutant ] ->
      (* Every entry matching one mutant is armed: the same source file
         compiled into two modules must not leave one copy disarmed, or a
         mutant evaluated through it would report a false survivor. *)
      List.iter (fun (entry, i) -> entry.armed_index := i) slots;
      armed_slots := slots;
      armed_mutant := Some mutant;
      (runaway_budget := match budget with Some n -> n | None -> max_int);
      Ok mutant
  | Some entry, [ _ ] ->
      (* Indistinguishable duplicates: list one candidate per site, so the
         count the message reports is the number of sites and not the
         number of names they share. *)
      let candidates =
        List.filter_map
          (fun (e, i) -> if e == entry then Some (site_mutant e i) else None)
          slots
      in
      Error (Ambiguous { id; candidates })
  | _, candidates -> Error (Ambiguous { id; candidates })

let arm_from_env ?budget () =
  match Sys.getenv_opt arm_variable with
  | None | Some "" -> Ok None
  | Some spec ->
      Result.bind (id_of_string spec) (fun id ->
          Result.map Option.some (arm ?budget id))

(* The Reach Map *)

type reached = { mutant : mutant; hits : int }

let next_epoch () = incr current_epoch

let drain () =
  let marked = !dirty in
  dirty := [];
  let items =
    List.rev_map
      (fun (entry, i) ->
        {
          mutant = site_mutant entry i;
          hits = entry.reach.(i) - entry.base.(i);
        })
      marked
  in
  let rec dedup acc = function
    | [] -> List.rev acc
    | x :: rest -> (
        match acc with
        | y :: acc' when compare_mutant x.mutant y.mutant = 0 ->
            dedup ({ y with hits = saturating_add y.hits x.hits } :: acc') rest
        | _ -> dedup (x :: acc) rest)
  in
  dedup [] (List.sort (fun a b -> compare_mutant a.mutant b.mutant) items)

let reset_reach () =
  List.iter
    (fun entry ->
      Array.fill entry.reach 0 (Array.length entry.reach) 0;
      Array.fill entry.base 0 (Array.length entry.base) 0)
    !registry;
  dirty := [];
  incr current_epoch

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
      invalid_arg "Windtrap_mutate.survived: a survivor names at least one test"
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
  type t = id

  let compare = compare_id
end)

type record = { id : id; before : string; after : string; verdict : verdict }

let record_of_mutant (m : mutant) verdict =
  { id = m.id; before = m.before; after = m.after; verdict }

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

let to_string ?identity t =
  let buffer = Buffer.create 1024 in
  Instr.add_header format buffer identity;
  Printf.bprintf buffer "%d\n" (Id_map.cardinal t);
  Id_map.iter
    (fun id (r, verdict) ->
      Printf.bprintf buffer "%d %s %d %d %d %s %d %s %d %s "
        (String.length id.file) id.file id.line id.col
        (String.length id.rewrite) id.rewrite (String.length r.r_before)
        r.r_before (String.length r.r_after) r.r_after;
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
          let id = { file; line; col; rewrite } in
          if Id_map.mem id !result then
            Instr.parse_fail "duplicate record for %s" (id_to_string id);
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

let () =
  Printexc.register_printer (function
    | Runaway { id; hits; budget } ->
        Some
          (Printf.sprintf
             "Windtrap_mutate.Runaway: %s evaluated %d times (budget %d)"
             (id_to_string id) hits budget)
    | _ -> None)
