(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let saturating_add x y = if x > max_int - y then max_int else x + y

(* Identifiers *)

type id = { file : string; line : int; col : int; rewrite : string }

(* The closed vocabulary. [register] and [id_of_string] refuse a name outside
   it, since no report can render its mutant. *)
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
  ]

let is_rewrite r = List.exists (String.equal r) rewrites
let id_to_string id = strf "%s:%d:%d:%s" id.file id.line id.col id.rewrite
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

(* Sites and the catalogue *)

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

(* One registration of a file's table. Each evaluation of site [i] counts in
   [reach.(i)]; the first one in an epoch marks the site unless it is marked
   already, and [base.(i)] holds the count before it until [drain] takes the
   mark. *)
type entry = {
  file : string;
  sites : site array;
  reach : int array;
  seen : int array; (* the epoch of each site's last evaluation *)
  base : int array; (* [-1] while the site is unmarked *)
  mutable armed_index : int; (* [-1] when no site of the table is armed *)
}

(* The registrations of one file carry equal tables, which is what lets [arm]
   set them all at one index. Epochs start at 1 and only increase, so the
   zeroes of a fresh [seen] array mark no site as seen. *)
type state = {
  mutable registry : entry list; (* newest first *)
  mutable epoch : int;
  mutable marked : (entry * int) list; (* newest first *)
  mutable armed : entry list; (* the registrations of the armed file *)
  mutable budget : int;
}

let state =
  { registry = []; epoch = 1; marked = []; armed = []; budget = max_int }

exception Runaway of { id : id; hits : int; budget : int }

let site_mutant entry i =
  let s = entry.sites.(i) in
  {
    id = { file = entry.file; line = s.line; col = s.col; rewrite = s.rewrite };
    before = s.before;
    after = s.after;
    dismissed = s.dismissed;
  }

let err_site ~file i fmt =
  Printf.ksprintf
    (fun m ->
      invalid_arg (strf "Windtrap_runtime.Mutate: %s: site %d: %s" file i m))
    fmt

let validate ~file sites =
  Array.iteri
    (fun i s ->
      if s.line < 1 then err_site ~file i "line %d is not 1-based" s.line;
      if s.col < 0 then err_site ~file i "negative column %d" s.col;
      if not (is_rewrite s.rewrite) then
        err_site ~file i "unknown rewrite %S" s.rewrite)
    sites

let sites_equal a b =
  let site_equal a b =
    a.line = b.line && a.col = b.col
    && String.equal a.rewrite b.rewrite
    && String.equal a.before b.before
    && String.equal a.after b.after
    && Option.equal String.equal a.dismissed b.dismissed
  in
  Array.length a = Array.length b && Array.for_all2 site_equal a b

let register ~file ~sites =
  validate ~file sites;
  match List.find_opt (fun e -> String.equal e.file file) state.registry with
  | Some prior when not (sites_equal prior.sites sites) ->
      (* Registration runs at module load inside the user's program, so a
         stale build artifact warns, and its table is dropped. *)
      Instr.warn
        "%s: conflicting instrumentation tables in one executable (stale build \
         artifacts? rebuild from clean); ignoring one module's sites"
        file;
      Fun.const false
  | Some _ | None ->
      let n = Array.length sites in
      let entry =
        {
          file;
          sites;
          reach = Array.make n 0;
          seen = Array.make n 0;
          base = Array.make n (-1);
          armed_index = -1;
        }
      in
      state.registry <- entry :: state.registry;
      fun i ->
        let hits = saturating_add entry.reach.(i) 1 in
        entry.reach.(i) <- hits;
        if entry.seen.(i) <> state.epoch then begin
          entry.seen.(i) <- state.epoch;
          if entry.base.(i) < 0 then begin
            entry.base.(i) <- hits - 1;
            state.marked <- (entry, i) :: state.marked
          end
        end;
        if i <> entry.armed_index then false
        else if hits <= state.budget then true
        else
          raise
            (Runaway
               { id = (site_mutant entry i).id; hits; budget = state.budget })

let catalogue () =
  let mutants entry =
    List.init (Array.length entry.sites) (site_mutant entry)
  in
  List.sort_uniq compare_mutant (List.concat_map mutants state.registry)

(* Arming *)

type arm_error =
  | Malformed of { spec : string; reason : string }
  | Uncatalogued of { id : id }
  | Unmatched of { id : id; candidates : mutant list }
  | Ambiguous of { id : id; candidates : mutant list }

let pp_candidates ppf mutants =
  let shown = 6 in
  List.iteri
    (fun i m -> if i < shown then Format.fprintf ppf "@\n    %a" pp_id m.id)
    mutants;
  let more = List.length mutants - shown in
  if more > 0 then Format.fprintf ppf "@\n    (and %d more)" more

let pp_arm_error ppf = function
  | Malformed { spec; reason } ->
      Format.fprintf ppf
        "%S is not a mutant identifier: %s; expected \
         <file>:<line>:<col>:<rewrite>"
        spec reason
  | Uncatalogued { id } ->
      Format.fprintf ppf
        "%a: not this executable's mutant; it catalogues no site in %s (if you \
         expected one, is the library under test instrumented with \
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

let is_digit = function '0' .. '9' -> true | _ -> false

(* [int_of_string_opt] alone also reads ["0x10"], ["1_0"] and ["+5"]. *)
let parse_nat s =
  if s = "" || not (String.for_all is_digit s) then None
  else int_of_string_opt s

(* The fields are read from the right, so the file may hold colons, and the
   reason names the first field that cannot be read. *)
let id_of_string spec =
  let malformed fmt =
    Printf.ksprintf (fun reason -> Error (Malformed { spec; reason })) fmt
  in
  let number field s =
    match parse_nat s with
    | Some n -> Ok n
    | None -> malformed "invalid %s %S" field s
  in
  let ( let* ) = Result.bind in
  match List.rev (String.split_on_char ':' spec) with
  | [] | [ _ ] -> malformed "no ':' separator"
  | rewrite :: _ when not (is_rewrite rewrite) ->
      malformed "unknown rewrite %S (expected one of %s)" rewrite
        (String.concat ", " rewrites)
  | [ _; _ ] -> malformed "no position before the rewrite"
  | [ _; col; _ ] ->
      let* _ = number "column" col in
      malformed "no line number"
  | rewrite :: col :: line :: file ->
      let* col = number "column" col in
      let* line = number "line" line in
      let file = String.concat ":" (List.rev file) in
      if file = "" then malformed "empty file name"
      else if line < 1 then malformed "line numbers are 1-based"
      else Ok { file; line; col; rewrite }

let disarm () =
  List.iter (fun entry -> entry.armed_index <- -1) state.armed;
  state.armed <- [];
  state.budget <- max_int

let arm ?(budget = max_int) (id : id) =
  if budget <= 0 then
    invalid_arg "Windtrap_runtime.Mutate.arm: budget must be positive";
  (* A refused arming must not leave the previous mutant live, or the next
     verdict would be the wrong site's. *)
  disarm ();
  let catalogues entry =
    String.equal entry.file id.file && Array.length entry.sites > 0
  in
  match List.filter catalogues state.registry with
  | [] -> Error (Uncatalogued { id })
  | first :: _ as entries -> (
      (* The tables are equal, so the indices of the first are every one's. *)
      let names i =
        let s = first.sites.(i) in
        s.line = id.line && s.col = id.col && String.equal s.rewrite id.rewrite
      in
      match List.filter names (List.init (Array.length first.sites) Fun.id) with
      | [] ->
          let candidates =
            List.filter (fun m -> String.equal m.id.file id.file) (catalogue ())
          in
          Error (Unmatched { id; candidates })
      | [ i ] ->
          List.iter (fun entry -> entry.armed_index <- i) entries;
          state.armed <- entries;
          state.budget <- budget;
          Ok (site_mutant first i)
      | indices ->
          (* A table arms one index, and no identifier tells these apart,
             so arming one would leave the others live. *)
          let candidates = List.map (site_mutant first) indices in
          Error (Ambiguous { id; candidates }))

(* Each registration of the armed file counts its own evaluations. *)
let armed_hits () =
  List.fold_left
    (fun acc entry -> saturating_add acc entry.reach.(entry.armed_index))
    0 state.armed

(* The reach map *)

type reached = { mutant : mutant; hits : int }

let next_epoch () = state.epoch <- state.epoch + 1

let drain () =
  let marked = state.marked in
  state.marked <- [];
  let reached (entry, i) =
    let hits = entry.reach.(i) - entry.base.(i) in
    entry.base.(i) <- -1;
    { mutant = site_mutant entry i; hits }
  in
  (* Each registration of a file marks its own copy of a mutant. *)
  let merge acc r =
    match acc with
    | prev :: acc when compare_mutant prev.mutant r.mutant = 0 ->
        { prev with hits = saturating_add prev.hits r.hits } :: acc
    | acc -> r :: acc
  in
  let by_mutant a b = compare_mutant a.mutant b.mutant in
  List.rev
    (List.fold_left merge []
       (List.sort by_mutant (List.rev_map reached marked)))

let reset_reach () =
  List.iter
    (fun entry ->
      Array.fill entry.reach 0 (Array.length entry.reach) 0;
      Array.fill entry.base 0 (Array.length entry.base) (-1))
    state.registry;
  state.marked <- [];
  next_epoch ()

let () =
  Printexc.register_printer (function
    | Runaway { id; hits; budget } ->
        Some
          (strf
             "Windtrap_runtime.Mutate.Runaway: %s evaluated %d times (budget \
              %d)"
             (id_to_string id) hits budget)
    | _ -> None)
