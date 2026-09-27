(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Shrink_tree = Gen.Engine.Shrink_tree

let one_line text =
  String.concat " " (Text.split_lines (Text.normalize_newlines text))

(* Abstract types *)

(* A side rides an exception local to its key, so one store holds the sides
   of every type and only their key projects them back. *)
type 'a key = { inject : 'a -> exn; project : exn -> 'a option }

let key (type a) () : a key =
  let module Side = struct
    exception Side of a
  end in
  {
    inject = (fun v -> Side.Side v);
    project = (function Side.Side v -> Some v | _ -> None);
  }

(* The id tells types apart where no value is at hand: in the subset of a
   case, in drawing and in the check of the prefixes. Each side has its own
   key, since a run holds a value's sides apart: the system's in its pool,
   the reference's by the step that made it. *)
type ('r, 's) abstract = {
  id : int;
  prefix : string;
  pp : (Format.formatter -> 'r -> unit) option;
  invariant : ('r -> 's -> unit) option;
  release : ('s -> unit) option;
  reference : 'r key;
  system : 's key;
}

let next_id = Atomic.make 0

let abstract ?pp ?invariant ?release prefix =
  {
    id = Atomic.fetch_and_add next_id 1;
    prefix;
    pp;
    invariant;
    release;
    reference = key ();
    system = key ();
  }

(* Signatures *)

type ('r, 's) form =
  | Returns : 'a Testable.t -> ('a, 'a) form
  | Makes : ('r, 's) abstract -> ('r, 's) form
  | Judges : 'a Testable.t -> (('a, exn) result -> unit, 'a) form

type ('r, 's, 'p) fn =
  | Result : ('r, 's) form -> ('r, 's, bool) fn
  | Drawn : 'a Gen.t * ('r, 's, 'p) fn -> ('a -> 'r, 'a -> 's, 'a -> 'p) fn
  | Chosen :
      ('ra, 'sa) abstract * ('r, 's, 'p) fn
      -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) fn

let ( @-> ) gen fn = Drawn (gen, fn)
let ( ^-> ) t fn = Chosen (t, fn)
let returns w = Result (Returns w)
let makes t = Result (Makes t)
let judges w = Result (Judges w)

(* Commands *)

(* The signature is existential, and it types the three functions. *)
type command =
  | Command : {
      name : string;
      loc : Loc.t option;
      fn : ('r, 's, 'p) fn;
      pre : 'p option;
      reference : 'r;
      system : 's;
    }
      -> command

let command ?__POS__ ?pre name fn reference system =
  let loc = Loc.resolve ?__POS__ () in
  Command { name = one_line name; loc; fn; pre; reference; system }

(* The abstract types of a command, for drawing and for the check of the
   prefixes: those it takes and the one it makes. *)
type any = Any : ('r, 's) abstract -> any
type shape = { takes : any list; makes : any option }

let type_id (Any t) = t.id

let rec shape : type r s p. (r, s, p) fn -> shape = function
  | Result (Makes t) -> { takes = []; makes = Some (Any t) }
  | Result (Returns _ | Judges _) -> { takes = []; makes = None }
  | Drawn (_, fn) -> shape fn
  | Chosen (t, fn) ->
      let shape = shape fn in
      { shape with takes = Any t :: shape.takes }

let command_shape (Command c) = shape c.fn

(* On several domains only the prefix has one reference state, so only there
   can a value be named or a [pre] be asked: after it, a command runs only
   when it makes no value and has no [pre]. *)
let runs_anywhere (Command c as command) =
  Option.is_none c.pre && Option.is_none (command_shape command).makes

(* [first_positions commands] is, for each command, the position of its first
   occurrence in [commands], so a command listed twice is one command. *)
let first_positions commands =
  let first c =
    let rec find i = if commands.(i) == c then i else find (i + 1) in
    find 0
  in
  Array.map first commands

(* Checking *)

(* ASCII only, and no double quote: [Printexc] prints these payloads with
   [%S]. *)
let check_prefix prefix =
  let is_ident = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let fail reason =
    invalid_arg
      (Pp.str "Windtrap.stateful: the prefix '%s' of an abstract type %s" prefix
         reason)
  in
  (match prefix with
  | "" -> fail "is not a lowercase OCaml identifier"
  | _ -> (
      match prefix.[0] with
      | ('a' .. 'z' | '_') when String.for_all is_ident prefix -> ()
      | _ -> fail "is not a lowercase OCaml identifier"));
  match prefix.[String.length prefix - 1] with
  | '0' .. '9' -> fail "ends with a digit"
  | _ -> ()

(* The types are gathered first, so that no frame of the standard library
   stands between a raise and the test in the printed backtrace. *)
let check ~steps ~domains commands =
  if Array.length commands = 0 then
    invalid_arg "Windtrap.stateful: no commands to draw from";
  if steps < 0 then invalid_arg "Windtrap.stateful: negative steps";
  if domains < 1 then invalid_arg "Windtrap.stateful: domains below 1";
  let types command =
    let { takes; makes } = command_shape command in
    takes @ Option.to_list makes
  in
  let owners = Hashtbl.create 8 in
  let rec check_all = function
    | [] -> ()
    | Any t :: types ->
        check_prefix t.prefix;
        (match Hashtbl.find_opt owners t.prefix with
        | Some owner when owner <> t.id ->
            invalid_arg
              (Pp.str
                 "Windtrap.stateful: two abstract types have the prefix '%s'"
                 t.prefix)
        | Some _ -> ()
        | None -> Hashtbl.replace owners t.prefix t.id);
        check_all types
  in
  check_all (List.concat_map types (Array.to_list commands));
  if domains > 1 && not (Array.exists runs_anywhere commands) then
    invalid_arg
      "Windtrap.stateful: on several domains every command makes a value or \
       has a ~pre, so no call can run after the prefix"

(* Programs *)

(* A drawn call's signature with every argument filled: a sample, or the
   choice of a value of an abstract type as the step of the drawn call that
   makes it. The call names the value while it makes one, so deleting other
   calls does not move the choice. *)
type ('r, 's, 'p) args =
  | Last : ('r, 's) form -> ('r, 's, bool) args
  | Sample :
      'a Gen.Engine.sample * ('r, 's, 'p) args
      -> ('a -> 'r, 'a -> 's, 'a -> 'p) args
  | Index :
      ('ra, 'sa) abstract * int * ('r, 's, 'p) args
      -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) args

type call =
  | Call : {
      step : int; (* the call's position in the drawn program *)
      command : int; (* the command's first position in the list *)
      name : string;
      loc : Loc.t option;
      args : ('r, 's, 'p) args;
      pre : 'p option;
      reference : 'r;
      system : 's;
    }
      -> call

(* A call that ran. [made] is set when the call made a value, [result] when
   its system's outcome is no value. *)
type row = {
  command : int;
  name : string;
  domain : int option; (* the branch of a parallel call, from 1 *)
  before : string option; (* the [reference before] cell *)
  call : string Lazy.t; (* [name a1 … an] *)
  judging : bool; (* its signature ends in [judges] *)
  mutable made : string option;
  mutable result : string Lazy.t option;
}

(* On one domain [prefix] is the whole program and [branches] is empty. On
   [n] domains [branches] holds [n] lists, which shrinking may empty. *)
type program = {
  prefix : call list;
  branches : call list array;
  suffix : call list;
  held : bool array; (* the case's subset, by position *)
  mutable record : row list option; (* the rows of the last run, in order *)
}

(* Printing *)

(* An argument rides the failure payload, which is capped at 64 KiB, so it
   is bounded in bytes; a cell is a column, so it is bounded in code points.
   A longer record prints its first and last [context_calls]. *)
let argument_bytes = 200
let cell_chars = 60
let context_calls = 20
let call_count n = Pp.str "%d call%s" n (if n = 1 then "" else "s")

let argument_text sample =
  let text = match Gen.Engine.render sample with Value t | Pre_image t -> t in
  let text = Text.truncate_bytes_utf8 argument_bytes (one_line text) in
  if String.contains text ' ' || String.starts_with ~prefix:"-" text then
    "(" ^ text ^ ")"
  else text

let row_text row =
  let call = Lazy.force row.call in
  match row.made with Some v -> "let " ^ v ^ " = " ^ call | None -> call

let is_parallel row = Option.is_some row.domain

(* The columns after [#]: [reference before] when a shown row has a cell,
   [domain] when the record has a parallel call, [call], then [result] when
   it has a parallel or a judging call. *)
let columns ~parallel ~judging rows =
  let before row = Option.value row.before ~default:"" in
  let domain row = Option.fold ~none:"" ~some:string_of_int row.domain in
  let result row = Option.fold ~none:"" ~some:Lazy.force row.result in
  List.concat
    [
      (if List.exists (fun row -> Option.is_some row.before) rows then
         [ ("reference before", before) ]
       else []);
      (if parallel then [ ("domain", domain) ] else []);
      [ ("call", row_text) ];
      (if parallel || judging then [ ("result", result) ] else []);
    ]

(* Hard newlines and padding only: the engine renders through
   [Format.asprintf] at its default margin, which would re-wrap break
   hints. *)
let record_text = function
  (* A reachable counterexample: an empty text would print a bare colon. *)
  | [] -> "(no calls)"
  | rows ->
      let total = List.length rows in
      let numbered =
        List.mapi (fun i row -> (string_of_int (i + 1), row)) rows
      in
      let omitted = total - (2 * context_calls) in
      let head, omission, tail =
        if omitted <= 0 then (numbered, [], [])
        else
          ( List.filteri (fun i _ -> i < context_calls) numbered,
            [ Pp.str "\u{2026} (%s omitted)" (call_count omitted) ],
            List.filteri (fun i _ -> i >= total - context_calls) numbered )
      in
      let shown = List.map snd (head @ tail) in
      let columns =
        columns
          ~parallel:(List.exists is_parallel rows)
          ~judging:(List.exists (fun row -> row.judging) rows)
          shown
      in
      let widths =
        List.map
          (fun (header, cell) ->
            List.fold_left
              (fun width row -> max width (Text.length_utf8 (cell row)))
              (String.length header) shown)
          columns
      in
      let number_width = max 2 (String.length (string_of_int total)) in
      (* Every cell but a line's last is padded, and the blank cells that
         end a line are dropped, so no line ends on a blank. *)
      let rec cells = function
        | [] -> ""
        | (width, cell) :: rest ->
            if List.for_all (fun (_, cell) -> cell = "") rest then "  " ^ cell
            else
              "  " ^ cell
              ^ String.make (width - Text.length_utf8 cell) ' '
              ^ cells rest
      in
      let line number texts =
        String.make (number_width - String.length number) ' '
        ^ number
        ^ cells (List.combine widths texts)
      in
      let row (number, row) =
        line number (List.map (fun (_, cell) -> cell row) columns)
      in
      String.concat "\n"
        ((line "#" (List.map fst columns) :: List.map row head)
        @ omission @ List.map row tail)

let pp_program ppf program =
  Pp.string ppf
    (match program.record with
    | None -> "(not run)"
    | Some rows -> record_text rows)

let summary program =
  match program.record with
  | None | Some [] -> None
  | Some rows -> (
      let total = call_count (List.length rows) in
      match List.length (List.filter is_parallel rows) with
      | 0 ->
          let last = List.nth rows (List.length rows - 1) in
          Some (Pp.str "%s, last: %s" total last.name)
      | parallel -> Some (Pp.str "%s, %d in parallel" total parallel))

(* Drawing *)

(* An index shrinks toward [0], the newest maker, as [Gen]'s integers do:
   [0] first, then candidates that each close half the gap to [k]. *)
let rec index_tree k =
  let rec candidates current () =
    if current = k then Seq.Nil
    else
      let gap = (k / 2) - (current / 2) in
      let rest = if gap = 0 then Seq.empty else candidates (current + gap) in
      Seq.Cons (index_tree current, rest)
  in
  Shrink_tree.make ~root:k ~children:(candidates 0)

(* The newest value with probability 1/2, else any of the [count]. *)
let draw_index count state =
  let newest, state = Seed.below ~bound:2L state in
  if Int64.equal newest 0L then (0, state)
  else
    let k, state = Seed.below ~bound:(Int64.of_int count) state in
    (Int64.to_int k, state)

(* Arguments are counted from one, abstract ones included. *)
let rec draw_args : type r s p.
    name:string ->
    makers:(int -> int list) ->
    int ->
    (r, s, p) fn ->
    Seed.state ->
    (r, s, p) args Shrink_tree.t * Seed.state =
 fun ~name ~makers position fn state ->
  match fn with
  | Result form -> (Shrink_tree.leaf (Last form), state)
  | Drawn (gen, fn) ->
      let sample, state = Gen.Engine.draw gen state in
      if not (Gen.Engine.prints (Shrink_tree.root sample)) then
        invalid_arg
          (Pp.str "%s: argument %d has no printer; attach one with Gen.with_pp"
             name position);
      let args, state = draw_args ~name ~makers (position + 1) fn state in
      let tree = Shrink_tree.pair sample args in
      (Shrink_tree.map (fun (sample, args) -> Sample (sample, args)) tree, state)
  | Chosen (t, fn) ->
      let steps = makers t.id in
      let k, state = draw_index (List.length steps) state in
      let args, state = draw_args ~name ~makers (position + 1) fn state in
      let maker k = List.nth steps k in
      let tree = Shrink_tree.pair (Shrink_tree.map maker (index_tree k)) args in
      (Shrink_tree.map (fun (maker, args) -> Index (t, maker, args)) tree, state)

(* Each command joins the subset with probability 3/4, every command when
   none does, and a command in the subset brings the makers of the types it
   takes. *)
let draw_subset firsts shapes state =
  let n = Array.length firsts in
  let joins = Array.make n false in
  let state = ref state in
  for i = 0 to n - 1 do
    if firsts.(i) = i then begin
      let quarter, next = Seed.below ~bound:4L !state in
      joins.(i) <- not (Int64.equal quarter 0L);
      state := next
    end
  done;
  if not (Array.exists Fun.id joins) then Array.fill joins 0 n true;
  let held = Array.make n false in
  let is_maker t shape = Option.map type_id shape.makes = Some (type_id t) in
  let rec hold i =
    let i = firsts.(i) in
    if not held.(i) then begin
      held.(i) <- true;
      let hold_makers t =
        Array.iteri (fun j shape -> if is_maker t shape then hold j) shapes
      in
      List.iter hold_makers shapes.(i).takes
    end
  in
  Array.iteri (fun i joined -> if joined then hold i) joins;
  Array.iteri (fun i first -> held.(i) <- held.(first)) firsts;
  (held, !state)

(* Where a call of a program on several domains runs. *)
type slot = Prefix | Branch of int | Suffix

(* The calls of each branch: the most, at most ten in all, whose orders stay
   within 7! = 5040, and never fewer than one. Two domains take five calls
   each (252 orders), three take three (1680), four take two (2520), five to
   seven take one; from eight domains one call each gives n! orders. *)
let branch_calls = function 2 -> 5 | 3 -> 3 | 4 -> 2 | _ -> 1

(* A parallel call's first candidates move it to the end of the prefix,
   then to the start of the suffix, where it runs alone. A moved call has no
   such candidate, so the tree stays finite in depth. *)
let rec slotted slot tree =
  let moved slot = Shrink_tree.map (fun call -> (slot, call)) tree in
  match slot with
  | Prefix | Suffix -> moved slot
  | Branch _ ->
      let rec argument_moves children () =
        match children () with
        | Seq.Nil -> Seq.Nil
        | Seq.Cons (child, rest) ->
            Seq.Cons (slotted slot child, argument_moves rest)
      in
      let moves () =
        Seq.Cons
          ( moved Prefix,
            fun () ->
              Seq.Cons (moved Suffix, argument_moves (Shrink_tree.children tree))
          )
      in
      Shrink_tree.make ~root:(slot, Shrink_tree.root tree) ~children:moves

(* The calls in program order: the prefix, each branch, the suffix, each in
   the order the list holds it, so a call moved to the prefix ends it and one
   moved to the suffix starts it. *)
let parallel_program ~domains held calls =
  let in_slot slot =
    List.filter_map
      (fun (s, call) -> if s = slot then Some call else None)
      calls
  in
  {
    prefix = in_slot Prefix;
    branches = Array.init domains (fun i -> in_slot (Branch i));
    suffix = in_slot Suffix;
    held;
    record = None;
  }

(* No reference runs here: a command is drawn when every type it takes has a
   value that an earlier call makes, if both of its sides return. Nothing
   repairs a candidate; [execute] skips what does not resolve. [makers]
   holds, per type, the steps of the calls that make it, newest first. After
   the prefix only a command that [runs_anywhere] is drawn, so a branch and
   the suffix choose among the prefix's values. *)
let program ?(steps = 20) ?(domains = 1) commands =
  let commands = Array.of_list commands in
  let firsts = first_positions commands in
  let shapes = Array.map command_shape commands in
  let anywhere = Array.map runs_anywhere commands in
  let draw state =
    check ~steps ~domains commands;
    let held, state = draw_subset firsts shapes state in
    let made = Hashtbl.create 8 in
    let makers t = Option.value ~default:[] (Hashtbl.find_opt made t) in
    let drawable ~prefix i =
      held.(i)
      && (prefix || anywhere.(i))
      && List.for_all (fun t -> makers (type_id t) <> []) shapes.(i).takes
    in
    let draw_call step i state =
      match commands.(i) with
      | Command { name; loc; fn; pre; reference; system } ->
          let args, state = draw_args ~name ~makers 1 fn state in
          let call args =
            Call
              {
                step;
                command = firsts.(i);
                name;
                loc;
                args;
                pre;
                reference;
                system;
              }
          in
          (Shrink_tree.map call args, state)
    in
    let positions = List.init (Array.length commands) Fun.id in
    let next_step = ref 0 in
    let rec draw_calls ~prefix n trees state =
      match if n = 0 then [] else List.filter (drawable ~prefix) positions with
      | [] -> (List.rev trees, state)
      | drawable ->
          let bound = Int64.of_int (List.length drawable) in
          let pick, state = Seed.below ~bound state in
          let i = List.nth drawable (Int64.to_int pick) in
          let step = !next_step in
          incr next_step;
          let tree, state = draw_call step i state in
          let make t =
            Hashtbl.replace made (type_id t) (step :: makers (type_id t))
          in
          Option.iter make shapes.(i).makes;
          draw_calls ~prefix (n - 1) (tree :: trees) state
    in
    if domains = 1 then
      let trees, state = draw_calls ~prefix:true steps [] state in
      let program calls =
        { prefix = calls; branches = [||]; suffix = []; held; record = None }
      in
      (Shrink_tree.map program (Shrink_tree.list trees), state)
    else
      let below n state =
        let k, state = Seed.below ~bound:(Int64.of_int (n + 1)) state in
        (Int64.to_int k, state)
      in
      let prefix_calls, state = below steps state in
      let suffix_calls, state = below (steps - prefix_calls) state in
      let prefix, state = draw_calls ~prefix:true prefix_calls [] state in
      let rec draw_branches i trees state =
        if i = domains then (List.concat (List.rev trees), state)
        else
          let branch, state =
            draw_calls ~prefix:false (branch_calls domains) [] state
          in
          draw_branches (i + 1)
            (List.map (slotted (Branch i)) branch :: trees)
            state
      in
      let branches, state = draw_branches 0 [] state in
      let suffix, state = draw_calls ~prefix:false suffix_calls [] state in
      let calls =
        List.map (slotted Prefix) prefix
        @ branches
        @ List.map (slotted Suffix) suffix
      in
      ( Shrink_tree.map (parallel_program ~domains held) (Shrink_tree.list calls),
        state )
  in
  Gen.Engine.make ~pp:pp_program draw

(* Running *)

(* The reference sides of a run, or of a replay by the judge, by the step of
   the call that made each. *)
type sides = (int, exn) Hashtbl.t

(* A value that a system made. Its reference side is in the run's [sides],
   under the step of the call that made it. *)
type value = {
  step : int; (* the step of the call that made it *)
  name : string;
  system : exn;
  invariant : (unit -> unit) option;
}

type release = { label : string; release : unit -> unit }
type 'a outcome = Returned of 'a | Raised of exn * Printexc.raw_backtrace

(* What is never an outcome: a verb's failure or a broken contract, as its
   failure, and a discard. *)
type never = Failed of Failure.t | Discarded

(* A call on the way to a record: bound, with how its system ended once it
   ran, an outcome or what is none. [number] is its row's number once the
   row is recorded. *)
type pending =
  | Pending : {
      step : int;
      loc : Loc.t option;
      row : row;
      mutable number : int;
      form : ('r, 's) form;
      reference : sides -> unit -> 'r;
      system : unit -> 's;
      mutable ended : ('s outcome, never) result option;
    }
      -> pending

type run = {
  sides : sides;
  mutable pool : value list; (* newest first *)
  counts : (string, int) Hashtbl.t; (* the values made, per prefix *)
  mutable releases : release list; (* newest first *)
  mutable rows : row list; (* newest first *)
  mutable ran : pending list; (* the prefix's calls that ran, newest first *)
}

(* [reference_side sides t step] is the reference side of the value that the
   call at [step] made. A value joins the pool when its system returns, and
   its call ends the run unless the reference returned a side too, so every
   value that a later call or an invariant reads has one. A replay by the
   judge replays the prefix, whose outcomes it checks, so it has a side
   wherever the run has one. *)
let reference_side sides (t : (_, _) abstract) step =
  match Option.bind (Hashtbl.find_opt sides step) t.reference.project with
  | Some r -> r
  | None -> assert false

(* [resolve pool t maker] is the value of [t] that the call at step [maker]
   made, or when it made none, the newest value of [t], with its system
   side. *)
let resolve pool (t : (_, _) abstract) maker =
  let of_type value =
    Option.map (fun s -> (value, s)) (t.system.project value.system)
  in
  match List.find_opt (fun value -> value.step = maker) pool with
  | Some value -> of_type value
  | None -> List.find_map of_type pool

(* A call whose arguments resolved. Its functions apply their arguments
   when called, so that no code of the user runs before [pre] holds. The
   reference reads its arguments' sides from the [sides] it is given, so
   the judge replays it on sides of its own; [pre] and the cells read the
   run's. *)
type ready =
  | Ready : {
      form : ('r, 's) form;
      pre : (unit -> bool) option;
      reference : sides -> unit -> 'r;
      system : unit -> 's;
      words : string Lazy.t list; (* the call's text after its name *)
      cells : (unit -> string) list; (* the reference sides that print *)
    }
      -> ready

(* Guarded here: the engine's guard would replace the whole table. A control
   keeps its meaning, since the cell is printed while the call runs. *)
let reference_cell pp r =
  match Failure.catch (fun () -> Pp.to_string pp r) with
  | Ok text -> one_line text
  | Error ((#Failure.fault | `Discard) as c) ->
      Pp.str "<pp raised %s>" (Failure.caught_to_string c)
  | Error (#Failure.control as c) -> Failure.reraise c

(* [bind run call] is [call] with its abstract arguments resolved among the
   values of [run], and [None] when one does not resolve. *)
let bind run (Call c) =
  let rec apply : type r s p.
      (r, s, p) args ->
      (unit -> p) option ->
      (sides -> unit -> r) ->
      (unit -> s) ->
      string Lazy.t list ->
      (unit -> string) list ->
      ready option =
   fun args pre reference system words cells ->
    match args with
    | Last form ->
        let words = List.rev words and cells = List.rev cells in
        Some (Ready { form; pre; reference; system; words; cells })
    | Sample (sample, args) ->
        let v = Gen.Engine.value sample in
        let reference sides =
          let f = reference sides in
          fun () -> f () v
        in
        apply args
          (Option.map (fun pre () -> pre () v) pre)
          reference
          (fun () -> system () v)
          (lazy (argument_text sample) :: words)
          cells
    | Index (t, maker, args) -> (
        match resolve run.pool t maker with
        | None -> None
        | Some (value, s) ->
            let r = reference_side run.sides t value.step in
            let reference sides =
              let f = reference sides in
              fun () -> f () (reference_side sides t value.step)
            in
            let cells =
              match t.pp with
              | None -> cells
              | Some pp -> (fun () -> reference_cell pp r) :: cells
            in
            apply args
              (Option.map (fun pre () -> pre () r) pre)
              reference
              (fun () -> system () s)
              (Lazy.from_val value.name :: words)
              cells)
  in
  apply c.args
    (Option.map (fun pre () -> pre) c.pre)
    (fun _ () -> c.reference)
    (fun () -> c.system)
    [] []

(* Outcomes *)

(* The failure of a fault, or of a discard, which fails in a command. *)
let failure_of = function
  | `Assertion failure -> failure
  | `Exception (exn, backtrace) ->
      Failure.raised
        ~actual:(Failure.exn_to_string exn)
        ~backtrace:(Failure.backtrace_to_string backtrace)
        ()
  | `Discard ->
      Failure.message
        "assume or reject in a command; a call's legality is its ~pre"

let never_failure = function
  | Failed failure -> failure
  | Discarded -> failure_of `Discard

(* Every control but a discard keeps its meaning, in [side] and [guard]. *)
let side fn =
  match Failure.catch fn with
  | Ok v -> Ok (Returned v)
  | Error
      ((`Assertion _ | `Exception ((Assert_failure _ | Match_failure _), _)) as
       broken) ->
      Error (Failed (failure_of broken))
  | Error (`Exception (exn, backtrace)) -> Ok (Raised (exn, backtrace))
  | Error `Discard -> Error Discarded
  | Error (#Failure.control as c) -> Failure.reraise c

let guard fn =
  match Failure.catch fn with
  | Ok v -> Ok v
  | Error ((#Failure.fault | `Discard) as c) -> Error (failure_of c)
  | Error (#Failure.control as c) -> Failure.reraise c

(* Two exceptions are equal when their constructors are, the module path
   removed, whatever their payloads. *)
let same_constructor a b =
  let name exn =
    let slot = Printexc.exn_slot_name exn in
    match String.rindex_opt slot '.' with
    | None -> slot
    | Some i -> String.sub slot (i + 1) (String.length slot - i - 1)
  in
  String.equal (name a) (name b)

let mismatch reference system =
  let exn = function
    | Raised (exn, _) -> Some (Failure.exn_to_string exn)
    | Returned _ -> None
  in
  let backtrace =
    match system with
    | Raised (_, backtrace) -> Some (Failure.backtrace_to_string backtrace)
    | Returned _ -> None
  in
  Failure.raised ?expected:(exn reference) ?actual:(exn system) ?backtrace ()

(* The failure of two outcomes that differ, the reference's first. *)
let differ w reference system =
  match (reference, system) with
  | Returned r, Returned s ->
      if Testable.equal w r s then None
      else
        Some
          (Failure.equality ~expected:(Testable.to_string w r)
             ~actual:(Testable.to_string w s) ())
  | Raised (r, _), Raised (s, _) when same_constructor r s -> None
  | (Returned _ | Raised _), _ -> Some (mismatch reference system)

let seen = function Returned v -> Ok v | Raised (exn, _) -> Error exn

(* A result prints only in a table's [result] column, when the table is
   printed: whatever its printer raises then is the cell. *)
let printed w v =
  lazy
    (match Failure.catch (fun () -> Testable.to_string w v) with
    | Ok text -> Text.truncate_utf8 cell_chars (one_line text)
    | Error c -> Pp.str "<pp raised %s>" (Failure.caught_to_string c))

let result_text : type r s. (r, s) form -> s outcome -> string Lazy.t option =
 fun form outcome ->
  match (outcome, form) with
  | Raised (exn, _), _ -> Some (lazy ("exception " ^ Failure.exn_to_string exn))
  | Returned v, Returns w -> Some (printed w v)
  | Returned v, Judges w -> Some (printed w v)
  | Returned _, Makes _ -> None

(* Executing *)

(* [label] is spelled only on failure. A headline prints [msg] on one line,
   so the user's own is flattened into it. *)
let attribute ?loc label (failure : Failure.t) =
  let msg =
    match failure.msg with
    | None -> label
    | Some msg -> label ^ "; " ^ one_line msg.kept
  in
  let loc = match failure.loc with None -> loc | Some _ -> failure.loc in
  { failure with msg = Some (Failure.text msg); loc }

(* [order] is the order of the calls that the judge of several domains was
   replaying. *)
let call_label ?order what ~total (Pending p) =
  let order = match order with None -> "" | Some o -> ", in the order " ^ o in
  Pp.str "%scall %d of %d%s: %s" what p.number total order
    (Lazy.force p.row.call)

let fresh_run () =
  {
    sides = Hashtbl.create 8;
    pool = [];
    counts = Hashtbl.create 8;
    releases = [];
    rows = [];
    ran = [];
  }

(* A system side is released once per type: an earlier value of its type
   that holds it physically registered its release already. [Obj] alone
   would compare across types. *)
let add_release run (t : (_, _) abstract) s ~label =
  let holds value =
    match t.system.project value.system with
    | Some s' -> s' == s
    | None -> false
  in
  match t.release with
  | Some release when not (List.exists holds run.pool) ->
      run.releases <- { label; release = (fun () -> release s) } :: run.releases
  | Some _ | None -> ()

(* A value is made when its system returns, and named then. *)
let make run ~step (t : (_, _) abstract) s =
  let count =
    1 + Option.value ~default:0 (Hashtbl.find_opt run.counts t.prefix)
  in
  Hashtbl.replace run.counts t.prefix count;
  let name = t.prefix ^ string_of_int count in
  add_release run t s ~label:("release of " ^ name);
  let invariant =
    Option.map
      (fun invariant () -> invariant (reference_side run.sides t step) s)
      t.invariant
  in
  run.pool <- { step; name; system = t.system.inject s; invariant } :: run.pool;
  name

let check_invariants run k =
  let check value invariant =
    match guard invariant with
    | Ok () -> ()
    | Error failure ->
        let label = Pp.str "after call %d of %d, on %s" k k value.name in
        raise (Failure.Check_failure (attribute label failure))
  in
  List.iter
    (fun value -> Option.iter (check value) value.invariant)
    (List.rev run.pool)

let pending ?before ~domain (Call c) (Ready r) =
  let text = lazy (String.concat " " (c.name :: List.map Lazy.force r.words)) in
  let row =
    {
      command = c.command;
      name = c.name;
      domain;
      before;
      call = text;
      judging =
        (match r.form with Judges _ -> true | Returns _ | Makes _ -> false);
      made = None;
      result = None;
    }
  in
  Pending
    {
      step = c.step;
      loc = c.loc;
      row;
      number = 0;
      form = r.form;
      reference = r.reference;
      system = r.system;
      ended = None;
    }

let add_row run (Pending p) =
  run.rows <- p.row :: run.rows;
  p.number <- List.length run.rows

let has_run (Pending p) = Option.is_some p.ended

(* Runs a pending call's system and records how it ended; [false] when it
   raised what is no outcome. Every control but a discard leaves. *)
let run_system (Pending p) =
  let ended = side p.system in
  p.ended <- Some ended;
  match ended with
  | Ok outcome ->
      p.row.result <- result_text p.form outcome;
      true
  | Error _ -> false

(* A system's never-outcome fails the run at its call. *)
let fail_never ~total (Pending p as pending) =
  match p.ended with
  | Some (Error never) ->
      let label = call_label "" ~total pending in
      raise
        (Failure.Check_failure
           (attribute ?loc:p.loc label (never_failure never)))
  | Some (Ok _) | None -> ()

let made run (Pending p) =
  match (p.form, p.ended) with
  | Makes t, Some (Ok (Returned s)) ->
      p.row.made <- Some (make run ~step:p.step t s)
  | (Makes _ | Returns _ | Judges _), _ -> ()

(* [judge_call ?order ~total sides pending] is the difference between the
   outcome of [pending]'s system and the one its reference gives on [sides]:
   [returns] compares, [makes] keeps the reference's side in [sides], and
   [judges] gives the reference the outcome to rule on, where a verb's
   failure or the system's own exception raised again is the system's
   mismatch. A broken reference is raised as [Property.Oracle_failure], its
   label naming [order], the order the judge of several domains replays. *)
let judge_call ?order ~total sides (Pending p as pending) =
  let outcome =
    match p.ended with
    | Some (Ok outcome) -> outcome
    | Some (Error _) | None -> assert false (* only an outcome is judged *)
  in
  let break what failure =
    let order = Option.map Lazy.force order in
    let label = call_label ?order what ~total pending in
    raise (Property.Oracle_failure (attribute ?loc:p.loc label failure))
  in
  let reference fn =
    match side fn with
    | Ok outcome -> outcome
    | Error never -> break "reference of " (never_failure never)
  in
  let fn = p.reference sides in
  match p.form with
  | Returns w -> differ w (reference fn) outcome
  | Makes t -> (
      match (reference fn, outcome) with
      | Returned r, Returned _ ->
          Hashtbl.replace sides p.step (t.reference.inject r);
          None
      | Raised (a, _), Raised (b, _) when same_constructor a b -> None
      | expected, _ -> Some (mismatch expected outcome))
  | Judges _ -> (
      match (side (fun () -> fn () (seen outcome)), outcome) with
      | Ok (Returned ()), _ -> None
      (* The system's own exception raised again rejects the outcome, as a
         reference that returned would; an exception built alike breaks the
         reference. *)
      | Ok (Raised (exn, _)), Raised (raised, _) when exn == raised ->
          Some (mismatch (Returned ()) outcome)
      | Ok (Raised (exn, backtrace)), _ ->
          break "reference of " (failure_of (`Exception (exn, backtrace)))
      | Error (Failed failure), _ -> Some failure
      | Error Discarded, _ -> break "reference of " (failure_of `Discard))

(* A call on one domain or in the prefix: the system runs, then the
   reference judges its outcome on the run's sides. The first failure ends
   the run, and the call that failed is the last row. *)
let run_call run (Call c as call) =
  match bind run call with
  | None -> ()
  | Some (Ready r as ready) -> (
      match match r.pre with None -> Ok true | Some pre -> guard pre with
      | Ok false -> ()
      | pre ->
          let before =
            match r.cells with
            | [] -> None
            | cells ->
                let cell =
                  String.concat ", " (List.map (fun f -> f ()) cells)
                in
                Some (Text.truncate_utf8 cell_chars cell)
          in
          let (Pending p as pending) =
            pending ?before ~domain:None call ready
          in
          add_row run pending;
          let label what = call_label what ~total:p.number pending in
          (match pre with
          | Error f ->
              raise
                (Property.Oracle_failure
                   (attribute ?loc:c.loc (label "~pre of ") f))
          | Ok _ -> ());
          ignore (run_system pending : bool);
          fail_never ~total:p.number pending;
          made run pending;
          run.ran <- pending :: run.ran;
          let fail f =
            raise (Failure.Check_failure (attribute ?loc:c.loc (label "") f))
          in
          Option.iter fail (judge_call ~total:p.number run.sides pending);
          check_invariants run p.number)

(* Every release runs, newest first. The first control among them wins,
   then the run's failure, then the first failure of a release. *)
let release_all run ending =
  let release ending (r : release) =
    match Failure.catch r.release with
    | Ok () -> ending
    | Error ((`Skip _ | `Timeout _ | `Exit) as c) -> (
        match ending with Error #Failure.control -> ending | _ -> Error c)
    | Error ((#Failure.fault | `Discard) as c) -> (
        match ending with
        | Ok () -> Error (`Assertion (attribute r.label (failure_of c)))
        | Error _ -> ending)
  in
  List.fold_left release ending run.releases

(* Judging *)

type verdict =
  | Explained of int list
  | Unexplained of { order : int list; at : int; failure : Failure.t }

(* [completed taken numbers] is [taken] followed by the calls of [numbers]
   that it does not hold, in their order. *)
let completed taken numbers =
  taken @ List.filter (fun number -> not (List.mem number taken)) numbers

(* Depth first over the orders that keep each branch's order, each followed
   by the suffix. The first child of a branch point continues on the state
   that the call before it left; every other starts from [fresh ()], replayed
   along its path, so a state need be neither persistent nor copyable.
   [path] is newest first. The closest order is the one whose first
   difference comes latest, the first found winning a tie; it is completed
   with the calls it did not reach, each branch in turn, then the suffix. *)
let judge ~fresh ~branches ~suffix =
  let calls = Array.of_list (List.map Array.of_list branches) in
  let cursors = Array.make (Array.length calls) 0 in
  let exhausted i = cursors.(i) = Array.length calls.(i) in
  let state = ref None in
  let at path =
    match !state with
    | Some s -> s
    | None ->
        let s = fresh () in
        List.iter (fun (_, call) -> ignore (call s)) (List.rev path);
        state := Some s;
        s
  in
  let closest = ref None in
  let differs path failure =
    state := None;
    let depth = List.length path in
    match !closest with
    | Some (d, _, _) when d >= depth -> ()
    | Some _ | None -> closest := Some (depth, path, failure)
  in
  let rec run_suffix path s = function
    | [] -> Some (List.rev_map fst path)
    | ((_, call) as step) :: rest -> (
        match call s with
        | None -> run_suffix (step :: path) s rest
        | Some failure ->
            differs (step :: path) failure;
            None)
  in
  let rec explore path =
    let rec from i =
      if i = Array.length calls then None
      else if exhausted i then from (i + 1)
      else
        let s = at path in
        let ((_, call) as step) = calls.(i).(cursors.(i)) in
        let found =
          match call s with
          | Some failure ->
              differs (step :: path) failure;
              None
          | None ->
              cursors.(i) <- cursors.(i) + 1;
              let found = explore (step :: path) in
              cursors.(i) <- cursors.(i) - 1;
              state := None;
              found
        in
        match found with Some _ -> found | None -> from (i + 1)
    in
    let rec all_exhausted i =
      i = Array.length calls || (exhausted i && all_exhausted (i + 1))
    in
    if all_exhausted 0 then run_suffix path (at path) suffix else from 0
  in
  match (explore [], !closest) with
  | Some order, _ -> Explained order
  | None, Some (_, ((at, _) :: _ as path), failure) ->
      let numbers = List.map fst (List.concat branches @ suffix) in
      Unexplained
        { order = completed (List.rev_map fst path) numbers; at; failure }
  | None, (Some (_, [], _) | None) ->
      assert false (* an order either ends or reaches a call that differs *)

(* Several domains *)

(* The parallel calls of [order], as [2, 4 then 3]. *)
let order_text ~parallel order =
  let calls = List.filter (fun n -> List.mem n parallel) order in
  match List.rev_map string_of_int calls with
  | [] -> ""
  | [ last ] -> last
  | last :: rest -> String.concat ", " (List.rev rest) ^ " then " ^ last

(* With no parallel call there is one order, and the failure reads as a
   call's. *)
let unexplained ~total ~parallel pendings ~order ~at (failure : Failure.t) =
  let (Pending p as pending) =
    List.find (fun (Pending p) -> p.number = at) pendings
  in
  if parallel = [] then
    attribute ?loc:p.loc (call_label "" ~total pending) failure
  else
    let head =
      Pp.str
        "no order of the calls gives these results\n\
         the closest order, %s, differs at call %d: %s"
        (order_text ~parallel order)
        at (Lazy.force p.row.call)
    in
    let msg =
      match failure.msg with
      | None -> head
      | Some msg -> head ^ "; " ^ one_line msg.kept
    in
    { failure with msg = Some (Failure.text msg) }

(* A replay gave a call of the prefix, which passed in the run, another
   outcome: the reference drifted, and the judge would blame the system. *)
let drifted () =
  Failure.message
    "a replay of the reference differs from this run; the reference must \
     behave the same from run to run"

let grace = function Failure.Control (`Timeout limit) -> limit | _ -> 0.

(* After the prefix: the branches, on the workers or, without them, one
   after the other, then the suffix's systems on this domain, then the
   judge. [stuck] is set when a call still runs after the grace, and the
   run of the suite then stops after this test (see [Run.stop]). *)
let run_parallel ~first ~workers ~stuck run program =
  let total () = List.length run.rows in
  let bind_call ~domain call =
    Option.map (pending ~domain call) (bind run call)
  in
  let branches =
    Array.mapi
      (fun i calls -> List.filter_map (bind_call ~domain:(Some (i + 1))) calls)
      program.branches
  in
  (* A branch stops at its first call that fails. What else a system
     raises, a control included, leaves the job; one after the other, the
     first raise ends the run, as a call does on one domain. *)
  let rec run_branch = function
    | [] -> ()
    | pending :: rest -> if run_system pending then run_branch rest
  in
  let jobs = Array.map (fun pendings () -> run_branch pendings) branches in
  let raised =
    match
      match workers with
      | None -> Array.iter (fun job -> job ()) jobs
      | Some workers -> Workers.run workers ~grace jobs
    with
    | () -> None
    | exception Workers.Stuck (exn, backtrace) ->
        stuck := true;
        Run.stop ();
        Printexc.raise_with_backtrace exn backtrace
    | exception exn -> Some (exn, Printexc.get_raw_backtrace ())
  in
  Array.iter (List.iter (fun p -> if has_run p then add_row run p)) branches;
  Option.iter
    (fun (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace)
    raised;
  Array.iter (List.iter (fail_never ~total:(total ()))) branches;
  (* A suffix call makes no value and has no [pre]. *)
  let run_suffix call =
    Option.map
      (fun pending ->
        add_row run pending;
        ignore (run_system pending : bool);
        fail_never ~total:(total ()) pending;
        pending)
      (bind_call ~domain:None call)
  in
  let suffix = List.filter_map run_suffix program.suffix in
  let branches = Array.to_list (Array.map (List.filter has_run) branches) in
  let pendings = List.concat branches @ suffix in
  let numbers = List.map (fun (Pending p) -> p.number) pendings in
  let parallel =
    List.map (fun (Pending p) -> p.number) (List.concat branches)
  in
  let total = total () in
  let prefix = List.rev run.ran in
  (* A state of the search is the reference's sides and the calls replayed
     on them, newest first. Every call of the prefix passed, so a replay
     that differs drifted. *)
  let fresh () =
    let sides = Hashtbl.create 8 in
    let replay (Pending p as pending) =
      if Option.is_some (judge_call ~total sides pending) then
        let label = call_label "reference of " ~total pending in
        raise
          (Property.Oracle_failure (attribute ?loc:p.loc label (drifted ())))
    in
    List.iter replay prefix;
    (sides, ref [])
  in
  (* A reference that breaks names the order being replayed, completed as
     the closest order is. *)
  let steps =
    List.map (fun (Pending p as pending) ->
        let step (sides, replayed) =
          replayed := p.number :: !replayed;
          let order =
            if parallel = [] then None
            else
              Some
                (lazy
                  (order_text ~parallel
                     (completed (List.rev !replayed) numbers)))
          in
          judge_call ?order ~total sides pending
        in
        (p.number, step))
  in
  let verdict =
    Run.without_labels (fun () ->
        judge ~fresh ~branches:(List.map steps branches) ~suffix:(steps suffix))
  in
  match verdict with
  | Explained order ->
      (* Labels count along the accepted order, once per case. *)
      if first then begin
        let sides, _ = Run.without_labels fresh in
        let replay number =
          let pending =
            List.find (fun (Pending p) -> p.number = number) pendings
          in
          ignore (judge_call ~total sides pending : Failure.t option)
        in
        List.iter replay order
      end
  | Unexplained { order; at; failure } ->
      raise
        (Failure.Check_failure
           (unexplained ~total ~parallel pendings ~order ~at failure))

(* One run from no value: the prefix as on one domain, invariants included,
   then, on several domains, the rest. A run whose call never returned
   releases nothing. A fatal exception leaves no record. *)
let run_once ~first ~workers program =
  program.record <- None;
  let run = fresh_run () in
  let stuck = ref false in
  let ending =
    Failure.catch (fun () ->
        List.iter (run_call run) program.prefix;
        if Array.length program.branches > 0 then
          run_parallel ~first ~workers ~stuck run program)
  in
  program.record <- Some (List.rev run.rows);
  match if !stuck then ending else release_all run ending with
  | Ok () -> ()
  | Error c -> Failure.reraise c

(* A repeat costs the system's calls and a judge, so it samples schedules
   for the price of a run. *)
let repetitions = 50

let execute ?workers program =
  let runs =
    if Array.length program.branches > 0 && Option.is_some workers then
      repetitions
    else 1
  in
  for i = 1 to runs do
    run_once ~first:(i = 1) ~workers program
  done

(* Declaring *)

let never_called ?loc ~cases names =
  Failure.message ?loc
    (Pp.str
       "never called: %s (over %d passing cases); a call runs only where its \
        arguments resolve and its ~pre holds"
       (String.concat ", " (List.map (Pp.str "%S") names))
       cases)

let mutating () =
  match (Run.config (Run.current ())).mutation with
  | Run.No_mutation -> false
  | Run.Loop _ | Run.Armed _ -> true

(* The workers live for the attempt: spawned before the first case, outside
   the property, and joined when the body ends, however it ends. Under
   mutation testing a program runs once on the test's domain, so a kill does
   not depend on a schedule and the process can still fork. *)
let with_workers ?loc ~domains fn =
  if domains = 1 || mutating () then fn None
  else
    match Workers.spawn domains with
    | exception Stdlib.Failure message ->
        raise
          (Failure.Check_failure
             (Failure.message ?loc ("cannot spawn a worker domain: " ^ message)))
    | workers -> (
        match fn (Some workers) with
        | () -> Workers.join workers
        | exception exn ->
            let backtrace = Printexc.get_raw_backtrace () in
            (* The body's exception wins over a handler's in the join. *)
            (try Workers.join workers with _ -> ());
            Printexc.raise_with_backtrace exn backtrace)

(* [Test_tree.Tag.prop] because a stateful test is a property: [--tag prop]
   selects it, and its report shows the root seed. The law returns only on a
   passing program, and [Run.property] returns only when every case passed, so
   [held] and [called] then mark the passing programs. On several domains the
   test takes no retries, since a failure there is never undone. *)
let stateful ?__POS__ ?tags ?timeout ?count ?(steps = 20) ?(domains = 1) name
    commands =
  let loc = Loc.resolve ?__POS__ () in
  let tags =
    Test_tree.Tag.prop :: "stateful"
    :: ((if domains > 1 then [ "parallel" ] else [])
       @ Option.value ~default:[] tags)
  in
  let retries = if domains > 1 then Some 0 else None in
  let gen = program ~steps ~domains commands in
  let body () =
    let commands = Array.of_list commands in
    check ~steps ~domains commands;
    let held = Array.make (Array.length commands) false in
    let called = Array.make (Array.length commands) false in
    let cases = ref 0 in
    with_workers ?loc ~domains (fun workers ->
        let cost = if Option.is_some workers then repetitions else 1 in
        Run.property ?loc ?count ~summary ~cost gen (fun program ->
            execute ?workers program;
            incr cases;
            Array.iteri (fun i h -> if h then held.(i) <- true) program.held;
            let ran (row : row) = called.(row.command) <- true in
            List.iter ran (Option.value ~default:[] program.record)));
    let firsts = first_positions commands in
    let never i _ = firsts.(i) = i && held.(i) && not called.(i) in
    match List.filteri never (Array.to_list commands) with
    | [] -> ()
    | _ when !cases = 0 -> ()
    | never ->
        let name (Command c) = c.name in
        raise
          (Failure.Check_failure
             (never_called ?loc ~cases:!cases (List.map name never)))
  in
  Test_tree.test ?__POS__ ~tags ?timeout ?retries name body
