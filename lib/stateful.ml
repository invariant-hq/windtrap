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
  | Chooses : 'a Testable.t -> (('a, exn) result -> 'a, 'a) form

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
let chooses w = Result (Chooses w)

(* Commands *)

(* The signature is existential, and it types the three functions. *)
type command =
  | Command : {
      name : string;
      loc : Loc.t option;
      fn : ('r, 's, 'p) fn;
      pre : 'p;
      reference : 'r;
      system : 's;
    }
      -> command

let rec always : type r s p. (r, s, p) fn -> p = function
  | Result _ -> true
  | Drawn (_, fn) -> fun _ -> always fn
  | Chosen (_, fn) -> fun _ -> always fn

let command ?__POS__ ?pre name fn reference system =
  let loc = Loc.resolve ?__POS__ () in
  let pre = match pre with Some pre -> pre | None -> always fn in
  Command { name = one_line name; loc; fn; pre; reference; system }

(* The abstract types of a command, for drawing and for the check of the
   prefixes: those it takes and the one it makes. *)
type any = Any : ('r, 's) abstract -> any
type shape = { takes : any list; makes : any option }

let type_id (Any t) = t.id

let rec shape : type r s p. (r, s, p) fn -> shape = function
  | Result (Makes t) -> { takes = []; makes = Some (Any t) }
  | Result (Returns _ | Chooses _) -> { takes = []; makes = None }
  | Drawn (_, fn) -> shape fn
  | Chosen (t, fn) ->
      let shape = shape fn in
      { shape with takes = Any t :: shape.takes }

let command_shape (Command c) = shape c.fn

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
let check ~steps commands =
  if Array.length commands = 0 then
    invalid_arg "Windtrap.stateful: no commands to draw from";
  if steps < 0 then invalid_arg "Windtrap.stateful: negative steps";
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
  check_all (List.concat_map types (Array.to_list commands))

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
      pre : 'p;
      reference : 'r;
      system : 's;
    }
      -> call

(* A call that ran. [made] is set when the call made a value. *)
type row = {
  command : int;
  name : string;
  before : string option; (* the [reference before] cell *)
  call : string Lazy.t; (* [name a1 … an] *)
  mutable made : string option;
}

type program = {
  calls : call list;
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
let pad width text = String.make (width - Text.length_utf8 text) ' '

let argument_text sample =
  let text = match Gen.Engine.render sample with Value t | Pre_image t -> t in
  let text = Text.truncate_bytes_utf8 argument_bytes (one_line text) in
  if String.contains text ' ' || String.starts_with ~prefix:"-" text then
    "(" ^ text ^ ")"
  else text

let row_text row =
  let call = Lazy.force row.call in
  match row.made with Some v -> "let " ^ v ^ " = " ^ call | None -> call

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
      let cells =
        List.exists (fun (_, row) -> Option.is_some row.before) (head @ tail)
      in
      let cell_width =
        List.fold_left
          (fun width (_, row) ->
            max width (Text.length_utf8 (Option.value row.before ~default:"")))
          0 (head @ tail)
      in
      let cell_width = max cell_width (String.length "reference before") in
      let number_width = max 2 (String.length (string_of_int total)) in
      let line number cell text =
        pad number_width number ^ number
        ^ (if cells then "  " ^ cell ^ pad cell_width cell else "")
        ^ "  " ^ text
      in
      let row (number, row) =
        line number (Option.value row.before ~default:"") (row_text row)
      in
      String.concat "\n"
        ((line "#" "reference before" "call" :: List.map row head)
        @ omission @ List.map row tail)

let pp_program ppf program =
  Pp.string ppf
    (match program.record with
    | None -> "(not run)"
    | Some rows -> record_text rows)

let summary program =
  match program.record with
  | None | Some [] -> None
  | Some rows ->
      let last = List.nth rows (List.length rows - 1) in
      Some (Pp.str "%s, last: %s" (call_count (List.length rows)) last.name)

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

(* Each command joins the subset with probability 1/2, every command when
   none does, and a command in the subset brings the makers of the types it
   takes. *)
let draw_subset firsts shapes state =
  let n = Array.length firsts in
  let joins = Array.make n false in
  let state = ref state in
  for i = 0 to n - 1 do
    if firsts.(i) = i then begin
      let coin, next = Seed.below ~bound:2L !state in
      joins.(i) <- Int64.equal coin 1L;
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

(* No reference runs here: a command is drawn when every type it takes has a
   value that an earlier call makes, if both of its sides return. Nothing
   repairs a candidate; [execute] skips what does not resolve. [makers]
   holds, per type, the steps of the calls that make it, newest first. *)
let program ?(steps = 20) commands =
  let commands = Array.of_list commands in
  let firsts = first_positions commands in
  let shapes = Array.map command_shape commands in
  let draw state =
    check ~steps commands;
    let held, state = draw_subset firsts shapes state in
    let made = Hashtbl.create 8 in
    let makers t = Option.value ~default:[] (Hashtbl.find_opt made t) in
    let drawable i =
      held.(i)
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
    let rec draw_calls n trees state =
      match if n = 0 then [] else List.filter drawable positions with
      | [] -> (List.rev trees, state)
      | drawable ->
          let bound = Int64.of_int (List.length drawable) in
          let pick, state = Seed.below ~bound state in
          let i = List.nth drawable (Int64.to_int pick) in
          let step = List.length trees in
          let tree, state = draw_call step i state in
          let make t =
            Hashtbl.replace made (type_id t) (step :: makers (type_id t))
          in
          Option.iter make shapes.(i).makes;
          draw_calls (n - 1) (tree :: trees) state
    in
    let trees, state = draw_calls steps [] state in
    let program calls = { calls; held; record = None } in
    (Shrink_tree.map program (Shrink_tree.list trees), state)
  in
  Gen.Engine.make ~pp:pp_program draw

(* Running *)

(* A value that a system made. Its reference side is the run's, under the
   step of the call that made it. *)
type value = {
  step : int; (* the step of the call that made it *)
  name : string;
  system : exn;
  invariant : (unit -> unit) option;
}

type release = { label : string; release : unit -> unit }

type run = {
  mutable pool : value list; (* newest first *)
  references : (int, exn) Hashtbl.t; (* the reference sides, by step *)
  counts : (string, int) Hashtbl.t; (* the values made, per prefix *)
  mutable releases : release list; (* newest first *)
  mutable rows : row list; (* newest first *)
}

(* [reference_side run t step] is the reference side of the value that the
   call at [step] made. A value joins the pool when its system returns, and
   its call ends the run unless the reference returned a side too, so every
   value that a later call or an invariant reads has one. *)
let reference_side run (t : (_, _) abstract) step =
  match
    Option.bind (Hashtbl.find_opt run.references step) t.reference.project
  with
  | Some r -> r
  | None -> assert false

(* [resolve run t maker] is the value of [t] that the call at step [maker]
   made, or when it made none, the newest value of [t], with its system
   side. *)
let resolve run (t : (_, _) abstract) maker =
  let of_type value =
    Option.map (fun s -> (value, s)) (t.system.project value.system)
  in
  match List.find_opt (fun value -> value.step = maker) run.pool with
  | Some value -> of_type value
  | None -> List.find_map of_type run.pool

(* A call whose arguments resolved. Its functions apply their arguments
   when called, so that no code of the user runs before [pre] holds. *)
type ready =
  | Ready : {
      form : ('r, 's) form;
      pre : unit -> bool;
      reference : unit -> 'r;
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
      (unit -> p) ->
      (unit -> r) ->
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
        apply args
          (fun () -> pre () v)
          (fun () -> reference () v)
          (fun () -> system () v)
          (lazy (argument_text sample) :: words)
          cells
    | Index (t, maker, args) -> (
        match resolve run t maker with
        | None -> None
        | Some (value, s) ->
            let r = reference_side run t value.step in
            let cells =
              match t.pp with
              | None -> cells
              | Some pp -> (fun () -> reference_cell pp r) :: cells
            in
            apply args
              (fun () -> pre () r)
              (fun () -> reference () r)
              (fun () -> system () s)
              (Lazy.from_val value.name :: words)
              cells)
  in
  apply c.args
    (fun () -> c.pre)
    (fun () -> c.reference)
    (fun () -> c.system)
    [] []

(* Outcomes *)

type 'a outcome = Returned of 'a | Raised of exn * Printexc.raw_backtrace

(* What is never an outcome: a verb's failure or a broken contract, as its
   failure, and a discard. *)
type never = Failed of Failure.t | Discarded

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
let make run ~step t s =
  let count =
    1 + Option.value ~default:0 (Hashtbl.find_opt run.counts t.prefix)
  in
  Hashtbl.replace run.counts t.prefix count;
  let name = t.prefix ^ string_of_int count in
  add_release run t s ~label:("release of " ^ name);
  let invariant =
    Option.map
      (fun invariant () -> invariant (reference_side run t step) s)
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

(* The system runs, then the reference judges its outcome: [returns]
   compares, [makes] keeps the reference's side of the value, and [chooses]
   gives the reference the outcome to accept. The first failure ends the run,
   and the call that failed is the last row. A broken reference is raised as
   [Property.Oracle_failure]. *)
let run_call run (Call c as call) =
  match bind run call with
  | None -> ()
  | Some (Ready r) -> (
      match guard r.pre with
      | Ok false -> ()
      | pre ->
          let k = List.length run.rows + 1 in
          let text =
            lazy (String.concat " " (c.name :: List.map Lazy.force r.words))
          in
          let before =
            match r.cells with
            | [] -> None
            | cells ->
                let cell =
                  String.concat ", " (List.map (fun f -> f ()) cells)
                in
                Some (Text.truncate_utf8 cell_chars cell)
          in
          let row =
            {
              command = c.command;
              name = c.name;
              before;
              call = text;
              made = None;
            }
          in
          run.rows <- row :: run.rows;
          let label what =
            Pp.str "%scall %d of %d: %s" what k k (Lazy.force text)
          in
          let fail f =
            raise (Failure.Check_failure (attribute ?loc:c.loc (label "") f))
          in
          let break what f =
            raise
              (Property.Oracle_failure (attribute ?loc:c.loc (label what) f))
          in
          (match pre with Error f -> break "~pre of " f | Ok _ -> ());
          let actual =
            match side r.system with
            | Ok outcome -> outcome
            | Error never -> fail (never_failure never)
          in
          let reference fn =
            match side fn with
            | Ok outcome -> outcome
            | Error never -> break "reference of " (never_failure never)
          in
          (match r.form with
          | Returns w ->
              Option.iter fail (differ w (reference r.reference) actual)
          | Makes t -> (
              (match actual with
              | Returned s -> row.made <- Some (make run ~step:c.step t s)
              | Raised _ -> ());
              match (reference r.reference, actual) with
              | Returned rv, Returned _ ->
                  Hashtbl.replace run.references c.step (t.reference.inject rv)
              | Raised (a, _), Raised (b, _) when same_constructor a b -> ()
              | expected, _ -> fail (mismatch expected actual))
          | Chooses w -> (
              let seen =
                match actual with
                | Returned v -> Ok v
                | Raised (exn, _) -> Error exn
              in
              match side (fun () -> r.reference () seen) with
              | Ok expected -> Option.iter fail (differ w expected actual)
              | Error (Failed f) -> fail f
              | Error Discarded -> break "reference of " (failure_of `Discard)));
          check_invariants run k)

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

(* A fatal exception leaves no record, so the record is cleared first. *)
let execute program =
  program.record <- None;
  let run =
    {
      pool = [];
      references = Hashtbl.create 8;
      counts = Hashtbl.create 8;
      releases = [];
      rows = [];
    }
  in
  let ending =
    Failure.catch (fun () -> List.iter (run_call run) program.calls)
  in
  program.record <- Some (List.rev run.rows);
  match release_all run ending with Ok () -> () | Error c -> Failure.reraise c

(* Declaring *)

let never_called ?loc ~cases names =
  Failure.message ?loc
    (Pp.str
       "never called: %s (over %d passing cases); a call runs only where its \
        arguments resolve and its ~pre holds"
       (String.concat ", " (List.map (Pp.str "%S") names))
       cases)

(* [Test_tree.Tag.prop] because a stateful test is a property: [--tag prop]
   selects it, and its report shows the root seed. The law returns only on a
   passing program, and [Run.property] returns only when every case passed, so
   [held] and [called] then mark the passing programs. *)
let stateful ?__POS__ ?tags ?timeout ?count ?(steps = 20) name commands =
  let loc = Loc.resolve ?__POS__ () in
  let tags =
    Test_tree.Tag.prop :: "stateful" :: Option.value ~default:[] tags
  in
  let gen = program ~steps commands in
  let body () =
    let commands = Array.of_list commands in
    check ~steps commands;
    let held = Array.make (Array.length commands) false in
    let called = Array.make (Array.length commands) false in
    let cases = ref 0 in
    Run.property ?loc ?count ~summary gen (fun program ->
        execute program;
        incr cases;
        Array.iteri (fun i h -> if h then held.(i) <- true) program.held;
        let ran (row : row) = called.(row.command) <- true in
        List.iter ran (Option.value ~default:[] program.record));
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
  Test_tree.test ?__POS__ ~tags ?timeout name body
