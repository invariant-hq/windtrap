(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

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
type ('r, 's) values = {
  id : int;
  prefix : string;
  pp : (Format.formatter -> 'r -> unit) option;
  invariant : ('r -> 's -> unit) option;
  release : ('s -> unit) option;
  reference : 'r key;
  system : 's key;
}

(* A type of values, which calls make, or of the elements that a value of
   [listed] lists, which a call takes from its own argument of that type. An
   element is the same on both sides; [key] carries it to an element listed
   in turn. *)
type ('r, 's) abstract =
  | Values : ('r, 's) values -> ('r, 's) abstract
  | Among : 'a among -> ('a, 'a) abstract

and 'a among =
  | Listing : {
      id : int;
      witness : 'a Testable.t;
      listed : ('r, 's) abstract;
      candidates : 'r -> 'a list;
      key : 'a key;
    }
      -> 'a among

let next_id = Atomic.make 0

let abstract ?pp ?invariant ?release prefix =
  Values
    {
      id = Atomic.fetch_and_add next_id 1;
      prefix;
      pp;
      invariant;
      release;
      reference = key ();
      system = key ();
    }

let among witness listed candidates =
  let id = Atomic.fetch_and_add next_id 1 in
  Among (Listing { id; witness; listed; candidates; key = key () })

let reference_key : type r s. (r, s) abstract -> r key = function
  | Values t -> t.reference
  | Among (Listing l) -> l.key

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
   prefixes: the types of the values it takes, the one it makes, and whether
   it takes an element, which one of those values lists. *)
type any = Any : ('r, 's) abstract -> any
type shape = { takes : any list; makes : any option; takes_element : bool }

let type_id (Any t) =
  match t with Values t -> t.id | Among (Listing l) -> l.id

let rec shape : type r s p. (r, s, p) fn -> shape = function
  | Result (Makes t) ->
      { takes = []; makes = Some (Any t); takes_element = false }
  | Result (Returns _ | Judges _) ->
      { takes = []; makes = None; takes_element = false }
  | Drawn (_, fn) -> shape fn
  | Chosen ((Values _ as t), fn) ->
      let shape = shape fn in
      { shape with takes = Any t :: shape.takes }
  | Chosen (Among _, fn) -> { (shape fn) with takes_element = true }

let command_shape (Command c) = shape c.fn

(* On several domains only the prefix has one reference state, so only there
   can a value be named, a [pre] be asked or a value's elements be listed:
   after it, a command runs only when it makes no value, has no [pre] and
   takes no element. *)
let runs_anywhere (Command c as command) =
  let shape = command_shape command in
  Option.is_none c.pre && Option.is_none shape.makes && not shape.takes_element

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

(* How a message names an argument of type [t]. *)
let rec argument_name (Any t) =
  match t with
  | Values t -> Pp.str "a value of '%s'" t.prefix
  | Among (Listing { listed = Values t; _ }) ->
      Pp.str "an element of '%s'" t.prefix
  | Among (Listing { listed = Among _ as listed; _ }) ->
      "an element of " ^ argument_name (Any listed)

(* The first rule of elements that the command breaks, as a message: an
   element is never made, and the call takes a value of the type that lists
   it. *)
let element_error (Command c) =
  let rec taken : type r s p. (r, s, p) fn -> int list = function
    | Result _ -> []
    | Drawn (_, fn) -> taken fn
    | Chosen (t, fn) -> type_id (Any t) :: taken fn
  in
  let taken = taken c.fn in
  let rec walk : type r s p. (r, s, p) fn -> string option = function
    | Result (Makes (Among _ as t)) ->
        Some
          (Pp.str
             "Windtrap.stateful: %s makes %s; an element is listed by a value, \
              never made"
             c.name (argument_name (Any t)))
    | Result (Returns _ | Makes (Values _) | Judges _) -> None
    | Drawn (_, fn) -> walk fn
    | Chosen (Values _, fn) -> walk fn
    | Chosen ((Among (Listing l) as t), fn) ->
        if List.mem (type_id (Any l.listed)) taken then walk fn
        else
          Some
            (Pp.str
               "Windtrap.stateful: %s takes %s without %s; an element is \
                listed by a value its call takes"
               c.name (argument_name (Any t))
               (argument_name (Any l.listed)))
  in
  walk c.fn

(* The types and the elements' errors are gathered first, so that no frame
   of the standard library stands between a raise and the test in the
   printed backtrace. *)
let check ~steps ~domains commands =
  if Array.length commands = 0 then
    invalid_arg "Windtrap.stateful: no commands to draw from";
  if steps < 0 then invalid_arg "Windtrap.stateful: negative steps";
  if domains < 1 then invalid_arg "Windtrap.stateful: domains below 1";
  let types command =
    let { takes; makes; takes_element = _ } = command_shape command in
    takes @ Option.to_list makes
  in
  let owners = Hashtbl.create 8 in
  let rec check_all = function
    | [] -> ()
    (* An element has no prefix, and no command makes one. *)
    | Any (Among _) :: types -> check_all types
    | Any (Values t) :: types ->
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
  (match List.find_map element_error (Array.to_list commands) with
  | Some message -> invalid_arg message
  | None -> ());
  if domains > 1 && not (Array.exists runs_anywhere commands) then
    invalid_arg
      "Windtrap.stateful: on several domains every command makes a value, has \
       a ~pre or takes an element, so no call can run after the prefix"

(* Programs *)

(* A parsed call's signature with every argument filled: a sample, the step
   of the call that makes the value it takes, [-1] when its type has no
   maker, or an element's place. A place is a position [k] below
   [2^position_bits], which takes the candidate at [k * n / 2^position_bits]
   of the [n] its value lists, so a deletion of an earlier call, which
   changes [n], keeps its relative place. *)
type ('r, 's, 'p) args =
  | Last : ('r, 's) form -> ('r, 's, bool) args
  | Sample :
      'a Gen.Engine.sample * ('r, 's, 'p) args
      -> ('a -> 'r, 'a -> 's, 'a -> 'p) args
  | Index :
      ('ra, 'sa) values * int * ('r, 's, 'p) args
      -> ('ra -> 'r, 'sa -> 's, 'ra -> 'p) args
  | Element :
      'a among * int * ('r, 's, 'p) args
      -> ('a -> 'r, 'a -> 's, 'a -> 'p) args

type call =
  | Call : {
      step : int; (* the call's position in the parse *)
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
}

(* The rows of a run, in order. *)
type record = row list

(* Printing *)

(* An argument rides the failure payload, which is capped at 64 KiB, so it
   is bounded in bytes; a cell is a column, so it is bounded in code points.
   A longer record prints its first and last [context_calls]. *)
let argument_bytes = 200
let cell_chars = 60
let context_calls = 20
let call_count n = Pp.str "%d call%s" n (if n = 1 then "" else "s")

let argument_word text =
  let text = Text.truncate_bytes_utf8 argument_bytes (one_line text) in
  if String.contains text ' ' || String.starts_with ~prefix:"-" text then
    "(" ^ text ^ ")"
  else text

let argument_text sample =
  argument_word
    (match Gen.Engine.render sample with Value t | Pre_image t -> t)

(* An element prints through its witness, and as [_] when the call failed
   before taking it. It prints only when the call's row is printed or named,
   so whatever the printer raises then is the text. *)
let element_text witness = function
  | None -> "_"
  | Some v ->
      argument_word
        (match Failure.catch (fun () -> Testable.to_string witness v) with
        | Ok text -> text
        | Error c -> Pp.str "<pp raised %s>" (Failure.caught_to_string c))

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
   hints. The table is the report's, laid out unstyled and its trailing
   spaces stripped as a report strips a table's. *)
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
      let columns =
        columns
          ~parallel:(List.exists is_parallel rows)
          ~judging:(List.exists (fun row -> row.judging) rows)
          (List.map snd (head @ tail))
      in
      let line number cells =
        List.map Report_sections.plain (number :: cells)
      in
      let lines =
        List.map
          (fun spans ->
            let text = Report_sections.render ~ansi:false spans in
            let rec stop i =
              if i > 0 && text.[i - 1] = ' ' then stop (i - 1) else i
            in
            String.sub text 0 (stop (String.length text)))
          (Report_sections.table ~margin:""
             ~columns:
               ({ Report_sections.gap = ""; align = `Right; width = Some 2 }
               :: List.map
                    (fun _ ->
                      {
                        Report_sections.gap = "  ";
                        align = `Left;
                        width = None;
                      })
                    columns)
             (line "#" (List.map fst columns)
             :: List.map
                  (fun (number, row) ->
                    line number (List.map (fun (_, cell) -> cell row) columns))
                  (head @ tail)))
      in
      let above = List.filteri (fun i _ -> i <= List.length head) lines in
      let below = List.filteri (fun i _ -> i > List.length head) lines in
      String.concat "\n" (above @ omission @ below)

let pp_record ppf record = Pp.string ppf (record_text record)

let summary = function
  | [] -> None
  | rows -> (
      let total = call_count (List.length rows) in
      match List.length (List.filter is_parallel rows) with
      | 0 ->
          let last = List.nth rows (List.length rows - 1) in
          Some (Pp.str "%s, last: %s" total last.name)
      | parallel -> Some (Pp.str "%s, %d in parallel" total parallel))

(* A program prints as it was drawn, one call a line, a parallel call after
   its branch: what a replay token's digest is of. A value is named as its
   maker would name it if every call ran. *)
let pp_program ppf program =
  let names = Hashtbl.create 8 and counts = Hashtbl.create 8 in
  let name ~step prefix =
    let count = 1 + Option.value ~default:0 (Hashtbl.find_opt counts prefix) in
    Hashtbl.replace counts prefix count;
    let name = prefix ^ string_of_int count in
    Hashtbl.replace names step name;
    name
  in
  let rec words : type r s p. (r, s, p) args -> string list = function
    | Last _ -> []
    | Sample (sample, args) -> argument_text sample :: words args
    | Index (_, maker, args) ->
        Option.value ~default:"_" (Hashtbl.find_opt names maker) :: words args
    | Element (_, place, args) -> Pp.str "(at %d)" place :: words args
  in
  let rec made : type r s p. (r, s, p) args -> string option = function
    | Last (Makes (Values t)) -> Some t.prefix
    | Last (Makes (Among _) | Returns _ | Judges _) -> None
    | Sample (_, args) -> made args
    | Index (_, _, args) -> made args
    | Element (_, _, args) -> made args
  in
  let line ?domain (Call c) =
    let call = String.concat " " (c.name :: words c.args) in
    let call =
      match made c.args with
      | None -> call
      | Some prefix -> Pp.str "let %s = %s" (name ~step:c.step prefix) call
    in
    match domain with None -> call | Some i -> Pp.str "%d: %s" i call
  in
  let lines =
    List.map line program.prefix
    @ List.concat
        (List.mapi
           (fun i calls -> List.map (line ~domain:(i + 1)) calls)
           (Array.to_list program.branches))
    @ List.map line program.suffix
  in
  Pp.string ppf
    (match lines with [] -> "(no calls)" | _ -> String.concat "\n" lines)

(* Drawing *)

(* A program parses a choice sequence (see [Choice]), which a generating
   source draws as 0.2 drew a program, from the same stream. *)

(* What a generating [src] draws: [v]. *)
let drawn src v = Choice.sample src (fun state -> (v, state))

(* The newest value with probability 1/2, else any of the [count]. *)
let draw_index count state =
  let newest, state = Seed.below ~bound:2L state in
  if Int64.equal newest 0L then (0L, state)
  else Seed.below ~bound:(Int64.of_int count) state

let position_bits = 30

(* Arguments are counted from one, abstract ones included. A value is the
   [k]th newest of the makers of its type, [makers] holding the step and the
   mark of each, newest first: the choice counts past the marks of the [k]
   newer makers, so a deletion of some of them lowers it with them, and it
   keeps its value. An element is one position. *)
let rec parse_args : type r s p.
    name:string ->
    makers:(int -> (int * int) list) ->
    int ->
    (r, s, p) fn ->
    Choice.t ->
    (r, s, p) args =
 fun ~name ~makers position fn src ->
  match fn with
  | Result form -> Last form
  | Drawn (gen, fn) ->
      let sample = Gen.Engine.parse gen src in
      if not (Gen.Engine.prints sample) then
        invalid_arg
          (Pp.str "%s: argument %d has no printer; attach one with Gen.with_pp"
             name position);
      Sample (sample, parse_args ~name ~makers (position + 1) fn src)
  | Chosen (Values t, fn) ->
      let made = makers t.id in
      let count = List.length made in
      let at = Choice.position src in
      let bound = Int64.of_int (Int.max 0 (count - 1)) in
      let k =
        Int64.to_int
          (Choice.choice src ~bound (Choice.sample src (draw_index count)))
      in
      Choice.counts_past src ~at
        (List.filteri (fun i _ -> i < k) (List.map snd made));
      let maker =
        match List.nth_opt made k with Some (s, _) -> s | None -> -1
      in
      Index (t, maker, parse_args ~name ~makers (position + 1) fn src)
  | Chosen (Among l, fn) ->
      let places = Int64.shift_left 1L position_bits in
      let place =
        Int64.to_int
          (Choice.choice src ~bound:(Int64.pred places)
             (Choice.sample src (Seed.below ~bound:places)))
      in
      Element (l, place, parse_args ~name ~makers (position + 1) fn src)

(* Each command joins the subset with probability 3/4, every command when
   none does, and a command in the subset brings the makers of the types it
   takes. The subset only steers drawing, so it is drawn unrecorded: a replay
   holds every command, and a command appended to the list moves no
   choice. *)
let draw_subset firsts shapes src =
  let n = Array.length firsts in
  let joins =
    Array.mapi
      (fun i first ->
        first = i
        &&
        match Choice.sample src (Seed.below ~bound:4L) with
        | Some quarter -> not (Int64.equal quarter 0L)
        | None -> true)
      firsts
  in
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
  held

(* The calls of each branch: the most, at most ten in all, whose orders stay
   within 7! = 5040, and never fewer than one. Two domains take five calls
   each (252 orders), three take three (1680), four take two (2520), five to
   seven take one; from eight domains one call each gives n! orders. *)
let branch_calls = function 2 -> 5 | 3 -> 3 | 4 -> 2 | _ -> 1
let discard () = raise (Failure.Control `Discard)

(* No reference runs here: a command is drawn when every type it takes has a
   value that an earlier call makes. Nothing repairs a parse; [execute] skips
   what does not resolve.

   Each call is a span of a continue bit, the command's position in the list
   and its arguments, so deleting the span deletes the call, and lowering the
   position turns the command into an earlier one. A maker's mark is its
   position's choice. On several domains a call after the prefix reads its
   segment first: [0] ends the prefix, [1] starts the suffix and [i + 2] is
   branch [i], so moving a call out of its branch lowers a choice. A command
   that cannot run after the prefix, or one more call than its branch holds,
   discards the parse. *)
let program ?(steps = 20) ?(domains = 1) commands =
  let commands = Array.of_list commands in
  let firsts = first_positions commands in
  let shapes = Array.map command_shape commands in
  let anywhere = Array.map runs_anywhere commands in
  let positions = List.init (Array.length commands) Fun.id in
  let last = Int64.of_int (Array.length commands - 1) in
  let parse src =
    check ~steps ~domains commands;
    let held = draw_subset firsts shapes src in
    let made = Hashtbl.create 8 in
    let makers t = Option.value ~default:[] (Hashtbl.find_opt made t) in
    let drawable ~prefix =
      List.filter
        (fun i ->
          held.(i)
          && (prefix || anywhere.(i))
          && List.for_all (fun t -> makers (type_id t) <> []) shapes.(i).takes)
        positions
    in
    let pick drawable state =
      let k, state =
        Seed.below ~bound:(Int64.of_int (List.length drawable)) state
      in
      (Int64.of_int (List.nth drawable (Int64.to_int k)), state)
    in
    let next_step = ref 0 in
    let parse_call ~prefix =
      let i =
        Int64.to_int
          (Choice.choice src ~bound:last
             (Choice.sample src (pick (drawable ~prefix))))
      in
      let mark = Choice.position src - 1 in
      match commands.(i) with
      | Command { name; loc; fn; pre; reference; system } ->
          let args = parse_args ~name ~makers 1 fn src in
          let step = !next_step in
          incr next_step;
          let make t =
            Hashtbl.replace made (type_id t) ((step, mark) :: makers (type_id t))
          in
          Option.iter make shapes.(i).makes;
          let command = firsts.(i) in
          (i, Call { step; command; name; loc; args; pre; reference; system })
    in
    (* At most [cap] calls, a generating source drawing one more while
       [wanted] holds of the count so far. *)
    let rec calls ~cap ~wanted parse_call count acc =
      let next =
        if count = cap then None
        else
          Choice.span src (fun () ->
              let more = drawn src (if wanted count then 1L else 0L) in
              if Int64.equal (Choice.choice src ~bound:1L more) 0L then None
              else Some (parse_call count))
      in
      match next with
      | None -> List.rev acc
      | Some call -> calls ~cap ~wanted parse_call (count + 1) (call :: acc)
    in
    let in_prefix _ = snd (parse_call ~prefix:true) in
    let any ~prefix _ = drawable ~prefix <> [] in
    if domains = 1 then
      let prefix = calls ~cap:steps ~wanted:(any ~prefix:true) in_prefix 0 [] in
      { prefix; branches = [||]; suffix = []; held }
    else
      let below n =
        let draw state =
          let k, state = Seed.below ~bound:(Int64.of_int (n + 1)) state in
          (Int64.to_int k, state)
        in
        Option.value ~default:0 (Choice.sample src draw)
      in
      let prefix_calls = below steps in
      let suffix_calls = below (steps - prefix_calls) in
      let prefix =
        calls ~cap:steps
          ~wanted:(fun count -> count < prefix_calls && any ~prefix:true count)
          in_prefix 0 []
      in
      let per = branch_calls domains in
      let ended = ref [] and suffix = ref [] in
      let branches = Array.make domains [] in
      (* A generating source fills each branch in turn, then the suffix. *)
      let after_prefix count =
        let drawn_segment =
          if count < domains * per then 2 + (count / per) else 1
        in
        let segment =
          Choice.choice src
            ~bound:(Int64.of_int (domains + 1))
            (drawn src (Int64.of_int drawn_segment))
        in
        let i, call = parse_call ~prefix:false in
        if not anywhere.(i) then discard ();
        match Int64.to_int segment with
        | 0 -> ended := call :: !ended
        | 1 -> suffix := call :: !suffix
        | segment ->
            let b = segment - 2 in
            if List.length branches.(b) = per then discard ();
            branches.(b) <- call :: branches.(b)
      in
      let wanted count =
        count < (domains * per) + suffix_calls && any ~prefix:false count
      in
      ignore
        (calls ~cap:((domains * per) + steps) ~wanted after_prefix 0 []
          : unit list);
      {
        prefix = prefix @ List.rev !ended;
        branches = Array.map List.rev branches;
        suffix = List.rev !suffix;
        held;
      }
  in
  Gen.Engine.make ~pp:pp_program parse

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
   ran, an outcome or what is none, and when it began and returned. [number]
   is its row's number once the row is recorded. *)
(* When a call's system began or returned: the monotonic clock, read on the
   domain that runs it, and that domain's own count of its readings. Two
   readings of one domain are ordered by the count, which a coarse clock
   cannot tie (macos-latest's gave two calls run one after the other the
   same instant); readings of two domains, by the clock alone, since taking
   anything shared would synchronise the domains under test. *)
type reading = { clock : Os.counter; domain : int; count : int }
type stamp = { began : reading; returned : reading }

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
      mutable ran : stamp option;
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
let reference_side sides (t : (_, _) values) step =
  match Option.bind (Hashtbl.find_opt sides step) t.reference.project with
  | Some r -> r
  | None -> assert false

(* [resolve pool t maker] is the value of [t] that the call at step [maker]
   made, or when it made none, the newest value of [t], with its system
   side. *)
let resolve pool (t : (_, _) values) maker =
  let of_type value =
    Option.map (fun s -> (value, s)) (t.system.project value.system)
  in
  match List.find_opt (fun value -> value.step = maker) pool with
  | Some value -> of_type value
  | None -> List.find_map of_type pool

(* How a call's elements fared: every one taken, or not, since a value
   listed no element. *)
type taking = Taken | Unlisted

(* A call whose values resolved. Its functions apply their arguments when
   called, so that no code of the user runs before [pre] holds, and they
   read its elements, so they run once [take] has taken every one. The
   reference reads its arguments' sides from the [sides] it is given, so
   the judge replays it on sides of its own; [take], [pre] and the cells
   read the run's. *)
type ready =
  | Ready : {
      form : ('r, 's) form;
      take : unit -> taking;
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

(* [bind run call] is [call] with its values resolved among the values of
   [run], and [None] when one does not resolve. An element is taken from
   the call's nearest argument of the type that lists it before it, else
   the first after it, which may be an element taken first. *)
let bind run (Call c) =
  (* The call's values and elements, last first: the argument's position,
     its type's id and its reference side, or why an element has none. *)
  let arguments = ref [] in
  (* The side of type [id] that the element at [position] reads: the nearest
     before it, else the first after it. *)
  let read ~position id =
    let of_type = List.filter (fun (_, id', _) -> id' = id) !arguments in
    let earlier, later =
      List.partition (fun (p, _, _) -> p < position) of_type
    in
    match (earlier, List.rev later) with
    | (_, _, side) :: _, _ | [], (_, _, side) :: _ -> side ()
    | [], [] -> assert false (* [check] gives an element a value to list it *)
  in
  let rec apply : type r s p.
      int ->
      (r, s, p) args ->
      (unit -> taking) ->
      (unit -> p) option ->
      (sides -> unit -> r) ->
      (unit -> s) ->
      string Lazy.t list ->
      (unit -> string) list ->
      ready option =
   fun position args take pre reference system words cells ->
    match args with
    | Last form ->
        let words = List.rev words and cells = List.rev cells in
        Some (Ready { form; take; pre; reference; system; words; cells })
    | Sample (sample, args) ->
        let v = Gen.Engine.value sample in
        let reference sides =
          let f = reference sides in
          fun () -> f () v
        in
        apply (position + 1) args take
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
            arguments :=
              (position, t.id, fun () -> Ok (t.reference.inject r))
              :: !arguments;
            let reference sides =
              let f = reference sides in
              fun () -> f () (reference_side sides t value.step)
            in
            let cells =
              match t.pp with
              | None -> cells
              | Some pp -> (fun () -> reference_cell pp r) :: cells
            in
            apply (position + 1) args take
              (Option.map (fun pre () -> pre () r) pre)
              reference
              (fun () -> system () s)
              (Lazy.from_val value.name :: words)
              cells)
    | Element (Listing l, place, args) ->
        let element = ref None in
        let take_element () =
          match !element with
          | Some taken -> taken
          | None ->
              let taken =
                match read ~position (type_id (Any l.listed)) with
                | Error untaken -> Error untaken
                | Ok side -> (
                    let r =
                      match (reference_key l.listed).project side with
                      | Some r -> r
                      | None -> assert false (* a side of [listed]'s key *)
                    in
                    match l.candidates r with
                    | [] -> Error Unlisted
                    | candidates ->
                        let n = List.length candidates in
                        Ok (List.nth candidates ((place * n) lsr position_bits))
                    )
              in
              element := Some taken;
              taken
        in
        arguments :=
          (position, l.id, fun () -> Result.map l.key.inject (take_element ()))
          :: !arguments;
        let take () =
          match take () with
          | Taken -> (
              match take_element () with
              | Ok _ -> Taken
              | Error untaken -> untaken)
          | untaken -> untaken
        in
        let taken () =
          match !element with
          | Some (Ok x) -> x
          | Some (Error _) | None -> assert false (* the call runs once taken *)
        in
        let reference sides =
          let f = reference sides in
          fun () -> f () (taken ())
        in
        let printed () =
          match !element with
          | Some (Ok x) -> Some x
          | Some (Error _) | None -> None
        in
        apply (position + 1) args take
          (Option.map (fun pre () -> pre () (taken ())) pre)
          reference
          (fun () -> system () (taken ()))
          (lazy (element_text l.witness (printed ())) :: words)
          cells
  in
  apply 1 c.args
    (fun () -> Taken)
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
let add_release run (t : (_, _) values) s ~label =
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
let make run ~step (t : (_, _) values) s =
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
      ran = None;
    }

let add_row run (Pending p) =
  run.rows <- p.row :: run.rows;
  p.number <- List.length run.rows

let has_run (Pending p) = Option.is_some p.ended

(* The domain's own count of the readings it took, which no other domain
   reads or writes. *)
let readings = Domain.DLS.new_key (fun () -> ref 0)

let reading () =
  let count = Domain.DLS.get readings in
  incr count;
  { clock = Os.counter (); domain = (Domain.self () :> int); count = !count }

(* Runs a pending call's system and records how it ended and when; [false]
   when it raised what is no outcome. Every control but a discard leaves. *)
let run_system (Pending p) =
  let began = reading () in
  let ended = side p.system in
  p.ran <- Some { began; returned = reading () };
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
  | Makes (Values t), Some (Ok (Returned s)) ->
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
          Hashtbl.replace sides p.step ((reference_key t).inject r);
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

(* A call on one domain or in the prefix: its elements are taken and its
   [pre] asked, the system runs, then the reference judges its outcome on
   the run's sides. The first failure ends the run, and the call that failed
   is the last row. *)
let run_call run (Call c as call) =
  match bind run call with
  | None -> ()
  | Some (Ready r as ready) -> (
      let legal =
        match guard r.take with
        | Ok Unlisted -> Ok false
        | Error f -> Error ("reference of ", f)
        | Ok Taken -> (
            match r.pre with
            | None -> Ok true
            | Some pre ->
                Result.map_error (fun f -> ("~pre of ", f)) (guard pre))
      in
      match legal with
      | Ok false -> ()
      | legal ->
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
          (match legal with
          | Error (what, f) ->
              raise
                (Property.Oracle_failure (attribute ?loc:c.loc (label what) f))
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

(* Depth first over the orders that keep each branch's order and real time,
   each followed by the suffix. A branch's next call waits while another
   branch's next call [precedes] it; a later call of that branch returned
   later still, so the heads alone decide. The first child of a branch point
   continues on the state that the call before it left; every other starts
   from [fresh ()], replayed along its path, so a state need be neither
   persistent nor copyable. [path] is newest first. The closest order is the
   one whose first difference comes latest, the first found winning a tie;
   it is completed with the calls it did not reach, each branch in turn, then
   the suffix. *)
let judge ~fresh ~precedes ~branches ~suffix =
  let calls = Array.of_list (List.map Array.of_list branches) in
  let cursors = Array.make (Array.length calls) 0 in
  let exhausted i = cursors.(i) = Array.length calls.(i) in
  let head i = fst calls.(i).(cursors.(i)) in
  let waits i =
    let rec on j =
      j < Array.length calls
      && ((j <> i && (not (exhausted j)) && precedes (head j) (head i))
         || on (j + 1))
    in
    on 0
  in
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
      else if exhausted i || waits i then from (i + 1)
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
let run_parallel ~workers ~stuck run program =
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
  (* A suffix call makes no value, has no [pre] and takes no element. *)
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
  (* Real time: a call that returned before another began comes first. *)
  let ran = Array.make (total + 1) None in
  List.iter (fun (Pending p) -> ran.(p.number) <- p.ran) pendings;
  let precedes a b =
    match (ran.(a), ran.(b)) with
    | Some { returned; _ }, Some { began; _ } ->
        if returned.domain = began.domain then returned.count < began.count
        else Os.earlier returned.clock began.clock
    | _ -> false
  in
  let verdict =
    Run.without_labels (fun () ->
        judge ~fresh ~precedes ~branches:(List.map steps branches)
          ~suffix:(steps suffix))
  in
  match verdict with
  | Explained order ->
      (* Labels count along the accepted order. *)
      let sides, _ = Run.without_labels fresh in
      let replay number =
        let pending =
          List.find (fun (Pending p) -> p.number = number) pendings
        in
        ignore (judge_call ~total sides pending : Failure.t option)
      in
      List.iter replay order
  | Unexplained { order; at; failure } ->
      raise
        (Failure.Check_failure
           (unexplained ~total ~parallel pendings ~order ~at failure))

(* One run from no value: the prefix as on one domain, invariants included,
   then, on several domains, the rest. A run whose call never returned
   releases nothing. A failing run raises its record with its failure, and a
   fatal exception leaves no record. *)
let execute ?workers program =
  let run = fresh_run () in
  let stuck = ref false in
  let ending =
    Failure.catch (fun () ->
        List.iter (run_call run) program.prefix;
        if Array.length program.branches > 0 then
          run_parallel ~workers ~stuck run program)
  in
  let record = List.rev run.rows in
  match if !stuck then ending else release_all run ending with
  | Ok () -> record
  | Error (#Failure.fault as fault) ->
      raise
        (Property.Run_failure
           { fault; record = record_text record; summary = summary record })
  | Error c -> Failure.reraise c

(* A run on the workers costs the system's calls and a judge, so a case runs
   [repetitions] times, sampling schedules for the price of a run. *)
let repetitions = 50

(* Declaring *)

let never_called ?loc ~cases ~elements names =
  Failure.vacuous ?loc
    (Pp.str
       "never called: %s (over %d passing cases); a call runs only where its \
        arguments resolve%s and its ~pre holds"
       (String.concat ", " (List.map (Pp.str "%S") names))
       cases
       (if elements then ", its value lists an element" else ""))

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
let stateful ?__POS__ ?timeout ?count ?(steps = 20) ?(domains = 1) name commands
    =
  let loc = Loc.resolve ?__POS__ () in
  let tags =
    Test_tree.Tag.prop :: "stateful"
    :: (if domains > 1 then [ "parallel" ] else [])
  in
  let retries = if domains > 1 then Some 0 else None in
  let gen = program ~steps ~domains commands in
  let body () =
    let commands = Array.of_list commands in
    check ~steps ~domains commands;
    let held = Array.make (Array.length commands) false in
    let called = Array.make (Array.length commands) false in
    let cases = ref 0 and passed = ref 0 and replayed = ref false in
    with_workers ?loc ~domains (fun workers ->
        (* On the workers a failing program depends on a schedule. *)
        let runs = if Option.is_some workers then repetitions else 1 in
        let stats =
          Run.property ?loc ?count ~runs gen (fun program ->
              let record = execute ?workers program in
              incr passed;
              Array.iteri (fun i h -> if h then held.(i) <- true) program.held;
              List.iter (fun (row : row) -> called.(row.command) <- true) record)
        in
        (* Every case runs [runs] times, and every run passed. *)
        cases := !passed / runs;
        replayed := stats.replayed > 0);
    let firsts = first_positions commands in
    let never i _ = firsts.(i) = i && held.(i) && not called.(i) in
    match List.filteri never (Array.to_list commands) with
    | [] -> ()
    (* Vacuity judges a generated run, and a replayed case alone is none. *)
    | _ when !cases = 0 || !replayed -> ()
    | never ->
        let name (Command c) = c.name in
        let elements =
          List.exists (fun c -> (command_shape c).takes_element) never
        in
        raise
          (Failure.Check_failure
             (never_called ?loc ~cases:!cases ~elements (List.map name never)))
  in
  List.fold_left
    (fun t name -> Test_tree.tag name t)
    (Test_tree.test ?__POS__ ?timeout ?retries name body)
    tags
