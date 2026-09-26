(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Failure = Windtrap.Private.Failure
module Gen_engine = Windtrap.Private.Gen_engine
module Loc = Windtrap.Private.Loc
module Property = Windtrap.Private.Property
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Private.Gen_engine.Shrink_tree
module Stateful = Windtrap.Private.Stateful
module Test_tree = Windtrap.Private.Test_tree

let strf = Printf.sprintf

exception Boom

(* Drawing and reading programs *)

let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_stateful" ~index)
let drawn gen index = Gen_engine.sample gen (state index)
let value node = Gen_engine.value (Shrink_tree.root node)
let program gen index = value (drawn gen index)
let printed gen program = Gen_engine.render_value gen program
let unit_scope run = run ()

(* The bodies below note what they see, and a program is read back by running
   it: [execute] evaluates no [pre], so the notes are the calls it holds. *)
let noted = ref []
let note text = noted := text :: !noted

let notes ?invariant program =
  noted := [];
  Stateful.execute ?invariant ~scope:unit_scope program;
  List.rev !noted

let calls node = notes (value node)

(* A counter floored at zero and capped at [cap]. Its commands take no
   argument, so a program is its sequence of names. Without [pre] a seed draws
   the same names, since [pre] draws nothing: those are the calls that repair
   folds over. *)
let cap = 3

let counter ~pre =
  let op name ~legal ~next =
    let pre = if pre then Some legal else None in
    Stateful.call ?pre name ~next (fun _ () -> note name)
  in
  [
    op "inc" ~legal:(fun m -> m < cap) ~next:succ;
    op "dec" ~legal:(fun m -> m > 0) ~next:pred;
  ]

(* Repair's rule, spelled over the drawn names. *)
let repaired drawn =
  let keep (m, kept) = function
    | "inc" when m < cap -> (m + 1, "inc" :: kept)
    | "dec" when m > 0 -> (m - 1, "dec" :: kept)
    | _ -> (m, kept)
  in
  List.rev (snd (List.fold_left keep (0, []) drawn))

let tick = Stateful.call "tick" ~next:succ (fun _ () -> note "tick")
let ticks ?pp_model steps = Stateful.program ~steps ?pp_model ~model:0 [ tick ]
let nothing _ () = ()
let dead name = Stateful.call name ~pre:(fun _ -> false) ~next:Fun.id nothing
let never = dead "never"

(* Five names, so a candidate that put one command in place of another would
   show as calls that are not a subsequence of the drawn ones. *)
let wide ~pre =
  let op i name =
    let pre = if pre then Some (fun m -> m mod 5 <> i) else None in
    Stateful.call ?pre name ~next:(fun m -> m + i + 1) (fun _ () -> note name)
  in
  List.mapi op [ "alpha"; "bravo"; "charlie"; "delta"; "echo" ]

(* Legality depends on the argument, so each body checks its [pre] again on
   the model that [execute] hands it. *)
module Ints = Set.Make (Int)

let set_commands =
  let op name legal next =
    Stateful.command name (Gen.int_range 0 4) ~pre:legal ~next (fun m x () ->
        note (if legal m x then name else strf "illegal %s %d" name x))
  in
  [
    op "add" (fun m x -> not (Ints.mem x m)) (fun m x -> Ints.add x m);
    op "remove" (fun m x -> Ints.mem x m) (fun m x -> Ints.remove x m);
  ]

(* A queue whose [pop] returns the newest element: right for one element,
   wrong from two on. *)
module Bad_queue = struct
  type t = { mutable items : int list }

  let create () = { items = [] }
  let push q x = q.items <- q.items @ [ x ]

  let pop q =
    match List.rev q.items with
    | [] -> invalid_arg "pop: empty"
    | newest :: rest ->
        q.items <- List.rev rest;
        newest
end

let queue_commands =
  [
    Stateful.command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < 4)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bad_queue.push q x);
    Stateful.call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal int (List.hd m) (Bad_queue.pop q));
  ]

let queue () = Stateful.program ~steps:8 ~model:[] queue_commands
let queue_scope run = run (Bad_queue.create ())

(* Failures as rows *)

let kind (f : Failure.t) =
  match f.kind with
  | Message m -> strf "message %S" m.kept
  | Raise { actual = Some a; _ } -> "raise " ^ a.kept
  | Raise { actual = None; _ } -> "raise"
  | Equality { expected; actual; _ } ->
      strf "equality %s, %s" expected.kept actual.kept
  | Containment _ -> "containment"
  | Baseline _ -> "baseline"
  | Property _ -> "property"
  | Timeout _ -> "timeout"

let label (f : Failure.t) =
  match f.msg with None -> "" | Some m -> strf "[%s] " m.kept

let row f = "failure " ^ label f ^ kind f

(* [escaped f] is what [f ()] did: it returned, failed or raised. *)
let escaped f =
  match f () with
  | () -> "returned"
  | exception Failure.Check_failure f -> row f
  | exception e -> "raised " ^ Failure.exn_to_string e

let raised e = escaped (fun () -> raise e)

let failure f =
  match f () with
  | () -> None
  | exception Failure.Check_failure f -> Some f
  | exception _ -> None

let site (loc : Loc.t option) =
  match loc with None -> "no site" | Some l -> strf "%s:%d" l.file l.line

let here (_, line, _, _) = strf "%s:%d" __FILE__ line

let property_failure = function
  | Property.Fail { failure; _ } -> Some failure
  | Pass _ | Coverage_failed _ | Gave_up _ -> None

let rendered (f : Failure.t) =
  match f.kind with Property { rendered; _ } -> Some rendered.kept | _ -> None

let inner (f : Failure.t) =
  match f.kind with Property { inner; _ } -> inner | _ -> None

let summary (f : Failure.t) =
  match f.kind with
  | Property { summary; _ } ->
      Some (Option.map (fun (s : Failure.text) -> s.kept) summary)
  | _ -> None

let search (f : Failure.t) =
  match f.kind with
  | Property { case_index; shrink_steps; _ } -> Some (case_index, shrink_steps)
  | _ -> None

(* The controls, and the two exceptions that [Failure.catch] never returns. *)
let passing =
  [
    ("a skip", Failure.Control (`Skip (Some "why")));
    ("a timeout", Failure.Control (`Timeout 0.5));
    ("an exit", Failure.Control `Exit);
    ("a discard", Failure.Control `Discard);
    ("Sys.Break", Sys.Break);
    ("Out_of_memory", Out_of_memory);
  ]

let asserted ?msg text =
  let f = Failure.message text in
  Failure.Check_failure { f with msg = Option.map Failure.text msg }

let raising e = [ Stateful.call "boom" ~next:succ (fun _ () -> raise e) ]
let one_call commands = program (Stateful.program ~steps:1 ~model:0 commands) 0

let execute ?loc ?invariant ~scope p () =
  Stateful.execute ?loc ?invariant ~scope p

let failing () = one_call (raising (asserted "the body"))

(* Commands *)

(* The bodies raise on lines of their own, away from the declarations. *)
let boom _ () _ = raise Exit
let boom_call _ () = raise Exit

let failing_site commands =
  let f = failure (execute ~scope:unit_scope (one_call commands)) in
  site (Option.bind f (fun (f : Failure.t) -> f.loc))

let captured_sites () =
  let p1, c1 =
    (__POS__, Stateful.command "boom" Gen.unit ~next:Fun.const boom)
  in
  let p2, c2 = (__POS__, Stateful.call "boom" ~next:Fun.id boom_call) in
  equal (list string)
    [ here p1; here p2 ]
    [ failing_site [ c1 ]; failing_site [ c2 ] ]

(* The payload is raised directly: [Check]'s capture succeeds in this test and
   would give every failure a location. *)
let located_site (_, loc, _) =
  let body _ () =
    raise
      (Failure.Check_failure
         (Failure.equality ?loc ~expected:"1" ~actual:"2" ()))
  in
  let pos = ("declared.ml", 42, 7, 11) in
  failing_site [ Stateful.call ~__POS__:pos "boom" ~next:Fun.id body ]

let located =
  [
    ("a failure without a location", None, "declared.ml:42");
    ("a failure with one", Some (Loc.of_pos ("body.ml", 9, 0, 4)), "body.ml:9");
  ]

let flattened_name () =
  let two = Stateful.call "two\nlines" ~next:Fun.id (fun _ () -> fail "x") in
  let pp_model ppf m = Format.fprintf ppf "a\nb%d" m in
  let gen = Stateful.program ~steps:2 ~pp_model ~model:0 [ two ] in
  let p = program gen 0 in
  let failed = require_some (failure (execute ~scope:unit_scope p)) in
  let summary = Option.value ~default:"no summary" (Stateful.summary p) in
  let label = (require_some failed.msg).kept in
  expect_exact (String.concat "\n" [ printed gen p; summary; label ])
  @@ __POS_OF__
       {| #  model before  call
 1  a b0          two lines
 2  a b0          two lines
2 calls, last: two lines
call 1 of 2: two lines|}

let commands =
  group "Commands"
    [
      test "a command without pre is legal in every model" (fun () ->
          let gen = Stateful.program ~steps:12 ~model:0 (counter ~pre:false) in
          equal int 12 (List.length (notes (program gen 0))));
      test "command and call default their site to the line that applies them"
        captured_sites;
      cases
        "a failing call without a location reports its command's site, and one \
         with a location keeps it"
        ~name:(fun (n, _, _) -> n)
        located
        (fun ((_, _, site) as r) -> equal string site (located_site r));
      test "a name's newlines become spaces in the table, summary and label"
        flattened_name;
    ]

(* Repair *)

let index = Gen.int_range 0 1_000_000

let repair_law index =
  let draw pre =
    notes (program (Stateful.program ~model:0 (counter ~pre)) index)
  in
  let drawn = draw false and kept = draw true in
  cover "a call was dropped" (List.length kept < List.length drawn);
  equal (list string) (repaired drawn) kept

(* A body notes a call that is illegal in the model before it. *)
let legal_nodes () =
  let gen = Stateful.program ~steps:12 ~model:Ints.empty set_commands in
  let budget = 3_000 in
  let nodes = ref 0 and illegal = ref [] in
  let rec visit node =
    if !nodes < budget then begin
      incr nodes;
      let bad =
        List.filter (String.starts_with ~prefix:"illegal") (calls node)
      in
      illegal := bad @ !illegal;
      Seq.iter visit (Shrink_tree.children node)
    end
  in
  List.iter (fun i -> visit (drawn gen i)) (List.init 40 Fun.id);
  equal int budget !nodes;
  equal (list string) [] !illegal

let malformed =
  [
    ("no command", 4, []);
    ("no command under steps 0", 0, []);
    ("a negative steps", -1, [ tick ]);
  ]

let repair =
  group "Repair"
    [
      prop
        "repair keeps a call iff its pre holds in the model that the kept \
         calls before it produced"
        index repair_law;
      test "steps defaults to 20" (fun () ->
          let gen = Stateful.program ~model:0 (counter ~pre:false) in
          equal int 20 (List.length (notes (program gen 0))));
      test "a pre that never holds leaves every program empty" (fun () ->
          let gen = Stateful.program ~steps:8 ~model:0 [ never ] in
          let programs = List.map (program gen) [ 0; 1; 2 ] in
          equal (list (list string)) [ []; []; [] ] (List.map notes programs));
      test "every node of the shrink tree holds only legal calls" legal_nodes;
      cases "a sample raises Invalid_argument naming Windtrap.stateful"
        ~name:(fun (n, _, _) -> n)
        malformed
        (fun (_, steps, commands) ->
          let gen = Stateful.program ~steps ~model:0 commands in
          raises_match (Exn.invalid_arg ~substring:"Windtrap.stateful: ")
            (fun () -> drawn gen 0));
    ]

(* Shrinking *)

let candidates tree = List.of_seq (Seq.map calls (Shrink_tree.children tree))

(* The arguments are [Gen.unit]'s, so every candidate of the root deletes
   calls. *)
let deleting_law index =
  let gen = Stateful.program ~steps:12 ~model:0 (counter ~pre:true) in
  let tree = drawn gen index in
  let parent = List.length (calls tree) in
  cover "repair dropped a call" (parent < 12);
  let long c = List.length c >= parent in
  equal (list (list string)) [] (List.filter long (candidates tree))

let no_longer_law index =
  let gen = Stateful.program ~steps:10 ~model:Ints.empty set_commands in
  let tree = drawn gen index in
  let parent = List.length (calls tree) in
  cover "repair dropped a call" (parent < 10);
  let long c = List.length c > parent in
  equal (list (list string)) [] (List.filter long (candidates tree))

let rec is_subsequence sub whole =
  match (sub, whole) with
  | [], _ -> true
  | _, [] -> false
  | x :: sub', y :: whole' ->
      if String.equal x y then is_subsequence sub' whole'
      else is_subsequence sub whole'

let subsequences () =
  let repaired = Stateful.program ~steps:14 ~model:0 (wide ~pre:true) in
  let drawn_gen = Stateful.program ~steps:14 ~model:0 (wide ~pre:false) in
  let nodes = ref 0 and strays = ref [] in
  for index = 0 to 4 do
    let whole = notes (program drawn_gen index) in
    let budget = !nodes + 400 in
    let rec visit node =
      if !nodes < budget then begin
        incr nodes;
        let calls = calls node in
        if not (is_subsequence calls whole) then strays := calls :: !strays;
        Seq.iter visit (Shrink_tree.children node)
      end
    in
    visit (drawn repaired index)
  done;
  equal int 2_000 !nodes;
  equal (list (list string)) [] !strays

let weights () =
  let commands = wide ~pre:false in
  let gen =
    Stateful.program ~steps:20 ~model:0 (List.hd commands :: commands)
  in
  let drawn =
    List.concat_map (fun i -> notes (program gen i)) (List.init 20 Fun.id)
  in
  let count name = List.length (List.filter (String.equal name) drawn) in
  let alpha = count "alpha" in
  List.iter
    (fun name ->
      greater int ~than:0 (count name);
      less int ~than:alpha (count name))
    [ "bravo"; "charlie"; "delta"; "echo" ]

let bad_queue () =
  let law _ p = Stateful.execute ~scope:queue_scope p in
  let outcome =
    Property.run ~count:(`Declared 40) ~summary:Stateful.summary ~root
      ~path:"bad_queue" (queue ()) law
  in
  let f = require_match property_failure outcome in
  equal text " #  call\n 1  push 0\n 2  push 1\n 3  pop"
    (require_match rendered f);
  equal (option string) (Some "3 calls, last: pop") (require_match summary f);
  equal string "failure [call 3 of 3: pop] equality 0, 1"
    (row (require_some (inner f)))

let shrinking =
  group "Shrinking"
    [
      prop
        "no candidate of a drawn program is as long as it when its calls take \
         no argument"
        index deleting_law;
      prop "no candidate of a drawn program is longer than it" index
        no_longer_law;
      test "every node's calls are calls of the drawn program, in order"
        subsequences;
      test "a command listed twice is drawn more often than one listed once"
        weights;
      test "a buggy queue shrinks to the shortest program that shows its bug"
        bad_queue;
    ]

(* Printing *)

let table gen = printed gen (program gen 0)

let raising_cell () =
  let pp_model ppf m =
    if m = 2 then raise Not_found else Format.pp_print_int ppf m
  in
  expect_exact (table (ticks ~pp_model 6))
  @@ __POS_OF__
       {| #  model before                 call
 1  0                            tick
 2  1                            tick
 3  <pp_model raised Not_found>  tick
 4  3                            tick
 5  4                            tick
 6  5                            tick|}

let long_cell () =
  let e_acute = String.concat "" (List.init 100 (fun _ -> "\u{00e9}")) in
  let pp_model ppf _ = Format.pp_print_string ppf e_acute in
  expect_exact (table (ticks ~pp_model 1))
  @@ __POS_OF__
       {| #  model before                                                  call
 1  ééééééééééééééééééééééééééééééééééééééééééééééééééééééééé...  tick|}

let printerless () =
  let opaque =
    Stateful.command "opaque" (Gen.constant 5) ~next:Fun.const (fun _ _ () ->
        ())
  in
  expect_exact (table (Stateful.program ~steps:1 ~model:0 [ opaque ]))
  @@ __POS_OF__
       {| #  call
 1  opaque <no printer: attach one with Gen.with_pp>|}

let long_argument () =
  let big = Gen.constant (String.make 300 'x') in
  let big = Gen.with_pp Format.pp_print_string big in
  let write = Stateful.command "write" big ~next:Fun.const (fun _ _ () -> ()) in
  expect_exact (table (Stateful.program ~steps:1 ~model:0 [ write ]))
  @@ __POS_OF__
       {| #  call
 1  write xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx... (truncated; 300 bytes total)|}

(* A sample of a printerless [Gen.map] renders as a pre-image. *)
let rendering () =
  let set =
    Stateful.command "set" (Gen.map succ Gen.nat) ~next:Fun.const (fun _ _ () ->
        ())
  in
  let gen = Stateful.program ~steps:1 ~model:0 [ set ] in
  let rendering =
    match Gen_engine.render (Shrink_tree.root (drawn gen 0)) with
    | Value s -> "value\n" ^ s
    | Pre_image s -> "pre-image\n" ^ s
  in
  expect_exact rendering
  @@ __POS_OF__
       {|value
 #  call
 1  set <no printer: attach one with Gen.with_pp>|}

let cut steps =
  let rows = String.split_on_char '\n' (table (ticks steps)) in
  let omission = List.filter (String.starts_with ~prefix:"\u{2026}") rows in
  strf "%d lines, %s" (List.length rows) (String.concat "" omission)

let printing =
  group "Printing"
    [
      test "a program prints as a table, a call without argument as its name"
        (fun () ->
          expect_exact (table (ticks 3))
          @@ __POS_OF__ {| #  call
 1  tick
 2  tick
 3  tick|});
      test "pp_model adds a column that holds the model before each call"
        (fun () ->
          let gen = ticks ~pp_model:Format.pp_print_int 12 in
          expect_exact (table gen)
          @@ __POS_OF__
               {| #  model before  call
 1  0             tick
 2  1             tick
 3  2             tick
 4  3             tick
 5  4             tick
 6  5             tick
 7  6             tick
 8  7             tick
 9  8             tick
10  9             tick
11  10            tick
12  11            tick|});
      test "a pp_model that raises costs its own cell" raising_cell;
      test "a model cell is cut at 60 code points" long_cell;
      test "an argument without a printer prints as the placeholder" printerless;
      test "an argument is cut at 200 bytes, with its size" long_argument;
      test "a program renders as a value, never a pre-image" rendering;
      test "a program of more than 40 calls prints its first and last 20"
        (fun () ->
          expect_exact (table (ticks 50))
          @@ __POS_OF__
               {| #  call
 1  tick
 2  tick
 3  tick
 4  tick
 5  tick
 6  tick
 7  tick
 8  tick
 9  tick
10  tick
11  tick
12  tick
13  tick
14  tick
15  tick
16  tick
17  tick
18  tick
19  tick
20  tick
… (10 calls omitted)
31  tick
32  tick
33  tick
34  tick
35  tick
36  tick
37  tick
38  tick
39  tick
40  tick
41  tick
42  tick
43  tick
44  tick
45  tick
46  tick
47  tick
48  tick
49  tick
50  tick|});
      cases "the calls omitted start past 40"
        ~name:(fun (n, _) -> strf "%d calls" n)
        [ (40, "41 lines, "); (41, "42 lines, \u{2026} (1 call omitted)") ]
        (fun (n, row) -> equal string row (cut n));
      test "the empty program prints (no commands)" (fun () ->
          let gen = Stateful.program ~steps:5 ~model:0 [ never ] in
          expect_exact (table gen) @@ __POS_OF__ {|(no commands)|});
      test "steps 0 draws the empty program" (fun () ->
          equal (list string) [] (notes (program (ticks 0) 0)));
    ]

(* Summaries *)

let summary_rows =
  [
    (0, None);
    (1, Some "1 call, last: tick");
    (3, Some "3 calls, last: tick");
    (50, Some "50 calls, last: tick");
  ]

(* Only [fail] is legal first, and only [ok] after it. *)
let last_not_failing () =
  let fails =
    Stateful.call "fail"
      ~pre:(fun m -> m = 0)
      ~next:succ
      (fun _ () -> fail "first")
  in
  let ok = Stateful.call "ok" ~pre:(fun m -> m > 0) ~next:succ nothing in
  let gen = Stateful.program ~steps:6 ~model:0 [ fails; ok ] in
  let rows p = List.length (String.split_on_char '\n' (printed gen p)) - 1 in
  let long p = if rows p >= 2 then Some p else None in
  let p =
    require_some
      (List.find_map long (List.map (program gen) (List.init 50 Fun.id)))
  in
  equal (option string)
    (Some (strf "%d calls, last: ok" (rows p)))
    (Stateful.summary p);
  equal string
    (strf {|failure [call 1 of %d: fail] message "first"|} (rows p))
    (escaped (execute ~scope:unit_scope p))

let summaries =
  group "Summaries"
    [
      cases "summary is the table in one line, and None for the empty program"
        ~name:(fun (n, _) -> strf "steps %d" n)
        summary_rows
        (fun (n, s) ->
          equal (option string) s (Stateful.summary (program (ticks n) 0)));
      test "summary names the last call, and never the failing one"
        last_not_failing;
    ]

(* Raising in pre and next *)

let raising_in phase e =
  let pre =
    match phase with `Pre -> Some (fun _ -> raise e) | `Next -> None
  in
  let next m = match phase with `Next -> raise e | `Pre -> m in
  [ Stateful.call ?pre "boom" ~next (fun _ () -> ()) ]

let phase_name = function `Pre -> "pre" | `Next -> "next"
let sampled commands () = ignore (one_call commands)

let wrapped =
  [
    (`Pre, "raised call 1: boom, ~pre raised Test_stateful.Boom");
    (`Next, "raised call 1: boom, ~next raised Test_stateful.Boom");
  ]

let exception_backtrace = function
  | Error (`Exception (_, raw)) -> Some raw
  | _ -> None

let backtrace () =
  let caught = Failure.catch (sampled (raising_in `Pre Boom)) in
  let raw = require_match exception_backtrace caught in
  contains ~sub:"test_stateful.ml" (Printexc.raw_backtrace_to_string raw)

let escapes =
  List.concat_map (fun (n, e) -> [ (`Pre, n, e); (`Next, n, e) ]) passing

(* Among the calls a draw offered, one was dropped before the call that
   raised. *)
let numbering () =
  let offered name holds m =
    note name;
    holds m
  in
  let raises m = if m >= 1 then failwith "nth" else false in
  let commands =
    [
      Stateful.call "first"
        ~pre:(offered "first" (fun m -> m = 0))
        ~next:succ nothing;
      Stateful.call "never"
        ~pre:(offered "never" (fun _ -> false))
        ~next:Fun.id nothing;
      Stateful.call "boom" ~pre:(offered "boom" raises) ~next:Fun.id nothing;
    ]
  in
  let gen = Stateful.program ~steps:12 ~model:0 commands in
  let after_a_drop index =
    noted := [];
    match drawn gen index with
    | _ -> None
    | exception e ->
        if List.length !noted > 2 then Some (Failure.exn_to_string e) else None
  in
  equal string {|call 2: boom, ~pre raised Failure("nth")|}
    (require_some (List.find_map after_a_drop (List.init 200 Fun.id)))

exception Candidate_boom

(* A program that starts with [inc] and holds a [check]: a candidate that
   deletes the [inc] offers [check] at model 0. *)
let candidate_raise () =
  let commands =
    [
      Stateful.call "inc" ~next:succ (fun _ () -> note "inc");
      Stateful.call "check"
        ~pre:(fun m -> if m = 0 then raise Candidate_boom else true)
        ~next:Fun.id
        (fun _ () -> note "check");
    ]
  in
  let gen = Stateful.program ~steps:6 ~model:0 commands in
  let usable index =
    match calls (drawn gen index) with
    | "inc" :: rest when List.mem "check" rest -> Some (drawn gen index)
    | _ -> None
    | exception _ -> None
  in
  let tree = require_some (List.find_map usable (List.init 50 Fun.id)) in
  let rec forced seq =
    match seq () with
    | Seq.Nil -> "no candidate raised"
    | Seq.Cons (_, rest) -> forced rest
    | exception e -> "raised " ^ Failure.exn_to_string e
  in
  contains ~sub:", ~pre raised Test_stateful.Candidate_boom"
    (forced (Shrink_tree.children tree))

let raising_in_pre_and_next =
  group "Raising in pre and next"
    [
      cases
        "a pre or a next that raises escapes the draw wrapped, naming the call"
        ~name:(fun (p, _) -> phase_name p)
        wrapped
        (fun (phase, row) ->
          equal string row (escaped (sampled (raising_in phase Boom))));
      test "the wrapped exception keeps the original backtrace" backtrace;
      test "an assertion in pre is wrapped as any other exception" (fun () ->
          let e = asserted "nope" in
          expect_exact (escaped (sampled (raising_in `Pre e)))
          @@ __POS_OF__
               {|raised call 1: boom, ~pre raised windtrap assertion failure: nope|});
      cases "a control or a fatal exception escapes pre and next as itself"
        ~name:(fun (p, n, _) -> strf "%s in %s" n (phase_name p))
        escapes
        (fun (phase, _, e) ->
          equal string (raised e) (escaped (sampled (raising_in phase e))));
      test "the wrapped exception numbers the kept calls from one" numbering;
      test "a pre that raises on a shrink candidate escapes its forcing"
        candidate_raise;
    ]

(* Executing *)

let trace () =
  let counted =
    Stateful.call "tick" ~next:succ (fun m () -> note (strf "body %d" m))
  in
  let p = program (Stateful.program ~steps:3 ~model:0 [ counted ]) 0 in
  let invariant m () = note (strf "invariant %d" m) in
  equal (list string)
    [
      "invariant 0";
      "body 0";
      "invariant 1";
      "body 1";
      "invariant 2";
      "body 2";
      "invariant 3";
    ]
    (notes ~invariant p)

let body_failures =
  [
    ( "an assertion",
      asserted "nope",
      {|failure [call 1 of 1: boom] message "nope"|} );
    ( "an assertion with a msg of two lines",
      asserted ~msg:"note\nand more" "nope",
      {|failure [call 1 of 1: boom; note and more] message "nope"|} );
    ( "any other exception",
      Not_found,
      "failure [call 1 of 1: boom] raise Not_found" );
  ]

(* [on visit f] runs [f] at the [visit]th check of the invariant, the first
   being on the fresh system. *)
let on visit f =
  let visits = ref 0 in
  fun _ () ->
    incr visits;
    if !visits = visit then f ()

let checked ~visit f =
  escaped
    (execute ~invariant:(on visit f) ~scope:unit_scope (program (ticks 3) 0))

let invariant_failures =
  [
    ( "an assertion on the fresh system",
      1,
      asserted ~msg:"note" "nope",
      {|failure [invariant on the fresh system; note] message "nope"|} );
    ( "an assertion after call 1",
      2,
      asserted "nope",
      {|failure [invariant after call 1 of 3: tick] message "nope"|} );
    ( "an exception on the fresh system",
      1,
      Not_found,
      "failure [invariant on the fresh system] raise Not_found" );
    ( "an exception after call 2",
      3,
      Not_found,
      "failure [invariant after call 2 of 3: tick] raise Not_found" );
  ]

let invariant_escapes =
  List.concat_map
    (fun (n, e) ->
      [ ("on the fresh system", 1, n, e); ("after a call", 2, n, e) ])
    passing

let invariant_sites =
  [
    ("an exception on the fresh system", 1, (fun () -> raise Exit), "no site");
    ("an exception after a call", 2, (fun () -> raise Exit), "no site");
    ( "a located failure after a call",
      2,
      (fun () -> fail ~__POS__:("invariant.ml", 3, 0, 5) "wrong"),
      "invariant.ml:3" );
  ]

let invariant_site (_, visit, f, _) =
  let pos = ("declared.ml", 7, 0, 3) in
  let p = one_call [ Stateful.call ~__POS__:pos "tick" ~next:succ nothing ] in
  let f = failure (execute ~invariant:(on visit f) ~scope:unit_scope p) in
  site (Option.bind f (fun (f : Failure.t) -> f.loc))

let executing =
  group "Executing"
    [
      test
        "a call runs its body on the model before it, and the invariant runs \
         before the first call and after each"
        trace;
      test "the invariant runs once on the empty program" (fun () ->
          let p = program (Stateful.program ~steps:5 ~model:0 [ never ]) 0 in
          let invariant m () = note (strf "invariant %d" m) in
          equal (list string) [ "invariant 0" ] (notes ~invariant p));
      cases
        "a body's failure is raised as a check failure under its call's label"
        ~name:(fun (n, _, _) -> n)
        body_failures
        (fun (_, e, row) ->
          equal string row
            (escaped (execute ~scope:unit_scope (one_call (raising e)))));
      cases "a control or a fatal exception escapes a body as itself" ~name:fst
        passing (fun (_, e) ->
          equal string (raised e)
            (escaped (execute ~scope:unit_scope (one_call (raising e)))));
      cases
        "an invariant's failure is raised as a check failure under its label"
        ~name:(fun (n, _, _, _) -> n)
        invariant_failures
        (fun (_, visit, e, row) ->
          equal string row (checked ~visit (fun () -> raise e)));
      cases "a control or a fatal exception escapes an invariant as itself"
        ~name:(fun (at, _, n, _) -> strf "%s %s" n at)
        invariant_escapes
        (fun (_, visit, _, e) ->
          equal string (raised e) (checked ~visit (fun () -> raise e)));
      cases
        "an invariant's failure keeps its own location, and has none without \
         one"
        ~name:(fun (n, _, _, _) -> n)
        invariant_sites
        (fun ((_, _, _, site) as r) -> equal string site (invariant_site r));
    ]

(* The scope *)

let released path =
  let p =
    match path with
    | None -> program (ticks 2) 0
    | Some e -> one_call (raising e)
  in
  let count = ref 0 in
  let scope run = Fun.protect ~finally:(fun () -> incr count) run in
  ignore (escaped (execute ~scope p) : string);
  !count

let paths =
  [
    ("a passing program", None);
    ("a failing body", Some (asserted "nope"));
    ("a skip", Some (Failure.Control (`Skip (Some "why"))));
    ("a timeout", Some (Failure.Control (`Timeout 0.5)));
    ("an uncaught exception", Some Not_found);
  ]

let never_ran =
  "the scope returned without running the program; a scope must call its \
   callback exactly once"

let twice =
  {|raised Invalid_argument("Windtrap.stateful: the scope called its callback twice; a scope must call it exactly once")|}

let doubled =
  [
    ( "calls back twice",
      false,
      fun run ->
        run ();
        run () );
    ( "swallows the second call's exception",
      false,
      fun run ->
        run ();
        try run () with Invalid_argument _ -> () );
    ( "swallows the program's failure, then calls back again",
      true,
      fun run ->
        (try run () with Failure.Check_failure _ -> ());
        run () );
    ( "swallows the second call's exception, then raises",
      false,
      fun run ->
        run ();
        (try run () with Invalid_argument _ -> ());
        raise Exit );
  ]

let doubled_row (_, fails, scope) =
  let p = if fails then failing () else program (ticks 1) 0 in
  escaped (execute ~scope p)

let runs_once () =
  let p = program (ticks 1) 0 in
  let scope run =
    run ();
    run ()
  in
  noted := [];
  ignore (escaped (execute ~scope p) : string);
  equal (list string) [ "tick" ] (List.rev !noted)

let crossing =
  [
    ("a scope that lets it through", unit_scope);
    ( "a scope that catches and raises it again",
      fun run ->
        match run () with
        | () -> ()
        | exception e ->
            Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) );
    ( "a scope that swallows it",
      fun run -> try run () with Failure.Check_failure _ -> () );
  ]

let releasing e run = match run () with () -> raise e | exception _ -> raise e

let cut_release run =
  Fun.protect ~finally:(fun () -> raise (Failure.Control (`Timeout 0.5))) run

let over_failing =
  ("an exception", releasing Not_found, None)
  :: List.map (fun (n, e) -> (n, releasing e, Some e)) passing
  @ [
      ( "a timeout that cuts a Fun.protect release",
        cut_release,
        Some (Failure.Control (`Timeout 0.5)) );
    ]

let over_failing_row (_, scope, replaced) =
  let expected =
    match replaced with
    | None -> {|failure [call 1 of 1: boom] message "the body"|}
    | Some e -> raised e
  in
  equal string expected (escaped (execute ~scope (failing ())))

let before =
  ("Not_found", Not_found) :: ("an assertion", asserted "nope") :: passing

let lifecycle () =
  let scopes = ref 0 and releases = ref 0 and executions = ref 0 in
  let scope run =
    incr scopes;
    Fun.protect ~finally:(fun () -> incr releases) (fun () -> queue_scope run)
  in
  let law _ p =
    incr executions;
    Stateful.execute ~scope p
  in
  let outcome =
    Property.run ~count:(`Declared 40) ~root ~path:"lifecycle" (queue ()) law
  in
  let f = require_match property_failure outcome in
  let case, steps = require_match search f in
  equal (pair int int) (!executions, !executions) (!scopes, !releases);
  greater int ~than:0 steps;
  greater int ~than:(case + 1) !executions

let misused =
  [
    ( "a scope that never calls back",
      (fun _ -> ()),
      strf "(no commands): message %S" never_ran );
    ( "a scope that calls back twice",
      (fun run ->
        run (Bad_queue.create ());
        run (Bad_queue.create ())),
      {|(no commands): raise Invalid_argument("Windtrap.stateful: the scope called its callback twice; a scope must call it exactly once")|}
    );
  ]

let shrunk_misuse (_, scope, _) =
  let law _ p = Stateful.execute ~scope p in
  let outcome =
    Property.run ~count:(`Declared 4) ~root ~path:"misuse" (queue ()) law
  in
  let f = require_match property_failure outcome in
  require_match rendered f ^ ": " ^ kind (require_some (inner f))

let never_calls_back () =
  let loc = Loc.of_pos ("spec.ml", 42, 7, 9) in
  let p = program (ticks 1) 0 in
  let f = require_some (failure (execute ~loc ~scope:ignore p)) in
  equal string (strf "message %S" never_ran) (kind f);
  equal string "spec.ml:42" (site f.loc)

let scope =
  group "The scope"
    [
      cases "a scope's release runs once on every path out of the program"
        ~name:fst paths (fun (_, path) -> equal int 1 (released path));
      test "a scope that returns without calling back fails the case at loc"
        never_calls_back;
      cases
        "a second call raises Invalid_argument, whatever else the case has to \
         say"
        ~name:(fun (n, _, _) -> n)
        doubled
        (fun r -> equal string twice (doubled_row r));
      test "a second call runs nothing" runs_once;
      cases
        "the failure of a program is raised through the scope, and again when \
         the scope swallows it"
        ~name:fst crossing (fun (_, scope) ->
          equal string {|failure [call 1 of 1: boom] message "the body"|}
            (escaped (execute ~scope (failing ()))));
      cases
        "what the scope raises over a failing program is dropped, unless a \
         control or a fatal exception"
        ~name:(fun (n, _, _) -> n)
        over_failing over_failing_row;
      cases "what the scope raises before it calls back propagates as it is"
        ~name:fst before (fun (_, e) ->
          let scope _ = raise e in
          equal string (raised e)
            (escaped (execute ~scope (program (ticks 1) 0))));
      test "what the scope raises after a passing program propagates as it is"
        (fun () ->
          let scope = releasing Not_found in
          equal string "raised Not_found"
            (escaped (execute ~scope (program (ticks 1) 0))));
      test "the scope runs once per case and once per shrink candidate"
        lifecycle;
      cases
        "under Property.run, a misused scope shrinks to the empty program, \
         whose failure names the misuse"
        ~name:(fun (n, _, _) -> n)
        misused
        (fun ((_, _, row) as r) -> equal string row (shrunk_misuse r));
    ]

(* Declaring *)

let single tree =
  require_match
    (function [ c ] -> Some c | _ -> None)
    (Test_tree.flatten [ tree ])

let body tree =
  require_match
    (function Test_tree.Body f -> Some f | Test_tree.Scoped _ -> None)
    (single tree).Test_tree.body

let known_tags = [ "absent"; "custom"; "prop"; "stateful" ]

let declared () =
  let pos = ("spec.ml", 42, 0, 7) in
  let t =
    Stateful.stateful ~__POS__:pos ~tags:[ "custom" ] ~timeout:2.5 "spec"
      ~model:0 ~scope:unit_scope [ tick ]
  in
  let c = single t in
  let tags = List.filter (fun n -> Test_tree.Tag.mem n c.tags) known_tags in
  let limit =
    match c.timeout with None -> "no limit" | Some s -> strf "limit %gs" s
  in
  equal string "spec: tags [custom, prop, stateful], limit 2.5s, spec.ml:42"
    (strf "%s: tags [%s], %s, %s" (String.concat "/" c.path)
       (String.concat ", " tags) limit (site c.loc))

let wiring () =
  let systems = ref 0 and releases = ref 0 and checks = ref 0 in
  let scope run =
    incr systems;
    Fun.protect ~finally:(fun () -> incr releases) run
  in
  let invariant _ () = incr checks in
  let t =
    Stateful.stateful ~count:3 ~steps:3 "wiring" ~model:0 ~invariant ~scope
      [ tick ]
  in
  noted := [];
  body t ();
  equal string "3 systems, 3 releases, 9 calls, 12 invariant checks"
    (strf "%d systems, %d releases, %d calls, %d invariant checks" !systems
       !releases (List.length !noted) !checks)

(* Legal only before the first [tick], so only some programs call it. *)
let first = Stateful.call "first" ~pre:(fun m -> m = 0) ~next:Fun.id nothing

let never_called names =
  strf
    "never called: %s (over 5 passing cases); a command is called only where \
     its ~pre holds"
    names

let judged =
  [
    ("a command listed twice", 5, [ never; never ], never_called {|"never"|});
    ("beside a command called", 5, [ tick; never ], never_called {|"never"|});
    ( "two commands, in the order of the list",
      5,
      [ dead "b"; tick; dead "a\nz" ],
      never_called {|"b", "a z"|} );
    ( "two commands of one name",
      5,
      [ dead "x"; dead "x" ],
      never_called {|"x", "x"|} );
    ("a command some programs call", 20, [ tick; first ], "passed");
    ("no case", 0, [ never ], "passed");
  ]

let verdict (_, count, commands, _) =
  let t =
    Stateful.stateful ~count ~steps:3 "spec" ~model:0 ~scope:unit_scope commands
  in
  match body t () with
  | () -> "passed"
  | exception Failure.Check_failure { kind = Message m; _ } -> m.kept

let declaration_sites () =
  let pos = ("spec.ml", 42, 0, 7) in
  let located scope commands =
    let t =
      Stateful.stateful ~__POS__:pos ~count:1 "spec" ~model:0 ~scope commands
    in
    let f = require_some (failure (body t)) in
    let inner = Option.bind (inner f) (fun (f : Failure.t) -> f.loc) in
    strf "%s, inner %s" (site f.loc) (site inner)
  in
  equal (list string)
    [ "spec.ml:42, inner no site"; "spec.ml:42, inner spec.ml:42" ]
    [ located unit_scope [ never ]; located (fun _ -> ()) [ tick ] ]

let threaded () =
  let third =
    Stateful.call "tick" ~next:succ (fun m () ->
        if m >= 2 then fail "the third call")
  in
  let t =
    Stateful.stateful ~count:3 ~steps:3 ~pp_model:Format.pp_print_int "failing"
      ~model:0 ~scope:unit_scope [ third ]
  in
  let f = require_some (failure (body t)) in
  expect_exact (require_match rendered f)
  @@ __POS_OF__
       {| #  model before  call
 1  0             tick
 2  1             tick
 3  2             tick|}

let replayed () =
  let trace = ref [] in
  let gen = queue () in
  let law _ p =
    trace := printed gen p :: !trace;
    Stateful.execute ~scope:queue_scope p
  in
  let outcome =
    Property.run ~count:(`Declared 40) ~root ~path:"replay" gen law
  in
  let f = require_match property_failure outcome in
  let case, steps = require_match search f in
  let counterexample = require_match rendered f in
  ( List.rev !trace,
    strf "%s\ncase %d, shrunk %d steps" counterexample case steps )

let replay () =
  let first = replayed () in
  let second = replayed () in
  greater int ~than:20 (List.length (fst first));
  equal (pair (list string) string) first second

let declaring =
  group "Declaring"
    [
      test "stateful declares a test tagged prop and stateful, at its site"
        declared;
      test "stateful runs count cases, each on a fresh system over steps calls"
        wiring;
      cases
        "a command no passing program called fails the test, named in the \
         order of the commands"
        ~name:(fun (n, _, _, _) -> n)
        judged
        (fun ((_, _, _, row) as r) -> equal string row (verdict r));
      test
        "the declaration site locates the test, a command never called and a \
         scope that never called back"
        declaration_sites;
      test "pp_model reaches the printed counterexample" threaded;
      test "a root seed replays the same programs and counterexample" replay;
      test "a timeout that is not finite and positive raises" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Stateful.stateful ~timeout:0. "t" ~model:0 ~scope:unit_scope
                [ tick ]));
    ]

let () =
  exit
    (run "stateful"
       [
         commands;
         repair;
         shrinking;
         printing;
         summaries;
         raising_in_pre_and_next;
         executing;
         scope;
         declaring;
       ])
