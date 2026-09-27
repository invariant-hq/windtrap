(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Check = Windtrap.Private.Check
module Failure = Windtrap.Private.Failure
module Gen_engine = Windtrap.Private.Gen_engine
module Loc = Windtrap.Private.Loc
module Property = Windtrap.Private.Property
module Seed = Windtrap.Private.Seed
module Shrink_tree = Gen_engine.Shrink_tree

let strf = Printf.sprintf
let root = 0x00c0ffee1234abcdL
let placeholder = "<no printer: attach one with Gen.with_pp>"
let unproduced = "<generator raised before producing a value>"
let even n = n mod 2 = 0

(* Outcomes as rows *)

type payload = {
  rendered : string;
  summary : string option;
  case_index : int;
  steps : int;
  ending : Failure.shrink_end;
  seed : Seed.seed;
  count : int option;
  examples : bool;
  rendering : Failure.rendering;
  inner : Failure.t option;
}

let payload (f : Failure.t) =
  match f.kind with
  | Property p ->
      Some
        {
          rendered = p.rendered.kept;
          summary = Option.map (fun (t : Failure.text) -> t.kept) p.summary;
          case_index = p.case_index;
          steps = p.shrink_steps;
          ending = p.shrink_end;
          seed = p.root;
          count = p.count;
          examples = p.examples;
          rendering = p.rendering;
          inner = p.inner;
        }
  | _ -> None

let tally (s : Property.stats) = strf "%d cases, %d discards" s.cases s.discards

let verdict = function
  | Property.Pass s -> "pass, " ^ tally s
  | Fail { stats; _ } -> "fail, " ^ tally stats
  | Coverage_failed s -> "coverage failed, " ^ tally s
  | Gave_up s -> "gave up, " ^ tally s

let stats = function
  | Property.Pass s | Fail { stats = s; _ } | Coverage_failed s | Gave_up s -> s

let ending = function
  | Failure.Converged -> "converged"
  | Budget_spent -> "budget spent"
  | Candidate_raised t -> "candidate raised " ^ t.kept
  | Timed_out limit -> strf "timed out after %gs" limit

let case ~examples index =
  strf "%s %d" (if examples then "example" else "case") index

let count = function None -> "no count" | Some n -> strf "count %d" n

let rendered p =
  match p.rendering with
  | Failure.Value -> p.rendered
  | Pre_image -> p.rendered ^ " (pre-image)"

let payload_row p =
  strf "%s, %d steps, %s: %s"
    (case ~examples:p.examples p.case_index)
    p.steps (ending p.ending) (rendered p)

let inner_row (f : Failure.t option) =
  let msg (f : Failure.t) =
    Option.fold ~none:"" ~some:(fun (t : Failure.text) -> t.kept ^ ": ") f.msg
  in
  match f with
  | None -> "no inner failure"
  | Some ({ kind = Equality { expected; actual; not_; _ }; _ } as f) ->
      strf "%sexpected %s, actual %s%s" (msg f) expected.kept actual.kept
        (if not_ then ", negated" else "")
  | Some ({ kind = Message t; _ } as f) -> msg f ^ "message " ^ t.kept
  | Some ({ kind = Raise { actual = Some t; _ }; _ } as f) ->
      msg f ^ "raise " ^ t.kept
  | Some _ -> "another failure"

let failure_row (f : Failure.t) =
  match (payload f, f.kind) with
  | Some p, _ -> payload_row p ^ "; " ^ inner_row p.inner
  | None, Timeout { limit; case = Some c } ->
      strf "timed out after %gs at %s, %d passed, root %Lx, %s" limit
        (case ~examples:c.examples c.case_index)
        c.passed c.root (count c.count)
  | None, _ -> "another failure"

(* The verdict, the stats and, for a [Fail], the failure: what a replay of the
   same arguments must reproduce. *)
let outcome_row = function
  | Property.Fail { failure; _ } as o -> verdict o ^ "; " ^ failure_row failure
  | o -> verdict o

let failed =
  require_match (function
    | Property.Fail { failure; _ } -> Some failure
    | _ -> None)

let counterexample o = require_match payload (failed o)

let coverage_row (s : Property.stats) =
  let status (c : Property.cover_status) =
    strf "%s %d%s" c.label c.hits (if c.satisfied then "" else " unsatisfied")
  in
  match s.coverage with
  | [] -> "no demand"
  | coverage -> String.concat ", " (List.map status coverage)

(* What leaves [f]: the exception as printed, or ["an outcome"]. *)
let escaped f =
  match f () with
  | (_ : Property.outcome) -> "an outcome"
  | exception e -> Printexc.to_string e

(* Generated streams *)

(* The value that case [index] of [path] draws under [root], by the derivation
   that [Property.run] states. *)
let value_at ?(root = root) gen ~path ~index =
  Gen_engine.value
    (Shrink_tree.root
       (Gen_engine.sample gen (Seed.make (Seed.derive ~root ~path ~index))))

(* The discarded cases before [n] cases of [gen] under [path] satisfy [keep]. *)
let discarded gen ~path ~keep n =
  let rec scan index kept acc =
    if kept = n then acc
    else if keep (value_at gen ~path ~index) then
      scan (index + 1) (kept + 1) acc
    else scan (index + 1) kept (acc + 1)
  in
  scan 0 0 0

(* A law that records the values it runs on. *)
let traced law =
  let seen = ref [] in
  let run ctx x =
    seen := x :: !seen;
    law ctx x
  in
  (run, fun () -> List.rev !seen)

(* Hand-built shrink trees *)

let node root children = Shrink_tree.make ~root ~children:(List.to_seq children)
let drawn tree = Gen_engine.make ~pp:Format.pp_print_int (fun s -> (tree, s))

(* [n] down to [0], one candidate per node. *)
let rec chain n =
  Shrink_tree.make ~root:n ~children:(fun () ->
      if n = 0 then Seq.Nil else Seq.Cons (chain (n - 1), Seq.empty))

let timeout limit = Failure.Control (`Timeout limit)

(* Determinism *)

(* Values in [10, 100] fail, [1, 9] discard and [0] passes, so a run
   interleaves discards, failures and a search that meets discarding
   candidates. *)
let banded = Gen.int_range 0 100

let band _ x =
  if x >= 10 then Check.fail "big" else if x >= 1 then Property.reject ()

(* A case of these laws runs [Property.run] once or twice, so 25 cases keep
   the suite's time. *)
let run_seed =
  Gen.with_pp
    (fun ppf (root, path) -> Format.fprintf ppf "%Lx %S" root path)
    (Gen.pair Gen.int64 (Gen.string_of (Gen.char_range 'a' 'e')))

let replays (root, path) =
  let run () = outcome_row (Property.run ~root ~path banded band) in
  equal string (run ()) (run ())

let derives (root, path) =
  let law, seen = traced band in
  let p = counterexample (Property.run ~root ~path banded law) in
  let ran = List.filteri (fun i _ -> i <= p.case_index) (seen ()) in
  equal (list int)
    (List.init (p.case_index + 1) (fun index ->
         value_at ~root banded ~path ~index))
    ran

let determinism =
  group "Determinism"
    [
      prop ~count:25 "an outcome is a function of run's arguments" run_seed
        replays;
      prop ~count:25
        "case index samples Seed.derive ~root ~path ~index, discarded cases \
         included"
        run_seed derives;
      test "a failure records the run's root seed" (fun () ->
          let p =
            counterexample (Property.run ~root ~path:"root" banded band)
          in
          equal int64 root p.seed);
    ]

(* Laws *)

let escapes =
  let from_law e () =
    Property.run ~root ~path:"escape" Gen.int (fun _ _ -> raise e)
  in
  let from_gen e () =
    Property.run ~root ~path:"escape"
      (Gen.map (fun _ -> raise e) Gen.int)
      (fun _ _ -> ())
  in
  List.concat_map
    (fun (name, e) ->
      [
        ("the law's " ^ name, (e, from_law e));
        ("the generator's " ^ name, (e, from_gen e));
      ])
    [
      ("skip", Failure.Control (`Skip (Some "no data")));
      ("exit", Failure.Control `Exit);
      ("interrupt", Sys.Break);
      ("exhausted memory", Out_of_memory);
    ]

let timeouts =
  let from_gen () =
    let draws = ref 0 in
    let gen =
      Gen.map
        (fun n ->
          incr draws;
          if !draws = 3 then raise (timeout 0.5) else n)
        Gen.int
    in
    Property.run ~count:(`Config 50) ~root ~path:"gen timeout" gen (fun _ _ ->
        ())
  in
  let from_law () =
    Property.run ~root ~path:"timeout" Gen.int (fun _ _ -> raise (timeout 0.5))
  in
  let from_example () =
    Property.run ~examples:[ 1; 2 ] ~root ~path:"timeout" Gen.int (fun _ x ->
        if x = 2 then raise (timeout 0.5))
  in
  [
    ( "a generation",
      ( from_gen,
        "fail, 2 cases, 0 discards; timed out after 0.5s at case 2, 2 passed, \
         root c0ffee1234abcd, count 50" ) );
    ( "the first run of a case",
      ( from_law,
        "fail, 0 cases, 0 discards; timed out after 0.5s at case 0, 0 passed, \
         root c0ffee1234abcd, no count" ) );
    ( "an example",
      ( from_example,
        "fail, 1 cases, 0 discards; timed out after 0.5s at example 1, 1 \
         passed, root c0ffee1234abcd, no count" ) );
  ]

let backtrace recording =
  let saved = Printexc.backtrace_status () in
  Printexc.record_backtrace recording;
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace saved) @@ fun () ->
  let p =
    counterexample
      (Property.run ~root ~path:"inner backtrace" Gen.int (fun _ _ ->
           raise Not_found))
  in
  match p.inner with
  | Some { kind = Raise { backtrace = Some _; _ }; _ } -> "a backtrace"
  | Some _ | None -> "no backtrace"

let laws =
  group "Laws"
    [
      test "a law's Stack_overflow fails the case" (fun () ->
          let p =
            counterexample
              (Property.run ~root ~path:"overflow" Gen.int (fun _ _ ->
                   raise Stack_overflow))
          in
          equal string "raise Stack overflow" (inner_row p.inner));
      cases "a control or a fatal exception escapes run" ~name:fst escapes
        (fun (_, (e, run)) -> equal string (Printexc.to_string e) (escaped run));
      cases "a timeout before any case failed is a Fail naming the case"
        ~name:fst timeouts (fun (_, (run, row)) ->
          equal string row (outcome_row (run ())));
      cases "an inner raise carries the backtrace iff one is recorded" ~name:fst
        [
          ("recorded", (true, "a backtrace"));
          ("not recorded", (false, "no backtrace"));
        ]
        (fun (_, (recording, row)) -> equal string row (backtrace recording));
    ]

(* Discarding *)

let discarding =
  group "Discarding"
    [
      cases "assume and reject raise Failure.Control `Discard" ~name:fst
        [
          ("assume true", ((fun () -> Property.assume true), "returned"));
          ("assume false", ((fun () -> Property.assume false), "discard"));
          ("reject", (Property.reject, "discard"));
        ]
        (fun (_, (f, row)) ->
          let raised =
            match f () with
            | () -> "returned"
            | exception Failure.Control `Discard -> "discard"
          in
          equal string row raised);
    ]

(* Labelling *)

let classify_partitions () =
  let path = "classify" in
  let law ctx x =
    Property.classify ctx "even" (even x);
    Property.classify ctx "odd" (not (even x))
  in
  let s = stats (Property.run ~root ~path ~count:(`Declared 50) Gen.int law) in
  let evens =
    List.length
      (List.filter even
         (List.init 50 (fun index -> value_at Gen.int ~path ~index)))
  in
  equal
    (list (pair string int))
    [ ("even", evens); ("odd", 50 - evens) ]
    s.collected

let discarded_cases_commit_nothing () =
  let path = "discard labels" in
  let law ctx x =
    Property.collect ctx "attempt";
    Property.assume (even x)
  in
  let o =
    Property.run ~root ~path ~count:(`Declared 10) (Gen.int_range 0 9) law
  in
  let discards = discarded (Gen.int_range 0 9) ~path ~keep:even 10 in
  equal string (strf "pass, 10 cases, %d discards" discards) (verdict o);
  equal (list (pair string int)) [ ("attempt", 10) ] (stats o).collected

let failing_cases_commit_nothing () =
  let law ctx x =
    Property.collect ctx "ran";
    Check.is_true (x < 5)
  in
  let o =
    Property.run ~root ~path:"shrink labels" ~examples:[ 0; 1; 2 ]
      (Gen.int_range 0 100) law
  in
  let s = stats o in
  equal (list (pair string int)) [ ("ran", s.cases) ] s.collected

let late_context () =
  let kept = ref None in
  let late = Property.run ~root ~path:"late" (Gen.constant 0) in
  ignore (late (fun ctx _ -> kept := Some ctx) : Property.outcome);
  let ctx = require_some !kept in
  Property.collect ctx "late";
  Property.cover ctx "late cover" true;
  let s = stats (late (fun _ _ -> ())) in
  equal (list (pair string int)) [] s.collected;
  equal string "no demand" (coverage_row s)

(* Each row runs a law over [Gen.constant 1] with [0] as the examples it lists,
   so a label on zero is an example's. *)
let covers =
  let run ?(count = 10) ?(examples = []) law () =
    Property.run ~root ~path:"cover" ~count:(`Declared count) ~examples
      (Gen.constant 1) law
  in
  let zero ctx x = Property.cover ctx "zero" (x = 0) in
  [
    ( "a label one passing case marks",
      (run ~count:25 ~examples:[ 0 ] zero, "pass, 26 cases, 0 discards; zero 1")
    );
    ( "a label the examples mark",
      ( run ~count:2 ~examples:[ 0; 0; 0; 0 ] zero,
        "pass, 6 cases, 0 discards; zero 4" ) );
    ( "a label no passing case marks",
      (run zero, "coverage failed, 10 cases, 0 discards; zero 0 unsatisfied") );
    ( "a cover whose condition is false",
      ( run (fun ctx _ -> Property.cover ctx "never" false),
        "coverage failed, 10 cases, 0 discards; never 0 unsatisfied" ) );
    ( "a cover reached in a discarded case",
      ( run ~count:5 (fun ctx _ ->
            Property.cover ctx "discarded" true;
            Property.reject ()),
        "gave up, 0 cases, 11 discards; discarded 0 unsatisfied" ) );
    ( "a cover reached in a failing case",
      ( run (fun ctx _ ->
            Property.cover ctx "failed" true;
            Check.fail "always"),
        "fail, 0 cases, 0 discards; failed 0 unsatisfied" ) );
    ( "a cover no case reaches",
      ( run ~count:5 (fun ctx x ->
            if x = 0 then Property.cover ctx "unreached" true),
        "pass, 5 cases, 0 discards; no demand" ) );
    ( "no cover",
      (run ~count:0 (fun _ _ -> ()), "pass, 0 cases, 0 discards; no demand") );
  ]

let labelling =
  group "Labelling"
    [
      test "classify marks a label iff its condition holds, sorted by label"
        classify_partitions;
      test "a committed case counts a label once" (fun () ->
          let law ctx _ =
            Property.collect ctx "case";
            Property.collect ctx "case"
          in
          let o =
            Property.run ~root ~path:"once" ~count:(`Declared 10) Gen.int law
          in
          equal (list (pair string int)) [ ("case", 10) ] (stats o).collected);
      test "a discarded case commits no label" discarded_cases_commit_nothing;
      test "a failing case and the shrink search commit no label"
        failing_cases_commit_nothing;
      test "a context used after its run changes no other run" late_context;
      cases "a cover demand registers when reached and is judged at the end"
        ~name:fst covers (fun (_, (run, row)) ->
          let o = run () in
          equal string row (strf "%s; %s" (verdict o) (coverage_row (stats o))));
    ]

(* Outcomes *)

let loc = { Loc.file = "test/example.ml"; line = 41; column = 2 }

let located =
  [
    ("a property failure", Gen.int, fun _ x -> Check.equal Testable.int 0 x);
    ("a timeout", Gen.int, fun _ _ -> raise (timeout 0.5));
  ]

let loc_row (l : Loc.t option) =
  Option.fold ~none:"no loc"
    ~some:(fun (l : Loc.t) -> strf "%s:%d:%d" l.file l.line l.column)
    l

let summarized =
  let summary v = if v = 0 then None else Some (strf "n=%d" v) in
  let below_10 _ v = Check.is_true (v < 10) in
  let run ?summary ?examples law () =
    Property.run ?summary ?examples ~root ~path:"summary" (Gen.int_range 0 1000)
      law
  in
  [
    ("the counterexample's", (run ~summary below_10, Some "n=10"));
    ( "a failing example's",
      (run ~summary ~examples:[ 50 ] below_10, Some "n=50") );
    ( "a value the summary maps to None",
      (run ~summary (fun _ _ -> Check.fail "always"), None) );
    ("no summary", (run below_10, None));
  ]

let replayed count max_discard =
  let law _ x = Check.is_true (x < 700) in
  let o =
    Property.run ~count ?max_discard ~root ~path:"replay path"
      (Gen.int_range 0 1000) law
  in
  payload_row (counterexample o)

let configurations =
  [
    ("count 1000", (`Config 1_000, None));
    ("a declared count 5000, max_discard 7", (`Declared 5_000, Some 7));
    ("count 300, max_discard 0", (`Config 300, Some 0));
  ]

let outcomes =
  group "Outcomes"
    [
      cases "a Fail is located at loc"
        ~name:(fun (n, _, _) -> n)
        located
        (fun (_, gen, law) ->
          let f = failed (Property.run ~loc ~root ~path:"located" gen law) in
          equal string "test/example.ml:41:2" (loc_row f.loc));
      test "inner is the law's failure on the counterexample" (fun () ->
          let law _ x = Check.equal ~msg:"labelled" Testable.int 0 x in
          let p =
            counterexample (Property.run ~root ~path:"inner" Gen.int law)
          in
          equal string
            ("labelled: expected 0, actual " ^ p.rendered)
            (inner_row p.inner));
      cases "count is the count of the configuration only" ~name:fst
        [
          ("`Config 7", (Some (`Config 7), Some 7));
          ("`Declared 7", (Some (`Declared 7), None));
          ("the default", (None, None));
        ]
        (fun (_, (given, expected)) ->
          let o =
            Property.run ?count:given ~root ~path:"count" Gen.int (fun _ _ ->
                Check.fail "always")
          in
          equal (option int) expected (counterexample o).count);
      cases "summary is the summary of the reported value" ~name:fst summarized
        (fun (_, (run, expected)) ->
          equal (option string) expected (counterexample (run ())).summary);
      cases "a replay descends the same path whatever the configuration"
        ~name:fst configurations (fun (_, (count, max_discard)) ->
          equal string
            (replayed (`Config 100) None)
            (replayed count max_discard));
    ]

(* Examples *)

let examples_first () =
  let path = "examples order" in
  let gen = Gen.int_range 0 5 in
  let law, seen = traced (fun _ _ -> ()) in
  let o =
    Property.run ~root ~path ~count:(`Declared 3) ~examples:[ 1000; 2000 ] gen
      law
  in
  let generated = List.init 3 (fun index -> value_at gen ~path ~index) in
  equal (list int) ([ 1000; 2000 ] @ generated) (seen ());
  equal string "pass, 5 cases, 0 discards" (verdict o)

let failing_example () =
  let law, seen = traced (fun _ x -> if x = 7 then Check.fail "seven") in
  let o =
    Property.run ~root ~path:"example fail" ~examples:[ 3; 7; 11 ] Gen.int law
  in
  equal string
    "fail, 1 cases, 0 discards; example 1, 0 steps, converged: 7; message seven"
    (outcome_row o);
  equal (list int) [ 3; 7 ] (seen ())

let example_rendered gen =
  let o =
    Property.run ~root ~path:"example rendering" ~examples:[ 1; 2 ] gen
      (fun _ x -> if x = 2 then Check.fail "two")
  in
  rendered (counterexample o)

let examples_draw_no_seed () =
  let path = "examples draw nothing" in
  let first = value_at Gen.int ~path ~index:0 in
  let o =
    Property.run ~root ~path
      ~examples:[ first - 1; first + 1 ]
      Gen.int
      (fun _ v -> Check.is_true (v <> first))
  in
  let p = counterexample o in
  equal string "case 0" (case ~examples:p.examples p.case_index)

let examples =
  group "Examples"
    [
      test "the examples run first, in order, and count in cases" examples_first;
      test "a failing example ends the run, unshrunk, at its position"
        failing_example;
      cases "a failing example renders through the generator's printer"
        ~name:fst
        [
          ("a printer", (Gen.int, "2"));
          ("no printer", (Gen.map Fun.id Gen.int, placeholder));
        ]
        (fun (_, (gen, row)) -> equal string row (example_rendered gen));
      test "a discarding example counts in discards" (fun () ->
          let o =
            Property.run ~root ~path:"example discard" ~count:(`Declared 2)
              ~examples:[ 1; 2; 3 ] (Gen.constant 0) (fun _ x ->
                Property.assume (x <> 2))
          in
          equal string "pass, 4 cases, 1 discards" (verdict o));
      test "the first generated case is index 0 whatever the examples"
        examples_draw_no_seed;
    ]

(* Generated cases *)

let give_ups =
  let run ?max_discard ?(examples = []) ~count gen law () =
    Property.run ~root ~path:"give up" ~count:(`Declared count) ?max_discard
      ~examples gen law
  in
  let discard _ _ = Property.reject () in
  let not_one _ x = Property.assume (x <> 1) in
  [
    ( "the default is twice the count",
      (run ~count:10 Gen.int discard, "gave up, 0 cases, 21 discards") );
    ( "an explicit max_discard",
      ( run ~count:5 ~max_discard:7 (Gen.int_range 0 9) discard,
        "gave up, 0 cases, 8 discards" ) );
    ( "a max_discard of 0 and a discard",
      ( run ~count:5 ~max_discard:0 Gen.int discard,
        "gave up, 0 cases, 1 discards" ) );
    ( "a max_discard of 0 and no discard",
      ( run ~count:5 ~max_discard:0 Gen.int (fun _ _ -> ()),
        "pass, 5 cases, 0 discards" ) );
    ( "discarding examples",
      ( run ~count:3 ~max_discard:2 ~examples:[ 1; 1; 1 ] (Gen.constant 0)
          not_one,
        "gave up, 0 cases, 3 discards" ) );
    ( "passing examples",
      ( run ~count:3 ~max_discard:0 ~examples:[ 1; 2; 3 ] (Gen.constant 0)
          (fun _ _ -> ()),
        "pass, 6 cases, 0 discards" ) );
    ( "a count of 0 already met",
      ( run ~count:0 ~max_discard:0 ~examples:[ 1 ] (Gen.constant 0) not_one,
        "gave up, 0 cases, 1 discards" ) );
  ]

let mixed_discards () =
  let path = "mixed discards" in
  let gen = Gen.int_range 0 9 in
  let o =
    Property.run ~root ~path ~count:(`Declared 20) gen (fun _ x ->
        Property.assume (even x))
  in
  let discards = discarded gen ~path ~keep:even 20 in
  equal string (strf "pass, 20 cases, %d discards" discards) (verdict o)

let generator_discards () =
  let path = "assume in map" in
  let odd_discarded =
    Gen.map
      (fun n ->
        Property.assume (even n);
        n)
      Gen.int
  in
  let o = Property.run ~root ~path odd_discarded (fun _ _ -> ()) in
  let discards = discarded Gen.int ~path ~keep:even 100 in
  equal string (strf "pass, 100 cases, %d discards" discards) (verdict o)

let spent_index discarding =
  let first = ref true in
  let discard_first () =
    if !first then (
      first := false;
      Property.reject ())
  in
  let gen, law = discarding discard_first in
  let p = counterexample (Property.run ~root ~path:"discard index" gen law) in
  p.case_index

let spenders =
  [
    ( "by the law",
      fun discard_first ->
        ( Gen.int,
          fun _ _ ->
            discard_first ();
            Check.fail "fails" ) );
    ( "by the generator",
      fun discard_first ->
        ( Gen.map
            (fun x ->
              discard_first ();
              x)
            Gen.int,
          fun _ _ -> Check.fail "fails" ) );
  ]

let raising_generators =
  let raising e = Gen_engine.make (fun _ -> raise e) in
  [
    ("Not_found", (raising Not_found, "raise Not_found"));
    ( "an assertion",
      ( raising (Failure.Check_failure (Failure.message "from the generator")),
        "message from the generator" ) );
    ( "a malformed generator",
      ( Gen.int_range 5 1,
        {|raise Invalid_argument("Gen.int_range: high < low")|} ) );
  ]

let generated =
  group "Generated cases"
    [
      test "a count of 0 runs no generated case" (fun () ->
          let law, seen = traced (fun _ _ -> ()) in
          let o =
            Property.run ~root ~path:"zero" ~count:(`Declared 0) Gen.int law
          in
          equal string "pass, 0 cases, 0 discards" (verdict o);
          equal (list int) [] (seen ()));
      cases "a count or max_discard below 0 raises Invalid_argument" ~name:fst
        [
          ( "count",
            fun () ->
              Property.run ~root ~path:"bad" ~count:(`Declared (-1)) Gen.int
                (fun _ _ -> ()) );
          ( "max_discard",
            fun () ->
              Property.run ~root ~path:"bad" ~max_discard:(-1) Gen.int
                (fun _ _ -> ()) );
        ]
        (fun (_, run) -> raises_match (fun e -> Exn.invalid_arg e) run);
      test "the default max_discard is clamped to max_int" (fun () ->
          let o =
            Property.run ~root ~path:"huge count"
              ~count:(`Declared ((max_int / 2) + 1))
              (Gen.constant 0)
              (fun _ _ -> Check.fail "first")
          in
          equal string "fail, 0 cases, 0 discards" (verdict o));
      cases "the run gives up once more than max_discard cases are discarded"
        ~name:fst give_ups (fun (_, (run, row)) ->
          equal string row (verdict (run ())));
      test "discards within max_discard leave the run to pass" mixed_discards;
      cases "a discard spends its index" ~name:fst spenders
        (fun (_, discarding) -> equal int 1 (spent_index discarding));
      test "a generation that discards discards the case" generator_discards;
      test "a generation that always discards gives up" (fun () ->
          let law, seen = traced (fun _ _ -> ()) in
          let never = Gen.such_that (fun _ -> false) Gen.int in
          let o =
            Property.run ~root ~path:"never" ~count:(`Declared 3) never law
          in
          equal string "gave up, 0 cases, 7 discards" (verdict o);
          equal (list int) [] (seen ()));
      cases "a generator's fault fails the case unshrunk" ~name:fst
        raising_generators (fun (_, (gen, inner)) ->
          let o = Property.run ~root ~path:"raising gen" gen (fun _ _ -> ()) in
          equal string
            (strf
               "fail, 0 cases, 0 discards; case 0, 0 steps, converged: %s; %s"
               unproduced inner)
            (outcome_row o));
    ]

(* Shrinking *)

let minimal () =
  let p =
    counterexample
      (Property.run ~root ~path:"shrink minimal" Gen.int (fun _ x ->
           Check.equal Testable.int 0 x))
  in
  mem string p.rendered [ "-1"; "1" ];
  at_least int ~than:1 p.steps;
  equal string ("expected 0, actual " ^ p.rendered) (inner_row p.inner)

(* The root is [3], its candidates [1] then [2], and [2] has none. The law
   fails on [3] and on [2] in the class of [first], and does [candidate] on
   [1]. *)
let search first candidate =
  let law _ = function
    | 3 -> first "first"
    | 1 -> candidate ()
    | _ -> first "at 2"
  in
  outcome_row
    (Property.run ~root ~path:"search"
       (drawn (node 3 [ node 1 []; node 2 [] ]))
       law)

let assertion what = Check.fail what
let exception_ what = if what = "first" then raise Exit else raise Not_found
let rejected = "fail, 0 cases, 0 discards; case 0, 1 steps, converged: 2; "
let oracle what = raise (Property.Oracle_failure (Failure.message what))

let candidates =
  [
    ("an assertion, then a pass", (assertion, ignore, rejected ^ "message at 2"));
    ( "an assertion, then a discard",
      (assertion, Property.reject, rejected ^ "message at 2") );
    ( "an assertion, then a skip",
      ( assertion,
        (fun () -> raise (Failure.Control (`Skip (Some "trap")))),
        rejected ^ "message at 2" ) );
    ( "an assertion, then an exit",
      ( assertion,
        (fun () -> raise (Failure.Control `Exit)),
        rejected ^ "message at 2" ) );
    ( "an assertion, then an exception",
      (assertion, (fun () -> raise Exit), rejected ^ "message at 2") );
    ( "an assertion, then another assertion",
      ( assertion,
        (fun () -> Check.fail "at 1"),
        "fail, 0 cases, 0 discards; case 0, 1 steps, converged: 1; message at 1"
      ) );
    ( "an exception, then an assertion",
      (exception_, (fun () -> Check.fail "at 1"), rejected ^ "raise Not_found")
    );
    ( "an assertion, then a broken oracle",
      (assertion, (fun () -> oracle "at 1"), rejected ^ "message at 2") );
    ( "an exception, then a broken oracle",
      (exception_, (fun () -> oracle "at 1"), rejected ^ "raise Not_found") );
    ( "a broken oracle, then an assertion",
      (oracle, (fun () -> Check.fail "at 1"), rejected ^ "message at 2") );
    ( "a broken oracle, then an exception",
      (oracle, (fun () -> raise Exit), rejected ^ "message at 2") );
    ( "a broken oracle, then another",
      ( oracle,
        (fun () -> oracle "at 1"),
        "fail, 0 cases, 0 discards; case 0, 1 steps, converged: 1; message at 1"
      ) );
    ( "an exception, then another exception",
      ( exception_,
        (fun () -> raise Not_found),
        "fail, 0 cases, 0 discards; case 0, 1 steps, converged: 1; raise \
         Not_found" ) );
  ]

let budget_runs () =
  let wide = 999 and depth = 50 in
  let rec tree k =
    let passing = Seq.init wide (fun i -> Shrink_tree.leaf (-i - 1)) in
    let failing = if k = 0 then Seq.empty else Seq.return (tree (k - 1)) in
    Shrink_tree.make ~root:k ~children:(Seq.append passing failing)
  in
  let runs = ref 0 in
  let law _ n =
    incr runs;
    if n >= 0 then raise Not_found
  in
  let o = Property.run ~root ~path:"wide" (drawn (tree depth)) law in
  equal string
    "fail, 0 cases, 0 discards; case 0, 10 steps, budget spent: 40; raise \
     Not_found"
    (outcome_row o);
  (* The case's first run, the budget, and the run again. *)
  equal int (Property.shrink_budget + 2) !runs

let quad () =
  let gen = Gen.quad Gen.int64 Gen.int64 Gen.int64 Gen.int64 in
  let far x = Int64.compare (Int64.abs x) 3L >= 0 in
  let law _ (a, b, c, d) =
    if far a && far b && far c && far d then raise Exit
  in
  let p = counterexample (Property.run ~root ~path:"quad" gen law) in
  equal string "converged" (ending p.ending);
  at_most int ~than:256 p.steps

let raising_candidate () =
  let tree =
    Shrink_tree.make ~root:20 ~children:(fun () -> failwith "forcing raised")
  in
  let o =
    Property.run ~root ~path:"raising" (drawn tree) (fun _ _ ->
        Check.fail "always")
  in
  equal string
    {|fail, 0 cases, 0 discards; case 0, 0 steps, candidate raised Failure("forcing raised"): 20; message always|}
    (outcome_row o)

let timed_out_searches =
  let first_candidate calls _ =
    if calls = 1 then Check.fail "original" else raise (timeout 0.25)
  in
  let fourth_run calls x =
    if calls >= 4 then raise (timeout 0.1) else if x >= 10 then Check.fail "big"
  in
  let run tree law () =
    let calls = ref 0 in
    let counted _ x =
      incr calls;
      law !calls x
    in
    Property.run ~root ~path:"timeout" (drawn tree) counted
  in
  [
    ( "on the first candidate",
      ( run (node 3 [ node 1 [] ]) first_candidate,
        "fail, 0 cases, 0 discards; case 0, 0 steps, timed out after 0.25s: 3; \
         message original" ) );
    ( "while a candidate is forced",
      ( run
          (Shrink_tree.make ~root:3 ~children:(fun () -> raise (timeout 0.3)))
          (fun _ _ -> Check.fail "original"),
        "fail, 0 cases, 0 discards; case 0, 0 steps, timed out after 0.3s: 3; \
         message original" ) );
    ( "after accepted steps",
      ( run (node 40 [ node 20 [ node 10 [ node 5 [] ] ] ]) fourth_run,
        "fail, 0 cases, 0 discards; case 0, 2 steps, timed out after 0.1s: 10; \
         message big" ) );
  ]

let formatting_timeout () =
  let gen =
    Gen_engine.make
      ~pp:(fun _ _ -> raise (timeout 0.25))
      (fun s -> (node 3 [], s))
  in
  let o =
    Property.run ~root ~path:"formatting" gen (fun _ _ -> Check.fail "always")
  in
  equal string
    (strf
       "fail, 0 cases, 0 discards; case 0, 0 steps, converged: <printer raised \
        %s>; message always"
       (Printexc.to_string (timeout 0.25)))
    (outcome_row o)

let renderings =
  let mapped = Gen.map (fun x -> x * 2) (Gen.int_range 0 50) in
  [
    ( "a printerless value",
      ("placeholder", Gen.map (fun x -> x * 2) (Gen.constant 7), placeholder) );
    ("a printerless map", ("pre-image", mapped, "5 (pre-image)"));
    ( "a map with a printer",
      ("pre-image", Gen.with_pp Format.pp_print_int mapped, "10") );
  ]

let shrinking =
  group "Shrinking"
    [
      test "shrink_budget is 10_000" (fun () ->
          equal int 10_000 Property.shrink_budget);
      test "the search descends to a node with no failing candidate" minimal;
      cases
        "a candidate is accepted iff it fails in the class of the first failure"
        ~name:fst candidates (fun (_, (first, candidate, row)) ->
          equal string row (search first candidate));
      cases "a search that spends the budget ends at the next candidate"
        ~name:fst
        [
          ("a candidate left", (Property.shrink_budget + 1, "budget spent: 1"));
          ("no candidate left", (Property.shrink_budget, "converged: 0"));
        ]
        (fun (_, (n, row)) ->
          let o =
            Property.run ~root ~path:"budget"
              (drawn (chain n))
              (fun _ _ -> raise Not_found)
          in
          equal string
            (strf
               "fail, 0 cases, 0 discards; case 0, 10000 steps, %s; raise \
                Not_found"
               row)
            (outcome_row o));
      cases "a law whose run costs 50 spends the budget in 200 runs" ~name:fst
        [
          ("a candidate left", (201, "200 steps, budget spent: 1"));
          ("no candidate left", (200, "200 steps, converged: 0"));
        ]
        (fun (_, (n, row)) ->
          let o =
            Property.run ~cost:50 ~root ~path:"budget"
              (drawn (chain n))
              (fun _ _ -> raise Not_found)
          in
          equal string
            (strf "fail, 0 cases, 0 discards; case 0, %s; raise Not_found" row)
            (outcome_row o));
      test "a cost below 1 raises" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Property.run ~cost:0 ~root ~path:"budget"
                (drawn (chain 1))
                (fun _ _ -> ())));
      test "a candidate that discards costs one run, whatever the cost"
        (fun () ->
          let discarding = Seq.init 300 (fun _ -> Shrink_tree.leaf (-1)) in
          let tree =
            Shrink_tree.make ~root:0
              ~children:
                (Seq.append discarding (Seq.return (Shrink_tree.leaf 1)))
          in
          let o =
            Property.run ~cost:50 ~root ~path:"budget" (drawn tree) (fun _ n ->
                if n < 0 then Property.reject ();
                raise Not_found)
          in
          equal string
            "fail, 0 cases, 0 discards; case 0, 1 steps, converged: 1; raise \
             Not_found"
            (outcome_row o));
      test "the budget counts every candidate probed" budget_runs;
      test "a quad of int64 converges within 256 steps" quad;
      test "a candidate whose forcing raises ends the search" raising_candidate;
      cases "a timeout in the search ends it at the last accepted node"
        ~name:fst timed_out_searches (fun (_, (run, row)) ->
          equal string row (outcome_row (run ())));
      test "a timeout while formatting does not leave run" formatting_timeout;
      cases "the counterexample renders as its value or its pre-image" ~name:fst
        renderings (fun (_, (path, gen, row)) ->
          let o =
            Property.run ~root ~path gen (fun _ x -> Check.is_true (x < 10))
          in
          equal string row (rendered (counterexample o)));
    ]

(* Output *)

(* The outcome of [law] on [gen] as a row, its failure's output and the
   values whose output the engine asked for, under an [output] that is the
   value of the last run. With [interrupt] as [(n, e)], [output] raises [e]
   at its [n]th call. *)
let echoed ?examples ?interrupt gen law =
  let last = ref None and asked = ref [] in
  let run ctx x =
    last := Some x;
    law ctx x
  in
  let output () =
    let x = require_some !last in
    (match interrupt with
    | Some (n, e) when List.length !asked + 1 = n -> raise e
    | Some _ | None -> ());
    asked := x :: !asked;
    Some (Failure.tail (string_of_int x))
  in
  let o = Property.run ?examples ~output ~root ~path:"output" gen run in
  ( outcome_row o,
    Option.map (fun (t : Failure.tail) -> t.text) (failed o).output_tail,
    List.rev !asked )

let at_least_10 _ x = if x >= 10 then Check.fail "big"

(* [40] fails, its candidate [5] passes and [20] fails, and [20]'s candidate
   [10] fails and has none. *)
let descent = drawn (node 40 [ node 5 []; node 20 [ node 10 []; node 15 [] ] ])
let outcome_asked = triple string (option string) (list int)

let output =
  group "Output"
    [
      test "a failure carries the output of the run on its counterexample"
        (fun () ->
          equal outcome_asked
            ( "fail, 0 cases, 0 discards; case 0, 2 steps, converged: 10; \
               message big",
              Some "10",
              [ 40; 20; 10 ] )
            (echoed descent at_least_10));
      test "a failing example carries the output of its run" (fun () ->
          equal outcome_asked
            ( "fail, 1 cases, 0 discards; example 1, 0 steps, converged: 42; \
               message big",
              Some "42",
              [ 42 ] )
            (echoed ~examples:[ 3; 42; 50 ] descent at_least_10));
      test "a generator that raises carries no output" (fun () ->
          equal outcome_asked
            ( strf
                "fail, 0 cases, 0 discards; case 0, 0 steps, converged: %s; \
                 raise Failure(\"drawn\")"
                unproduced,
              None,
              [] )
            (echoed (Gen.map (fun _ -> failwith "drawn") Gen.int) at_least_10));
      test
        "a timeout while the output is read ends the search at the node \
         before, with its output" (fun () ->
          equal outcome_asked
            ( "fail, 0 cases, 0 discards; case 0, 1 steps, timed out after \
               0.25s: 20; message big",
              Some "20",
              [ 40; 20 ] )
            (echoed ~interrupt:(3, timeout 0.25) descent at_least_10));
      test
        "a timeout while the failing case's output is read ends the search \
         there, without output" (fun () ->
          equal outcome_asked
            ( "fail, 0 cases, 0 discards; case 0, 0 steps, timed out after \
               0.25s: 40; message big",
              None,
              [] )
            (echoed ~interrupt:(1, timeout 0.25) descent at_least_10));
      test "a timeout while a failing example's output is read leaves no output"
        (fun () ->
          equal outcome_asked
            ( "fail, 1 cases, 0 discards; example 1, 0 steps, converged: 42; \
               message big",
              None,
              [] )
            (echoed ~examples:[ 3; 42; 50 ]
               ~interrupt:(1, timeout 0.25)
               descent at_least_10));
    ]

(* Running again *)

(* A law that fails on the first run on each value of at least 10, and does
   [later] on any later run on a value. *)
let first_sight later =
  let seen = Hashtbl.create 8 in
  fun _ x ->
    if Hashtbl.mem seen x then later ()
    else begin
      Hashtbl.replace seen x ();
      if x >= 10 then Check.fail "big"
    end

let failed_again o =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with Property p -> Some p.failed_again | _ -> None)
    (failed o)

(* The outcome of [law] on [gen] as a row, its failure's [failed_again], and
   the values that the law ran on. *)
let ran_again ?deterministic ?examples gen law =
  let law, seen = traced law in
  let o = Property.run ?deterministic ?examples ~root ~path:"again" gen law in
  (outcome_row o, failed_again o, seen ())

let converged_at_10 =
  "fail, 0 cases, 0 discards; case 0, 2 steps, converged: 10; message big"

let ran_again_row = triple string (option bool) (list int)

let reruns =
  [
    ("a law that fails again", (at_least_10, Some true));
    ("a law that passes", (first_sight ignore, Some false));
    ("a law that discards", (first_sight Property.reject, Some false));
    ( "a law that raises a control",
      (first_sight (fun () -> raise (Failure.Control `Exit)), Some false) );
    ( "a law that fails in another class",
      (first_sight (fun () -> raise Exit), Some true) );
    ( "a law that the test's limit cuts",
      (first_sight (fun () -> raise (timeout 0.25)), None) );
  ]

let timed_out_search () =
  let calls = ref 0 in
  let law _ x =
    incr calls;
    if !calls >= 3 then raise (timeout 0.25) else at_least_10 () x
  in
  equal ran_again_row
    ( "fail, 0 cases, 0 discards; case 0, 0 steps, timed out after 0.25s: 40; \
       message big",
      None,
      [ 40; 5; 20 ] )
    (ran_again descent law)

(* A law that sorts its array in place and fails when the array was not
   sorted, a function of the value it is given that changes that value. *)
let sorts_in_place _ a =
  let given = Array.copy a in
  Array.sort Int.compare a;
  if a <> given then Check.fail "unsorted"

let changed_in_place () =
  let o =
    Property.run ~root ~path:"again" Gen.(array (int_range 0 9)) sorts_in_place
  in
  equal
    (pair string (option bool))
    ( "fail, 0 cases, 0 discards; case 0, 7 steps, converged: [|1; 0|]; \
       message unsorted",
      Some true )
    (outcome_row o, failed_again o)

(* [40] fails, its candidate [3] discards and [20] fails, and [20]'s
   candidate [1] discards and [10] fails. The run again follows the accepted
   candidates, the second of their siblings, and not the discarding ones. *)
let past_discards () =
  let law _ x = if x < 5 then Property.reject () else at_least_10 () x in
  equal ran_again_row
    (converged_at_10, Some true, [ 40; 3; 20; 1; 10; 10 ])
    (ran_again
       (drawn (node 40 [ node 3 []; node 20 [ node 1 []; node 10 [] ] ]))
       law)

let running_again =
  group "Running again"
    [
      cases "the counterexample runs once more after the search" ~name:fst
        reruns (fun (_, (law, again)) ->
          equal ran_again_row
            (converged_at_10, again, [ 40; 5; 20; 10; 10 ])
            (ran_again descent law));
      test "a failing example does not run again" (fun () ->
          equal ran_again_row
            ( "fail, 1 cases, 0 discards; example 1, 0 steps, converged: 42; \
               message big",
              None,
              [ 3; 42 ] )
            (ran_again ~examples:[ 3; 42 ] descent (first_sight ignore)));
      test "a law that is not deterministic does not run again" (fun () ->
          equal ran_again_row
            (converged_at_10, None, [ 40; 5; 20; 10 ])
            (ran_again ~deterministic:false descent (first_sight ignore)));
      test "a search that a timeout ended does not run again" timed_out_search;
      test "a generator that raised runs nothing again" (fun () ->
          equal ran_again_row
            ( strf
                "fail, 0 cases, 0 discards; case 0, 0 steps, converged: %s; \
                 raise Failure(\"drawn\")"
                unproduced,
              None,
              [] )
            (ran_again
               (Gen.map (fun _ -> failwith "drawn") Gen.int)
               at_least_10));
      test
        "a law that changes its value fails again on the counterexample drawn \
         again"
        changed_in_place;
      test
        "a counterexample reached past discarding candidates fails again on it"
        past_discards;
    ]

let () =
  exit
    (run "property"
       [
         determinism;
         laws;
         discarding;
         labelling;
         outcomes;
         examples;
         generated;
         shrinking;
         output;
         running_again;
       ])
