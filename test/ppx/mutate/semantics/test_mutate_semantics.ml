(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Guarantee 12: with nothing armed, a mutation-instrumented program is
   observationally identical to an uninstrumented one - same evaluation
   order, tail-call status, laziness, outcomes, counts and exit code.
   This is the mandatory suite that guarantee names, and it is enforced by
   it or not at all.

   Two fixture libraries, one source tree. [Mutsem_fixtures] is
   covsem_fixtures.ml (shared with the coverage semantics suite by
   copy_files), mutsem_order.ml and mutsem_boom.ml, all three put through
   [ppx_windtrap.mutate]; [Mutsem_baseline] is the SAME three sources
   with no rewriter at all. This executable is itself uninstrumented, so
   every comparison below is between an instrumented program and a real
   uninstrumented twin rather than against an expectation a maintainer
   could edit to match a defect. mutsem_boom.ml is not run here - it is
   the exit-code fixture, which dune runs as two executables - but it is
   linked and catalogued here, which is the only place anything can check
   that the copy those executables compare IS the instrumented one.

   What each family proves, and what it does not:

   - Operand order is the reason this suite exists. [cmp] and [ari]
     lift their operands into one tuple binding before branching -
     typed left to right, which is what preserves the user's
     type-directed record disambiguation - and the instrumenter's
     manual claims the literal tuple is destructured without being
     built, into "the order the compiler gives the uninstrumented
     application". OCaml specifies neither argument nor tuple-component
     evaluation order, so that claim is about the compiler in the
     build. Two tests carry it: one pins the uninstrumented twin's
     order (the premise), the other compares the twin against the
     instrumented copy (the conclusion). If a compiler ever evaluates
     the two forms in different orders, the first goes red naming the
     premise and the second goes red naming the divergent expression.
   - Tail position is MEASURED, with [Printexc.get_callstack], not hoped
     for: OCaml 5 grows a fibre's stack on demand, so a lost tail call
     does not overflow at any depth a test can afford - a non-tail
     recursion fifty million frames deep returns normally on this build.
     The fixture carries a deliberately non-tail control so the
     measurement is shown to bite before it is trusted, and there is no
     deep-recursion test: recursing far proves the answer is right at
     scale, which the outcomes test already establishes at a thousand
     levels, and nothing else.
   - The differential is shown NOT to be vacuous: every mutant of
     mutsem_order.ml is armed in turn and the same battery replayed, so
     a build in which the rewriter quietly emitted nothing - which would
     make every comparison below pass - fails here instead.
   - The registry assertions are the coverage suite's, in the mutation
     dialect: registration at module load, no site evaluated before the
     first call, reach counts that match the number of evaluations, and
     nothing armed from first line to last.

   A windtrap suite ([run] executes tests sequentially in declaration
   order); the reach-map assertions and [lazy_witness] observe shared
   in-process state, so the tests are order-dependent - run the suite
   whole, not filtered. *)

open Windtrap
module I = Mutsem_fixtures.Mutsem_order
module B = Mutsem_baseline.Mutsem_order
module F = Mutsem_fixtures.Covsem_fixtures
module U = Mutsem_baseline.Covsem_fixtures
module M = Windtrap_runtime.Mutate

(* Types are part of "observationally identical". These coercions are
   checked when this file compiles: an instrumented module must still
   satisfy its uninstrumented twin's signature, so no instrumented
   binding may have lost a type variable to the value restriction or
   changed shape. They are one-directional on purpose - instrumentation
   ADDS the generated registration module, so the reverse coercion is
   expected to fail.

   That asymmetry is also what keeps the twin honest, and it is the only
   check in this file that does. The rewriter names its generated module
   after the SOURCE PATH, so an accidentally instrumented baseline grows
   a [Windtrap_mut___..._baseline_...] the instrumented copy does not
   have, and these three lines stop compiling. Nothing at run time could
   tell: a mutant identifier records the path, but the registry
   assertions below can only compare what they are given, and a suite
   comparing an instrumented program against another instrumented
   program would otherwise pass every test here. Verified by putting
   [(preprocess (pps ppx_windtrap.mutate))] on baseline/dune's library:
   the error names
   [Windtrap_mut___test___ppx___mutate___semantics___baseline___covsem_fixtures___ml]
   as required but not provided. *)
module _ : module type of Mutsem_baseline.Covsem_fixtures =
  Mutsem_fixtures.Covsem_fixtures

module _ : module type of Mutsem_baseline.Mutsem_order =
  Mutsem_fixtures.Mutsem_order

module _ : module type of Mutsem_baseline.Mutsem_boom =
  Mutsem_fixtures.Mutsem_boom

(* {1 The battery} *)

(* Running a witness under an armed mutant may raise - a mutated
   comparison can turn a bounded loop into one the runaway budget stops -
   so an escaping exception is part of the observation rather than a
   crash. With nothing armed none of these fire, which is itself the
   claim.

   [reset] empties the fixture copy's trace log before each witness. A
   witness that raises never reaches its own [show], so without this the
   tags it left would leak into the next one - and, worse, out of an
   armed replay into the disarmed one, where they would read as a
   semantics difference that is really the test's own residue. *)
let run_battery reset ws =
  List.map
    (fun (name, f) ->
      ignore (reset () : string);
      match f () with
      | s -> (name, s)
      | exception e -> (name, "raised " ^ Printexc.to_string e))
    ws

let instrumented_battery () = run_battery I.trace I.witnesses
let witness = pair string string
let baseline_battery = lazy (run_battery B.trace B.witnesses)

(* {1 The registry} *)

(* [mutants_of basename] is the catalogued mutants of the one registered
   source called [basename]. It fails rather than filters when two
   registered paths share a basename: the baseline library compiles files
   of the same names one directory down, so a basename match would
   quietly pool an instrumented fixture with an instrumented twin and the
   vacuity test below would report a live differential built from both. *)
let mutants_of basename =
  let ms =
    List.filter
      (fun (m : M.mutant) -> Filename.basename m.id.file = basename)
      (M.catalogue ())
  in
  (match
     List.sort_uniq compare (List.map (fun (m : M.mutant) -> m.id.file) ms)
   with
  | [] -> failf "no registered source is called %s" basename
  | [ _ ] -> ()
  | paths ->
      failf "%s names %d registered sources: %s" basename (List.length paths)
        (String.concat ", " paths));
  ms

let family (m : M.mutant) =
  match m.id.rewrite with
  | "not" -> "neg"
  | "lt" | "le" | "gt" | "ge" | "eq" | "neq" -> "cmp"
  | "and" | "or" -> "con"
  | "add" | "sub" | "fadd" | "fsub" -> "ari"
  | other -> "unexpected:" ^ other

(* The registry is process-global. Under --instrument-with the windtrap
   core this executable links is itself mutation-instrumented, and its
   sites drain into the same window as the fixtures'. Every claim in this
   suite is about the three fixture sources, so the drain is read through
   this rather than raw — the same discipline the coverage twin applies
   to its snapshot. Basenames, because the fixtures' recorded paths are
   this directory's and the baseline library's are one directory down;
   the path assertions below still check the whole path. *)
let fixture_sources =
  [ "covsem_fixtures.ml"; "mutsem_order.ml"; "mutsem_boom.ml" ]

let drain_fixtures () =
  List.filter
    (fun (r : M.reached) ->
      List.mem (Filename.basename r.M.mutant.M.id.M.file) fixture_sources)
    (M.drain ())

let fixture_catalogue () =
  List.filter
    (fun (m : M.mutant) ->
      List.mem (Filename.basename m.M.id.M.file) fixture_sources)
    (M.catalogue ())

(* [reached f] is [f]'s result and the mutants its evaluation reached,
   as (rewrite, hits) pairs in catalogue order. The drain before the
   epoch bump discards whatever earlier tests marked. *)
let reached f =
  ignore (M.drain ());
  M.next_epoch ();
  let v = f () in
  let rs = drain_fixtures () in
  (v, List.map (fun (r : M.reached) -> (r.mutant.id.rewrite, r.hits)) rs)

let reach = list (pair string int)

let tests =
  [
    (* {1 Registration and inertness} *)
    test "registration happens at module load, before any call" (fun () ->
        let catalogue = fixture_catalogue () in
        is_true ~msg:"the fixtures registered at load" (catalogue <> []);
        is_true ~msg:"nothing is armed" (M.armed () = None);
        (* The mutation dialect of coverage's "no point visited before any
           call": a site marks itself the first time it is evaluated, and
           no fixture's module initialization evaluates one - not even
           the lazy bodies, which is half of what the
           laziness test below re-checks from the other side. *)
        equal ~msg:"no site was evaluated before the first call" reach []
          (List.map
             (fun (r : M.reached) -> (r.mutant.id.rewrite, r.hits))
             (drain_fixtures ()));
        (* Exactly the three instrumented sources register, each once.
           Whole PATHS, not basenames: the baseline library compiles
           files of the same three names one directory down, so a
           basename comparison would read the same three entries whether
           or not the twin had been instrumented by accident - and would
           say nothing while looking like it said everything. The
           coercions at the top of this file are what actually catch that
           case; this assertion catches the complementary one, a source
           that stopped being instrumented, and mutsem_boom.ml is here
           because it is the only fixture no other test in this file
           runs. *)
        let paths =
          List.sort_uniq compare
            (List.map (fun (m : M.mutant) -> m.id.file) catalogue)
        in
        equal ~msg:"exactly three sources registered" int 3 (List.length paths);
        equal ~msg:"and they are the three instrumented ones" (list string)
          [ "covsem_fixtures.ml"; "mutsem_boom.ml"; "mutsem_order.ml" ]
          (List.map Filename.basename paths);
        equal ~msg:"the three share one directory" int 1
          (List.length
             (List.sort_uniq compare (List.map Filename.dirname paths)));
        is_true ~msg:"and it is not baseline/"
          (List.for_all
             (fun p -> Filename.basename (Filename.dirname p) <> "baseline")
             paths));
    test "the catalogue is well formed and covers all four operators" (fun () ->
        (* Not asserted here, deliberately: that every [rewrite] is in the
           vocabulary, that [line >= 1], that [col >= 0], that the span is
           ordered. [register] raises [Invalid_argument] on each of those,
           at module load, before this suite's first test runs - so a
           check for them could be read but never observed false, and a
           test that cannot fail is a comment with a runtime cost. What is
           left below is what the runtime does not police. *)
        let catalogue = fixture_catalogue () in
        List.iter
          (fun (m : M.mutant) ->
            is_true ~msg:"the before rendering is non-empty" (m.before <> "");
            is_true ~msg:"the after rendering is non-empty" (m.after <> "");
            is_true ~msg:"the two renderings differ" (m.before <> m.after);
            is_true ~msg:"nothing in these fixtures is dismissed"
              (m.dismissed = None))
          catalogue;
        let families = List.sort_uniq compare (List.map family catalogue) in
        equal ~msg:"all four operator families are represented" (list string)
          [ "ari"; "cmp"; "con"; "neg" ]
          families;
        (* A mutant identifier must name at most one site. *)
        let ids =
          List.map (fun (m : M.mutant) -> M.id_to_string m.id) catalogue
        in
        equal ~msg:"every mutant identifier is unique" int (List.length ids)
          (List.length (List.sort_uniq compare ids)));
    (* {1 Operand order: the premise, then the conclusion} *)
    test "the compiler evaluates operands right to left" (fun () ->
        (* The premise the instrumenter's fixed binding order relies on,
           read off the UNINSTRUMENTED twin. If a future compiler
           evaluates left to right, this test names the change and the
           next one names the damage. *)
        equal ~msg:"a < b evaluates b, then a" string "t | r,l"
          (B.show (B.cmp_lt 1 2));
        equal ~msg:"a + b evaluates b, then a" string "3 | r,l"
          (B.show (B.ari_add 1 2));
        equal ~msg:"a +. b evaluates b, then a" string "3.75 | r,l"
          (B.show (B.ari_fadd 1.5 2.25));
        equal ~msg:"the right operand's exception is the one that escapes"
          string "r | r"
          (B.show (B.cmp_exception_order ()));
        equal ~msg:"and again for arithmetic" string "r | r"
          (B.show (B.ari_exception_order ()));
        (* [=] and [<>] take the encoding that binds the whole comparison
           rather than its operands, so their order is the compiler's
           either way; pinned here so a change is visible on both sides. *)
        equal ~msg:"a = b evaluates b, then a" string "f | r,l"
          (B.show (B.cmp_eq 1 2)));
    test "the instrumented program evaluates exactly as its twin does"
      (fun () ->
        let instrumented = instrumented_battery () in
        let baseline = Lazy.force baseline_battery in
        equal ~msg:"the twins carry the same number of witnesses" int
          (List.length baseline) (List.length instrumented);
        List.iter2
          (fun i b ->
            (* Names first: a mismatch there means the two copies are not
               the same source and nothing below is a comparison. *)
            equal ~msg:"witness order agrees" string (fst b) (fst i);
            equal ~msg:(Printf.sprintf "witness %S" (fst b)) witness b i)
          instrumented baseline);
    test
      "the differential is not vacuous: every operator family has a live mutant"
      (fun () ->
        (* A build in which the rewriter emitted no guard at all would
           pass every comparison above. Arming each mutant of
           mutsem_order.ml in turn and replaying the battery is what
           separates "identical because instrumentation preserves
           meaning" from "identical because there is no instrumentation".
           The budget is a safety net: a mutated loop condition that stops
           terminating raises Runaway instead of hanging runtest. *)
        let baseline = Lazy.force baseline_battery in
        let mutants = mutants_of "mutsem_order.ml" in
        is_true ~msg:"the fixture has mutants" (mutants <> []);
        let changed =
          List.filter
            (fun (m : M.mutant) ->
              (match M.arm ~budget:1_000_000 m.M.id with
              | Ok _ -> ()
              | Error e -> failf "%a" M.pp_arm_error e);
              M.reset_reach ();
              let armed_out = instrumented_battery () in
              M.disarm ();
              armed_out <> baseline)
            mutants
        in
        is_true ~msg:"arming changes what the program computes" (changed <> []);
        (* Every operator family must have at least one mutant the
           battery can see, or that family's half of the comparison above
           proves nothing. *)
        let families = List.sort_uniq compare (List.map family changed) in
        equal ~msg:"every operator family has an observable mutant"
          (list string)
          [ "ari"; "cmp"; "con"; "neg" ]
          families;
        is_true ~msg:"nothing is left armed" (M.armed () = None);
        equal ~msg:"disarming restores the program exactly" (list witness)
          baseline (instrumented_battery ()));
    (* {1 Tail position, measured} *)
    test "tail calls survive instrumentation, measured on the call stack"
      (fun () ->
        (* The control first. If the measurement cannot tell a lost tail
           call from a kept one, none of the readings below mean
           anything. *)
        let shallow, deep = I.control_depths () in
        equal ~msg:"the instrumented control grows one frame per level" int
          99_000 (deep - shallow);
        let shallow, deep = B.control_depths () in
        equal ~msg:"the twin's control grows one frame per level" int 99_000
          (deep - shallow);
        (* The reading itself. Absolute depths are not compared across the
           twins - inlining decisions differ between a guarded body and a
           bare one, and a frame either way would be noise. Constancy in
           the recursion depth is the property, and it holds on both
           sides or neither. *)
        let constant which (name, at_shallow, at_deep) =
          equal
            ~msg:(Printf.sprintf "%s: still a tail call (%s)" name which)
            int at_shallow at_deep
        in
        List.iter (constant "instrumented") (I.tail_depths ());
        List.iter (constant "uninstrumented twin") (B.tail_depths ()));
    (* There is deliberately NO "recurse until it overflows" test here,
       and the shared fixture's deep shapes are exercised at a thousand
       levels in the outcomes test below rather than at twenty million.
       Recursing deep is not a decision procedure for tail position on
       OCaml 5: a fibre's stack grows on demand, and a non-tail recursion
       fifty million frames deep returns normally on this build (measured
       - a standalone probe of the same shape as [accumulate], run at
       1M, 3M, 20M and 50M, printed its answer every time). A suite that
       spent 0.7s on 130 million iterations to conclude nothing would be
       worse than no test, because its name would say otherwise. What
       decides tail position is [tail_depths] above, and the shapes the
       deep runs used to gesture at - a [||] arm that is a [let], a [&&]
       arm that is a [match] - are witnesses there.

       [tmc_map] is the one place a deep run still earns its keep, and
       even there the load-bearing half is the compilation: [tail_mod_cons]
       is an error, not a warning, when the compiler cannot apply it, so
       a rewriter that disturbed the attribute or the constructor
       argument would fail the build. The run is what pins the result. *)
    test "tail_mod_cons survives instrumentation" (fun () ->
        let n = 100_000 in
        let xs = List.init n (fun i -> i) in
        equal ~msg:"the TMC map is correct" (list int) (U.tmc_map succ xs)
          (F.tmc_map succ xs));
    (* {1 Outcomes: the shared fixture computes what its twin computes} *)
    test "the shared fixture computes the uninstrumented results" (fun () ->
        equal ~msg:"countdown" string (U.countdown 1_000) (F.countdown 1_000);
        equal ~msg:"even" bool (U.even 1_000) (F.even 1_000);
        equal ~msg:"odd" bool (U.odd 1_001) (F.odd 1_001);
        equal ~msg:"cps_count" int
          (U.cps_count 1_000 (fun x -> x))
          (F.cps_count 1_000 (fun x -> x));
        equal ~msg:"pipe_down" int (U.pipe_down 1_000) (F.pipe_down 1_000);
        equal ~msg:"any_odd odd" bool (U.any_odd 7) (F.any_odd 7);
        equal ~msg:"any_odd even" bool (U.any_odd 8) (F.any_odd 8);
        equal ~msg:"all_even even" bool (U.all_even 4) (F.all_even 4);
        equal ~msg:"all_even odd" bool (U.all_even 3) (F.all_even 3);
        equal ~msg:"or_let" bool (U.or_let 1_000) (F.or_let 1_000);
        equal ~msg:"or_match" bool (U.or_match 1_000) (F.or_match 1_000);
        equal ~msg:"or_if" bool (U.or_if 1_000) (F.or_if 1_000);
        equal ~msg:"or_try" bool (U.or_try 1_000) (F.or_try 1_000);
        equal ~msg:"tmc_map" (list int)
          (U.tmc_map succ (List.init 100 Fun.id))
          (F.tmc_map succ (List.init 100 Fun.id));
        let trace = pair int (list string) in
        equal ~msg:"order_witness true" trace (U.order_witness true)
          (F.order_witness true);
        equal ~msg:"order_witness false" trace (U.order_witness false)
          (F.order_witness false);
        let btrace = pair bool (list string) in
        List.iter
          (fun (x, y) ->
            equal
              ~msg:(Printf.sprintf "or_trace %b %b" x y)
              btrace (U.or_trace x y) (F.or_trace x y);
            equal
              ~msg:(Printf.sprintf "and_trace %b %b" x y)
              btrace (U.and_trace x y) (F.and_trace x y))
          [ (true, true); (true, false); (false, true); (false, false) ];
        equal ~msg:"arg_order"
          (pair (pair int int) (list string))
          (U.arg_order ()) (F.arg_order ());
        equal ~msg:"seq_order" (list string) (U.seq_order ()) (F.seq_order ());
        equal ~msg:"pipeline" int (U.pipeline 3) (F.pipeline 3);
        equal ~msg:"pipeline_bound" int (U.pipeline_bound 3)
          (F.pipeline_bound 3);
        equal ~msg:"sum_object" int
          (U.sum_object [ 1; 2; 3 ])
          (F.sum_object [ 1; 2; 3 ]);
        equal ~msg:"poke" int (U.poke (new U.adder)) (F.poke (new F.adder));
        equal ~msg:"sum_while" int (U.sum_while 10) (F.sum_while 10);
        equal ~msg:"sum_while, zero iterations" int (U.sum_while 0)
          (F.sum_while 0);
        equal ~msg:"sum_for" int (U.sum_for 10) (F.sum_for 10);
        equal ~msg:"letop_sum" int (U.letop_sum 40 2) (F.letop_sum 40 2);
        List.iter
          (fun n ->
            equal
              ~msg:(Printf.sprintf "bucket %d" n)
              string (U.bucket n) (F.bucket n))
          [ 5; 50; 500 ];
        equal ~msg:"safe_div by zero" int (U.safe_div 7 0) (F.safe_div 7 0);
        equal ~msg:"safe_div" int (U.safe_div 7 2) (F.safe_div 7 2);
        equal ~msg:"dispatch Add" int (U.dispatch `Add 2 3)
          (F.dispatch `Add 2 3);
        equal ~msg:"dispatch Sub" int (U.dispatch `Sub 7 3)
          (F.dispatch `Sub 7 3);
        equal ~msg:"a returning application returns" bool (U.tap_ok U.ret_unit)
          (F.tap_ok F.ret_unit);
        let raised g f =
          match g f with _ -> "returned" | exception Exit -> "Exit"
        in
        equal ~msg:"a raising application still raises" string
          (raised U.tap_raise U.raise_unit)
          (raised F.tap_raise F.raise_unit));
    (* {1 Laziness} *)
    test "lazy stays lazy; trivial lazy stays a value" (fun () ->
        (* The one-shot half of the differential: [lazy_witness] mutates
           state that cannot be reset, so it is not in the replayable
           battery and is called exactly once per copy. *)
        equal ~msg:"the instrumented copy is lazy exactly as its twin is" string
          (B.lazy_witness ()) (I.lazy_witness ());
        (* And from the registry's side, on the shared fixture: the two
           [ari] sites in the thunk's body are unreached until the force,
           reached exactly once by it, and never again. *)
        let thunk, forced, force_count = F.make_thunk () in
        is_true ~msg:"the lazy body has not run" (!forced = false);
        let v, first = reached (fun () -> Lazy.force thunk) in
        equal ~msg:"forcing the thunk" int 42 v;
        is_true ~msg:"the lazy body ran on force" (!forced = true);
        equal ~msg:"forcing reached exactly the lazy body's two sites" reach
          [ ("sub", 1); ("sub", 1) ]
          first;
        let v, again = reached (fun () -> Lazy.force thunk) in
        equal ~msg:"forcing again computes the same value" int 42 v;
        equal ~msg:"forcing twice runs the body once" int 1 !force_count;
        equal ~msg:"forcing again reaches nothing" reach [] again;
        is_true ~msg:"trivial lazy compiles as in an uninstrumented build"
          (Lazy.is_val (F.trivial ()) = Lazy.is_val (lazy 42));
        is_true ~msg:"and as in its twin"
          (Lazy.is_val (F.trivial ()) = Lazy.is_val (U.trivial ())));
    (* {1 Counts: the reach map measures evaluations, not calls} *)
    test "the reach map counts evaluations, and only what ran" (fun () ->
        (* [sum_while 10] evaluates its loop condition eleven times and
           its accumulator ten. Counts, not just presence: an
           instrumentation that evaluated a guarded operand twice would
           still compute the right answer and would still be caught
           here. *)
        let total, rs = reached (fun () -> F.sum_while 10) in
        equal ~msg:"the loop still sums" int 55 total;
        equal ~msg:"eleven condition evaluations, ten additions" reach
          [ ("lt", 11); ("sub", 10) ]
          rs;
        let total, rs = reached (fun () -> F.sum_while 0) in
        equal ~msg:"zero iterations" int 0 total;
        equal ~msg:"one condition evaluation, no addition" reach
          [ ("lt", 1) ]
          rs;
        (* Short-circuiting from the registry's side: the right arm of
           [&&] carries no site of its own here, but the connective does,
           and a skipped arm must not add hits. *)
        let r, rs = reached (fun () -> F.and_trace false true) in
        is_true ~msg:"&& short-circuits" (r = (false, [ "left" ]));
        equal ~msg:"the connective was evaluated once" reach [ ("or", 1) ] rs);
    (* {1 Inertness, restated at the end} *)
    test "nothing was armed from the first line to the last" (fun () ->
        is_true ~msg:"no mutant is armed" (M.armed () = None);
        (* [id_of_string] round-trips every catalogued identifier: the
           report the loop will print names sites that can be found
           again. *)
        List.iter
          (fun (m : M.mutant) ->
            match M.id_of_string (M.id_to_string m.id) with
            | Ok id ->
                is_true ~msg:"the identifier round-trips"
                  (M.compare_id id m.id = 0)
            | Error e -> failf "%a" M.pp_arm_error e)
          (M.catalogue ()));
  ]

let () = exit @@ run "mutate semantics" tests
