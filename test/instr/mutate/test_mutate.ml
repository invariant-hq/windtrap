(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Windtrap_runtime.Mutate: identifiers and their one spelling (with
   every rejection class), the register/guard registry (reach counting,
   epoch and dirty-list bookkeeping across simulated tests, hit counts,
   reset, duplicate and conflicting registrations), and arming
   (not-found versus ambiguous, the runaway budget) - the last also end
   to end through a child executable. The verdict lattice and the
   verdict file are Windtrap_runtime.Verdicts'; their suite is
   test/instr/verdicts.

   A windtrap suite ([run] executes tests sequentially in declaration
   order). The registry is a module global: every test registers under
   its own file name and opens its own window, and every arming is undone
   on the way out, failure included ([disarming]), so a test passes alone
   as in the whole suite. *)

open Windtrap
module M = Windtrap_runtime.Mutate
module Child = Windtrap_test_support.Child

(* Printers and lookups the runtime does not export: they are for
   diagnostics and assertions, which is a test's business rather than a
   published surface. *)
let pp_id ppf (i : M.id) = Format.pp_print_string ppf (M.id_to_string i)
let id_t = Testable.structural ~pp:pp_id

(* This suite's own registrations, told apart from the process's

   The registry is global and this executable links windtrap, which under
   --instrument-with is itself mutation-instrumented: about a thousand
   sites in lib/ register at library load, before a line of this file
   runs. Every assertion below is about the synthetic files these tests
   register, and the synthetic names deliberately look like real ones
   ("lib/calc.ml"), so they cannot be told apart by shape.

   They can be told apart by TIME. Whatever is in the catalogue at this
   module's load (after the library's, before any test's) is not this
   suite's. Capturing it costs one list and needs no maintenance when a
   test adds a name. *)
let foreign_files =
  List.map (fun (m : M.mutant) -> m.M.id.M.file) (M.catalogue ())

let mine file = not (List.mem file foreign_files)

(* [M.drain ()] and [M.catalogue ()], restricted to this suite's files.
   The raw drain must still happen (draining is what closes a window),
   so these filter the result rather than skipping the call. *)
let drain () =
  List.filter (fun (r : M.reached) -> mine r.M.mutant.M.id.M.file) (M.drain ())

let catalogue () =
  List.filter (fun (m : M.mutant) -> mine m.M.id.M.file) (M.catalogue ())

let pp_mutant ppf (m : M.mutant) =
  Format.fprintf ppf "%a%s" pp_id m.M.id
    (match m.M.dismissed with None -> "" | Some r -> " off:" ^ r)

let mutant_t = Testable.structural ~pp:pp_mutant

let pp_reached ppf (r : M.reached) =
  Format.fprintf ppf "%a x%d" pp_mutant r.M.mutant r.M.hits

let reached_t = Testable.structural ~pp:pp_reached

let site ?dismissed ~line ~col ~rewrite ?(before = "b") ?(after = "a") () =
  { M.line; col; rewrite; before; after; dismissed }

let mutant ~file ~line ~col ~rewrite ?(before = "b") ?(after = "a") ?dismissed
    () =
  { M.id = { M.file; line; col; rewrite }; before; after; dismissed }

let id ~file ~line ~col ~rewrite = { M.file; line; col; rewrite }

(* [register] returns the file's guard closure; a test that only needs the
   registration binds it away rather than [ignore]ing a function. *)
let register_only ~file ~sites =
  let _guard : int -> bool = M.register ~file ~sites in
  ()

(* A fresh observation window: the previous test's residue is dropped and
   a new epoch opened, so a drain here reports only what this test did. *)
let fresh () =
  ignore (drain ());
  M.next_epoch ()

(* Every test that arms runs inside [disarming]: a failure after [arm]
   would otherwise leave the mutant armed for every test after it. [arm]
   disarms whatever was armed before it resolves, so arming an identifier
   of no catalogued file leaves nothing armed (pinned below). *)
let disarming f =
  Fun.protect
    ~finally:(fun () ->
      ignore
        (M.arm { M.file = "t/nowhere.ml"; line = 1; col = 0; rewrite = "not" }))
    f

let arm_ok ?budget sel =
  match M.arm ?budget sel with
  | Ok m -> m
  | Error e -> failf "arm %a: %a" pp_id sel M.pp_arm_error e

(* Hermeticity: all paths are absolute, so the suite behaves identically
   under dune's sandbox and when run by hand from anywhere. The child
   executable sits next to this one. Nothing is ever written under
   _build/_mutants. *)
let exe_dir = Filename.dirname Sys.executable_name

(* Identifiers *)

let identity_tests =
  [
    test "id_to_string is <file>:<line>:<col>:<rewrite>" (fun () ->
        equal ~msg:"canonical spelling" string "lib/calc.ml:9:12:add"
          (M.id_to_string
             (id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add")));
    test "compare_id orders by file, then line, then column, then rewrite"
      (fun () ->
        let ids =
          [
            id ~file:"lib/b.ml" ~line:1 ~col:0 ~rewrite:"add";
            id ~file:"lib/a.ml" ~line:9 ~col:1 ~rewrite:"add";
            id ~file:"lib/a.ml" ~line:2 ~col:7 ~rewrite:"or";
            id ~file:"lib/a.ml" ~line:9 ~col:1 ~rewrite:"and";
            id ~file:"lib/a.ml" ~line:9 ~col:0 ~rewrite:"neq";
          ]
        in
        equal ~msg:"sorted" (list string)
          [
            "lib/a.ml:2:7:or";
            "lib/a.ml:9:0:neq";
            "lib/a.ml:9:1:add";
            "lib/a.ml:9:1:and";
            "lib/b.ml:1:0:add";
          ]
          (List.map M.id_to_string (List.sort M.compare_id ids));
        is_true ~msg:"compare_id is an equality on identifiers"
          (0
          = M.compare_id
              (id ~file:"a" ~line:1 ~col:2 ~rewrite:"or")
              (id ~file:"a" ~line:1 ~col:2 ~rewrite:"or"));
        is_false ~msg:"a differing rewrite is a differing id"
          (0
          = M.compare_id
              (id ~file:"a" ~line:1 ~col:2 ~rewrite:"or")
              (id ~file:"a" ~line:1 ~col:2 ~rewrite:"and")));
    test "id_of_string round-trips the canonical spelling" (fun () ->
        List.iter
          (fun i ->
            equal ~msg:(M.id_to_string i) (result id_t string) (Ok i)
              (Result.map_error
                 (fun e -> Format.asprintf "%a" M.pp_arm_error e)
                 (M.id_of_string (M.id_to_string i))))
          [
            id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add";
            id ~file:"a.ml" ~line:1 ~col:0 ~rewrite:"not";
            (* A file name holding colons still parses: the fields are
               taken from the right. *)
            id ~file:"a:b.ml" ~line:44 ~col:3 ~rewrite:"fsub";
          ]);
    cases "id_of_string rejects"
      ~name:(fun (spec, _) -> if spec = "" then "<empty>" else spec)
      [
        ("", "no ':' separator");
        ("lib/calc.ml", "no ':' separator");
        ("lib/calc.ml:9:12:frobnicate", "unknown rewrite");
        (* The vocabulary is closed: a plausible operator name that is not
           in it must not slip through as an identifier matching
           nothing. *)
        ("lib/calc.ml:9:12:plus", "unknown rewrite");
        ("lib/calc.ml:9:12:LT", "unknown rewrite");
        (* No instrumenter emits it, so no identifier names it. *)
        ("lib/calc.ml:9:12:drop", "unknown rewrite");
        ("add", "no ':' separator");
        (":add", "no position before the rewrite");
        ("lib/calc.ml:9:add", "no line number");
        ("lib/calc.ml:9:x:add", "invalid column");
        ("lib/calc.ml:9:-1:add", "invalid column");
        ("lib/calc.ml:x:12:add", "invalid line");
        ("lib/calc.ml:0:12:add", "1-based");
        (":9:12:add", "empty file name");
        (* The byte-span spelling is not a spelling: the column parse
           rejects it, and the message names the field it read. *)
        ("lib/calc.ml:312-317:add", "invalid column");
        ("lib/calc.ml:0x10:2:add", "invalid line");
        ("lib/calc.ml:1_0:2:add", "invalid line");
      ]
      (fun (spec, needle) ->
        match M.id_of_string spec with
        | Ok i ->
            failf "%S parsed as %a, expected a rejection mentioning %S" spec
              pp_id i needle
        | Error (M.Malformed { reason; _ }) ->
            contains ~msg:"reason" ~sub:needle reason
        | Error e ->
            failf "%S: expected Malformed, got %a" spec M.pp_arm_error e);
    (* The fields are read from the right, and the first one wrong is the
       one the reason names. *)
    cases "id_of_string names the first field it cannot read"
      ~name:(fun (spec, _) -> spec)
      [
        ("add", "no ':' separator");
        ( "a.ml:1:",
          "unknown rewrite \"\" (expected one of not, lt, le, gt, ge, eq, neq, \
           add, sub, fadd, fsub, and, or)" );
        ("x:add", "no position before the rewrite");
        ("a.ml:x:add", "invalid column \"x\"");
        ("a.ml:9:add", "no line number");
        (":x:1:add", "invalid line \"x\"");
        (":0:1:add", "empty file name");
        ("a.ml:0:1:add", "line numbers are 1-based");
        ( "a.ml:99999999999999999999:1:add",
          "invalid line \"99999999999999999999\"" );
      ]
      (fun (spec, reason) ->
        match M.id_of_string spec with
        | Error (M.Malformed m) ->
            equal ~msg:"spec" string spec m.spec;
            equal ~msg:"reason" string reason m.reason
        | Ok i -> failf "%S parsed as %a" spec pp_id i
        | Error e -> failf "expected Malformed, got %a" M.pp_arm_error e);
    test "a malformed identifier prints its spelling and the expected form"
      (fun () ->
        match M.id_of_string "lib/calc.ml:9" with
        | Error e ->
            equal ~msg:"message" string
              "\"lib/calc.ml:9\" is not a mutant identifier: unknown rewrite \
               \"9\" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, \
               fadd, fsub, and, or); expected <file>:<line>:<col>:<rewrite>"
              (Format.asprintf "%a" M.pp_arm_error e)
        | Ok i -> failf "parsed as %a" pp_id i);
  ]

(* [with_stderr f] is [f ()] and what it wrote on standard error. *)
let with_stderr f =
  let path = temp_file () in
  let saved = Unix.dup Unix.stderr in
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  let result =
    Fun.protect
      ~finally:(fun () ->
        flush stderr;
        Unix.dup2 saved Unix.stderr;
        Unix.close saved)
      f
  in
  (result, In_channel.with_open_bin path In_channel.input_all)

(* Registration and the guard *)

let registry_tests =
  [
    test "the guard counts reaches and answers false when nothing is armed"
      (fun () ->
        let g =
          M.register ~file:"t/reach.ml"
            ~sites:
              [|
                site ~line:1 ~col:0 ~rewrite:"lt" ();
                site ~line:2 ~col:0 ~rewrite:"add" ();
              |]
        in
        fresh ();
        is_false ~msg:"first evaluation" (g 0);
        is_false ~msg:"second evaluation" (g 0);
        is_false ~msg:"other site" (g 1);
        equal ~msg:"the window reports both sites, with hit counts"
          (list reached_t)
          [
            {
              M.mutant =
                mutant ~file:"t/reach.ml" ~line:1 ~col:0 ~rewrite:"lt" ();
              hits = 2;
            };
            {
              M.mutant =
                mutant ~file:"t/reach.ml" ~line:2 ~col:0 ~rewrite:"add" ();
              hits = 1;
            };
          ]
          (drain ());
        equal ~msg:"draining twice yields nothing" (list reached_t) []
          (drain ()));
    test "epochs partition evaluations into per-test windows" (fun () ->
        let g =
          M.register ~file:"t/epoch.ml"
            ~sites:
              [|
                site ~line:1 ~col:0 ~rewrite:"lt" ();
                site ~line:2 ~col:0 ~rewrite:"or" ();
              |]
        in
        let ids reached =
          List.map
            (fun (r : M.reached) -> M.id_to_string r.M.mutant.M.id)
            reached
        in
        fresh ();
        (* Window 1 stands in for one test: it touches both sites. *)
        ignore (g 0);
        ignore (g 1);
        equal ~msg:"window 1 sees both" (list string)
          [ "t/epoch.ml:1:0:lt"; "t/epoch.ml:2:0:or" ]
          (ids (drain ()));
        (* Window 2 touches only the second: the first must not reappear
           merely because it was evaluated earlier. *)
        M.next_epoch ();
        ignore (g 1);
        equal ~msg:"window 2 sees only what it touched" (list string)
          [ "t/epoch.ml:2:0:or" ]
          (ids (drain ()));
        (* Window 3 touches the first again: an old epoch stamp must not
           suppress it. *)
        M.next_epoch ();
        ignore (g 0);
        equal ~msg:"window 3 sees the site again" (list string)
          [ "t/epoch.ml:1:0:lt" ]
          (ids (drain ())));
    test "hits are counted per window, not cumulatively" (fun () ->
        let g =
          M.register ~file:"t/hits.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"eq" () |]
        in
        let hits () = List.map (fun (r : M.reached) -> r.M.hits) (drain ()) in
        fresh ();
        ignore (g 0);
        ignore (g 0);
        ignore (g 0);
        equal ~msg:"three hits in the first window" (list int) [ 3 ] (hits ());
        M.next_epoch ();
        ignore (g 0);
        equal ~msg:"one hit in the second window, not four" (list int) [ 1 ]
          (hits ()));
    test "an evaluation between windows is reported by the next drain"
      (fun () ->
        (* The loop drains at Test_started too: whatever accumulated since
           the previous drain ran outside any test (module init, fixture
           release), and the loop counts a site reached only there as
           unreached. *)
        let g =
          M.register ~file:"t/outside.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"not" () |]
        in
        fresh ();
        equal ~msg:"the test window is empty" (list reached_t) [] (drain ());
        ignore (g 0);
        (* Between tests. *)
        equal ~msg:"the teardown evaluation is still observed" (list int) [ 1 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ())));
    test "marks a window left undrained stay for the next drain" (fun () ->
        let g =
          M.register ~file:"t/undrained.ml"
            ~sites:
              [|
                site ~line:1 ~col:0 ~rewrite:"lt" ();
                site ~line:2 ~col:0 ~rewrite:"gt" ();
              |]
        in
        fresh ();
        ignore (g 0);
        M.next_epoch ();
        ignore (g 1);
        equal ~msg:"the earlier window's site, and the later one's"
          (list string)
          [ "t/undrained.ml:1:0:lt"; "t/undrained.ml:2:0:gt" ]
          (List.map
             (fun (r : M.reached) -> M.id_to_string r.M.mutant.M.id)
             (drain ())));
    test "a mark left undrained across epochs counts every evaluation"
      (fun () ->
        let g =
          M.register ~file:"t/across.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"eq" () |]
        in
        fresh ();
        ignore (g 0);
        ignore (g 0);
        ignore (g 0);
        M.next_epoch ();
        ignore (g 0);
        equal ~msg:"three evaluations, a new epoch, one more: four" (list int)
          [ 4 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ())));
    test "an evaluation again in the epoch after a drain is never drained"
      (fun () ->
        (* The lower bound: the site marked itself at its first evaluation
           of the epoch, that mark is drained, and a later evaluation in the
           same epoch marks nothing. *)
        let g =
          M.register ~file:"t/lower.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"eq" () |]
        in
        fresh ();
        ignore (g 0);
        equal ~msg:"the first evaluation is drained" (list int) [ 1 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ()));
        ignore (g 0);
        ignore (g 0);
        equal ~msg:"the next two are not" (list reached_t) [] (drain ());
        M.next_epoch ();
        ignore (g 0);
        equal ~msg:"until a new epoch opens" (list int) [ 1 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ())));
    test
      "the empty file name is registered, and its identifier does not \
       round-trip" (fun () ->
        register_only ~file:"" ~sites:[| site ~line:1 ~col:0 ~rewrite:"or" () |];
        match
          List.filter (fun (m : M.mutant) -> m.M.id.M.file = "") (catalogue ())
        with
        | [ m ] -> (
            match M.id_of_string (M.id_to_string m.M.id) with
            | Error (M.Malformed { reason; _ }) ->
                contains ~msg:"the spelling is refused" ~sub:"empty file name"
                  reason
            | Ok i -> failf "%a parsed" pp_id i
            | Error e -> failf "expected Malformed, got %a" M.pp_arm_error e)
        | mutants ->
            failf "one mutant of the empty file, got %d" (List.length mutants));
    test "reset_reach zeroes counts and opens a fresh window" (fun () ->
        let g =
          M.register ~file:"t/reset.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"ge" () |]
        in
        fresh ();
        ignore (g 0);
        ignore (g 0);
        ignore (g 0);
        M.reset_reach ();
        equal ~msg:"the dirty list is emptied" (list reached_t) [] (drain ());
        ignore (g 0);
        equal ~msg:"counting restarts from zero" (list int) [ 1 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ())));
    test "the catalogue is sorted and free of link-order dependence" (fun () ->
        register_only ~file:"t/cat_b.ml"
          ~sites:
            [|
              site ~line:9 ~col:2 ~rewrite:"or" ();
              site ~line:1 ~col:2 ~rewrite:"and" ();
            |];
        register_only ~file:"t/cat_a.ml"
          ~sites:[| site ~line:4 ~col:0 ~rewrite:"sub" () |];
        let mine =
          List.filter
            (fun (m : M.mutant) ->
              String.length m.M.id.M.file > 6
              && String.sub m.M.id.M.file 0 6 = "t/cat_")
            (catalogue ())
        in
        equal ~msg:"catalogue order" (list string)
          [ "t/cat_a.ml:4:0:sub"; "t/cat_b.ml:1:2:and"; "t/cat_b.ml:9:2:or" ]
          (List.map (fun (m : M.mutant) -> M.id_to_string m.M.id) mine));
    test "the catalogue carries the source texts and dismissals" (fun () ->
        register_only ~file:"t/dismiss.ml"
          ~sites:
            [|
              site ~line:3 ~col:1 ~rewrite:"add" ~before:"a + b" ~after:"a - b"
                ~dismissed:"equivalent" ();
            |];
        equal ~msg:"the dismissed mutant" (list mutant_t)
          [
            mutant ~file:"t/dismiss.ml" ~line:3 ~col:1 ~rewrite:"add"
              ~before:"a + b" ~after:"a - b" ~dismissed:"equivalent" ();
          ]
          (List.filter
             (fun (m : M.mutant) -> m.M.id.M.file = "t/dismiss.ml")
             (catalogue ())));
    test "an index outside the site table raises" (fun () ->
        let g =
          M.register ~file:"t/bounds.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"lt" () |]
        in
        raises_match ~msg:"past the end" Exn.invalid_arg (fun () -> g 1);
        raises_match ~msg:"negative" Exn.invalid_arg (fun () -> g (-1)));
    cases "a malformed site table raises"
      ~name:(fun (name, _) -> name)
      [
        ("line 0", site ~line:0 ~col:0 ~rewrite:"lt" ());
        ("negative column", site ~line:1 ~col:(-1) ~rewrite:"lt" ());
        ("unknown rewrite", site ~line:1 ~col:0 ~rewrite:"plus" ());
        ( "drop, which no instrumenter emits",
          site ~line:1 ~col:0 ~rewrite:"drop" () );
      ]
      (fun (name, s) ->
        raises_match ~msg:name Exn.invalid_arg (fun () ->
            register_only ~file:("t/bad_" ^ name ^ ".ml") ~sites:[| s |]));
    cases "a malformed site table's message names the file and the site"
      ~name:(fun (_, message) -> message)
      [
        ( site ~line:0 ~col:0 ~rewrite:"lt" (),
          "Windtrap_runtime.Mutate: t/bad.ml: site 1: line 0 is not 1-based" );
        ( site ~line:1 ~col:(-2) ~rewrite:"lt" (),
          "Windtrap_runtime.Mutate: t/bad.ml: site 1: negative column -2" );
        ( site ~line:1 ~col:0 ~rewrite:"plus" (),
          "Windtrap_runtime.Mutate: t/bad.ml: site 1: unknown rewrite \"plus\""
        );
      ]
      (fun (bad, message) ->
        raises ~msg:"the first bad site of the table" (Invalid_argument message)
          (fun () ->
            register_only ~file:"t/bad.ml"
              ~sites:[| site ~line:1 ~col:0 ~rewrite:"lt" (); bad; bad |]));
    test "duplicate sites of one table are catalogued and drained once"
      (fun () ->
        let g =
          M.register ~file:"t/dup_reach.ml"
            ~sites:
              [|
                site ~line:1 ~col:0 ~rewrite:"or" ~before:"x" ();
                site ~line:1 ~col:0 ~rewrite:"or" ~before:"y" ();
              |]
        in
        equal ~msg:"the catalogue keeps the first site" (list mutant_t)
          [
            mutant ~file:"t/dup_reach.ml" ~line:1 ~col:0 ~rewrite:"or"
              ~before:"x" ();
          ]
          (List.filter
             (fun (m : M.mutant) -> m.M.id.M.file = "t/dup_reach.ml")
             (catalogue ()));
        fresh ();
        ignore (g 1);
        ignore (g 0);
        ignore (g 1);
        equal ~msg:"the drain keeps the site marked first, with every hit"
          (list reached_t)
          [
            {
              M.mutant =
                mutant ~file:"t/dup_reach.ml" ~line:1 ~col:0 ~rewrite:"or"
                  ~before:"y" ();
              hits = 3;
            };
          ]
          (drain ()));
    test "one source file registered twice arms and drains as one mutant"
      (fun () ->
        let sites = [| site ~line:2 ~col:4 ~rewrite:"gt" () |] in
        let g1 = M.register ~file:"t/twice.ml" ~sites in
        let g2 = M.register ~file:"t/twice.ml" ~sites:(Array.copy sites) in
        equal ~msg:"the file's mutants are catalogued once" int 1
          (List.length
             (List.filter
                (fun (m : M.mutant) -> m.M.id.M.file = "t/twice.ml")
                (catalogue ())));
        fresh ();
        ignore (g1 0);
        ignore (g2 0);
        ignore (g2 0);
        equal ~msg:"drained once, with the hits added" (list int) [ 3 ]
          (List.map (fun (r : M.reached) -> r.M.hits) (drain ()));
        (* Both copies must arm: leaving one disarmed would report a false
           survivor for code reached through it. *)
        disarming (fun () ->
            let armed =
              arm_ok (id ~file:"t/twice.ml" ~line:2 ~col:4 ~rewrite:"gt")
            in
            equal ~msg:"armed mutant" string "t/twice.ml:2:4:gt"
              (M.id_to_string armed.M.id);
            is_true ~msg:"the first copy is armed" (g1 0);
            is_true ~msg:"the second copy is armed" (g2 0));
        is_false ~msg:"disarm clears the first copy" (g1 0);
        is_false ~msg:"disarm clears the second copy" (g2 0);
        ignore (drain ()));
    test "a conflicting registration warns and yields an inert guard" (fun () ->
        let sites = [| site ~line:1 ~col:0 ~rewrite:"lt" () |] in
        register_only ~file:"t/conflict.ml" ~sites;
        let g, err =
          with_stderr (fun () ->
              M.register ~file:"t/conflict.ml"
                ~sites:
                  [| site ~line:1 ~col:0 ~rewrite:"lt" ~before:"x < y" () |])
        in
        equal ~msg:"behind windtrap's one anchor, a warning, the file first"
          string
          "windtrap: warning: t/conflict.ml: conflicting instrumentation \
           tables in one executable (stale build artifacts? rebuild from \
           clean); ignoring one module's sites\n"
          err;
        fresh ();
        is_false ~msg:"the dropped guard is inert" (g 0);
        equal ~msg:"the dropped guard reports no reach" (list reached_t) []
          (drain ());
        equal ~msg:"the first table is the one catalogued" (list string)
          [ "t/conflict.ml:1:0:lt" ]
          (List.map
             (fun (m : M.mutant) -> M.id_to_string m.M.id)
             (List.filter
                (fun (m : M.mutant) -> m.M.id.M.file = "t/conflict.ml")
                (catalogue ()))));
  ]

(* Arming *)

let arming_tests =
  [
    test "arming a file the executable catalogues no site in is Uncatalogued"
      (fun () ->
        (* Not [Unmatched]: one identifier is armed across a whole
           project at once, and an executable built from other sources
           is not the one it is about. The caller reads this case as
           "not mine" and runs on, so it must be a case of its own and
           not an [Unmatched] whose candidate list happens to be
           empty. *)
        disarming @@ fun () ->
        match M.arm (id ~file:"t/absent.ml" ~line:1 ~col:0 ~rewrite:"lt") with
        | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
        | Error (M.Uncatalogued { id } as e) ->
            equal ~msg:"the identifier is returned whole" string
              "t/absent.ml:1:0:lt"
              (Format.asprintf "%a" pp_id id);
            let rendered = Format.asprintf "%a" M.pp_arm_error e in
            contains ~msg:"the message says whose mutant it is not"
              ~sub:"not this executable's mutant" rendered;
            contains ~msg:"and keeps the misconfiguration diagnosis"
              ~sub:"instrumented with ppx_windtrap.mutate" rendered
        | Error e -> failf "expected Uncatalogued, got %a" M.pp_arm_error e);
    test "a catalogued file with no matching site is Unmatched, never declined"
      (fun () ->
        (* The other half of the distinction: this executable WAS built
           from the file, so the identifier is wrong or stale rather
           than someone else's, and a caller must refuse on it. *)
        register_only ~file:"t/stale.ml"
          ~sites:[| site ~line:5 ~col:3 ~rewrite:"lt" () |];
        disarming @@ fun () ->
        match M.arm (id ~file:"t/stale.ml" ~line:9 ~col:0 ~rewrite:"lt") with
        | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
        | Error (M.Unmatched { candidates; _ }) ->
            equal ~msg:"the file's sites are named" (list string)
              [ "t/stale.ml:5:3:lt" ]
              (List.map
                 (fun (m : M.mutant) -> M.id_to_string m.M.id)
                 candidates)
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
    test "arming a wrong position in a known file lists the file's sites"
      (fun () ->
        register_only ~file:"t/near.ml"
          ~sites:
            [|
              site ~line:5 ~col:3 ~rewrite:"lt" ();
              site ~line:8 ~col:1 ~rewrite:"add" ();
            |];
        disarming @@ fun () ->
        match M.arm (id ~file:"t/near.ml" ~line:5 ~col:4 ~rewrite:"lt") with
        | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
        | Error (M.Unmatched { candidates; _ } as e) ->
            equal ~msg:"candidates" (list string)
              [ "t/near.ml:5:3:lt"; "t/near.ml:8:1:add" ]
              (List.map
                 (fun (m : M.mutant) -> M.id_to_string m.M.id)
                 candidates);
            let rendered = Format.asprintf "%a" M.pp_arm_error e in
            contains ~msg:"the message names a candidate"
              ~sub:"t/near.ml:5:3:lt" rendered
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
    test "two sites no identifier can tell apart are ambiguous, not arbitrary"
      (fun () ->
        (* Same line, column and rewrite: what a location-duplicating
           rewriter emits, and what [add_site] refuses to emit. No
           identifier separates them, and one file carries one armed
           index, so arming either would leave the other live - a false
           survivor for code reached through it. It must be refused. *)
        let g =
          M.register ~file:"t/dup.ml"
            ~sites:
              [|
                site ~line:4 ~col:2 ~rewrite:"eq" ();
                site ~line:4 ~col:2 ~rewrite:"eq" ();
              |]
        in
        ( disarming @@ fun () ->
          match M.arm (id ~file:"t/dup.ml" ~line:4 ~col:2 ~rewrite:"eq") with
          | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
          | Error (M.Ambiguous { candidates; _ } as e) ->
              equal ~msg:"both sites are named" int 2 (List.length candidates);
              let rendered = Format.asprintf "%a" M.pp_arm_error e in
              contains ~msg:"the message says they are alike"
                ~sub:"tell them apart" rendered;
              contains ~msg:"and names the remedies that do work"
                ~sub:"[@mutate off]" rendered
          | Error e -> failf "expected Ambiguous, got %a" M.pp_arm_error e );
        fresh ();
        is_false ~msg:"the first site stays disarmed" (g 0);
        is_false ~msg:"the second site stays disarmed" (g 1);
        ignore (drain ()));
    test "a refused arming disarms whatever was armed before" (fun () ->
        let g =
          M.register ~file:"t/refuse.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"or" () |]
        in
        disarming @@ fun () ->
        ignore (arm_ok (id ~file:"t/refuse.ml" ~line:1 ~col:0 ~rewrite:"or"));
        fresh ();
        is_true ~msg:"armed" (g 0);
        (match M.arm (id ~file:"t/nothing.ml" ~line:1 ~col:0 ~rewrite:"or") with
        | Ok m -> failf "armed %a" pp_mutant m
        | Error _ -> ());
        is_false ~msg:"the previous mutant is no longer armed" (g 0);
        ignore (drain ()));
    test "arming a second mutant disarms the first, budget included" (fun () ->
        (* At most one mutant is armed per process (guarantee 12), and the two
           live in different files - so this bites the disarm [arm] does
           before it resolves, not the overwrite of one file's slot. *)
        let ga =
          M.register ~file:"t/rearm_a.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"lt" () |]
        and gb =
          M.register ~file:"t/rearm_b.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"gt" () |]
        in
        disarming @@ fun () ->
        ignore
          (arm_ok ~budget:2
             (id ~file:"t/rearm_a.ml" ~line:1 ~col:0 ~rewrite:"lt"));
        fresh ();
        is_true ~msg:"the first is armed" (ga 0);
        let second =
          arm_ok (id ~file:"t/rearm_b.ml" ~line:1 ~col:0 ~rewrite:"gt")
        in
        equal ~msg:"arm's result names the second" string "t/rearm_b.ml:1:0:gt"
          (M.id_to_string second.M.id);
        is_false ~msg:"the first is no longer armed" (ga 0);
        (* The first arming's budget of 2 must not survive into the
           second, or the second mutant would run away at its third hit. *)
        for _ = 1 to 5 do
          is_true ~msg:"the second is armed, without an inherited budget" (gb 0)
        done;
        ignore (drain ()));
    test "the runaway budget fires on the evaluation that exceeds it" (fun () ->
        let g =
          M.register ~file:"t/runaway.ml"
            ~sites:[| site ~line:2 ~col:0 ~rewrite:"not" () |]
        in
        disarming (fun () ->
            ignore
              (arm_ok ~budget:3
                 (id ~file:"t/runaway.ml" ~line:2 ~col:0 ~rewrite:"not"));
            fresh ();
            is_true ~msg:"hit 1 is within budget" (g 0);
            is_true ~msg:"hit 2 is within budget" (g 0);
            is_true ~msg:"hit 3 is within budget" (g 0);
            raises_match ~msg:"hit 4 exceeds it"
              (function
                | M.Runaway { id; hits; budget } ->
                    M.id_to_string id = "t/runaway.ml:2:0:not"
                    && hits = 4 && budget = 3
                | _ -> false)
              (fun () -> g 0);
            contains ~msg:"the exception prints its mutant"
              ~sub:"t/runaway.ml:2:0:not"
              (Printexc.to_string
                 (M.Runaway
                    {
                      id = id ~file:"t/runaway.ml" ~line:2 ~col:0 ~rewrite:"not";
                      hits = 4;
                      budget = 3;
                    })));
        is_false ~msg:"a disarmed site has no budget to exceed" (g 0);
        ignore (drain ()));
    test "reset_reach restores the budget headroom a fork consumed" (fun () ->
        (* The child inherits the dry run's accumulated counts; without
           the reset its very first evaluation would look like a runaway. *)
        let g =
          M.register ~file:"t/forked.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"and" () |]
        in
        fresh ();
        for _ = 1 to 10 do
          ignore (g 0)
        done;
        disarming @@ fun () ->
        ignore
          (arm_ok ~budget:2
             (id ~file:"t/forked.ml" ~line:1 ~col:0 ~rewrite:"and"));
        M.reset_reach ();
        is_true ~msg:"the first armed hit is within budget" (g 0);
        is_true ~msg:"the second is too" (g 0);
        raises_match ~msg:"the third exceeds it"
          (function M.Runaway _ -> true | _ -> false)
          (fun () -> g 0);
        ignore (drain ()));
    test "the runaway is raised again at every later evaluation" (fun () ->
        let g =
          M.register ~file:"t/again.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"not" () |]
        in
        disarming (fun () ->
            ignore
              (arm_ok ~budget:1
                 (id ~file:"t/again.ml" ~line:1 ~col:0 ~rewrite:"not"));
            M.reset_reach ();
            is_true ~msg:"within the budget" (g 0);
            List.iter
              (fun expected ->
                raises_match
                  ~msg:(Printf.sprintf "evaluation %d, after a catch" expected)
                  (function
                    | M.Runaway { hits; _ } -> hits = expected | _ -> false)
                  (fun () -> g 0))
              [ 2; 3; 4 ]);
        ignore (drain ()));
    test "the budget bounds each registration, where armed_hits adds them up"
      (fun () ->
        let sites = [| site ~line:1 ~col:0 ~rewrite:"le" () |] in
        let g1 = M.register ~file:"t/per_copy.ml" ~sites in
        let g2 = M.register ~file:"t/per_copy.ml" ~sites:(Array.copy sites) in
        disarming (fun () ->
            ignore
              (arm_ok ~budget:2
                 (id ~file:"t/per_copy.ml" ~line:1 ~col:0 ~rewrite:"le"));
            M.reset_reach ();
            List.iter
              (fun g -> is_true ~msg:"two evaluations of each copy" (g 0))
              [ g1; g1; g2; g2 ];
            equal ~msg:"four in all, over a budget of two" int 4
              (M.armed_hits ());
            raises_match ~msg:"the third of one copy runs away"
              (function M.Runaway { hits; _ } -> hits = 3 | _ -> false)
              (fun () -> g1 0));
        equal ~msg:"nothing armed is none" int 0 (M.armed_hits ());
        ignore (drain ()));
    test "arm counts since reset_reach, not since itself" (fun () ->
        let g =
          M.register ~file:"t/kept.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"ge" () |]
        in
        let the_id = id ~file:"t/kept.ml" ~line:1 ~col:0 ~rewrite:"ge" in
        disarming (fun () ->
            ignore (arm_ok the_id);
            M.reset_reach ();
            ignore (g 0);
            ignore (g 0);
            ignore (arm_ok the_id);
            equal ~msg:"armed again, the two evaluations are still counted" int
              2 (M.armed_hits ());
            M.reset_reach ();
            equal ~msg:"until reset_reach" int 0 (M.armed_hits ()));
        ignore (drain ()));
    test "a refused budget disarms nothing" (fun () ->
        let g =
          M.register ~file:"t/kept_armed.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"sub" () |]
        in
        disarming (fun () ->
            ignore
              (arm_ok
                 (id ~file:"t/kept_armed.ml" ~line:1 ~col:0 ~rewrite:"sub"));
            raises_match ~msg:"a budget of 0" Exn.invalid_arg (fun () ->
                M.arm ~budget:0
                  (id ~file:"t/kept_armed.ml" ~line:1 ~col:0 ~rewrite:"sub"));
            fresh ();
            is_true ~msg:"the mutant armed before stays armed" (g 0));
        ignore (drain ()));
    test "a dismissed mutant arms" (fun () ->
        register_only ~file:"t/off.ml"
          ~sites:
            [| site ~line:2 ~col:1 ~rewrite:"add" ~dismissed:"equivalent" () |];
        disarming @@ fun () ->
        equal ~msg:"the mutant, dismissal included" mutant_t
          (mutant ~file:"t/off.ml" ~line:2 ~col:1 ~rewrite:"add"
             ~dismissed:"equivalent" ())
          (arm_ok (id ~file:"t/off.ml" ~line:2 ~col:1 ~rewrite:"add")));
    test "an unknown rewrite at a catalogued file is Unmatched" (fun () ->
        (* [arm] checks no rewrite, so the vocabulary refuses nothing here:
           the identifier simply matches no site. *)
        register_only ~file:"t/unknown_rewrite.ml"
          ~sites:[| site ~line:1 ~col:0 ~rewrite:"lt" () |];
        disarming @@ fun () ->
        match
          M.arm (id ~file:"t/unknown_rewrite.ml" ~line:1 ~col:0 ~rewrite:"plus")
        with
        | Error (M.Unmatched _) -> ()
        | Ok m -> failf "armed %a" pp_mutant m
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
    test "a refusal lists six candidates, then how many remain" (fun () ->
        register_only ~file:"t/many.ml"
          ~sites:
            (Array.init 9 (fun i -> site ~line:(i + 1) ~col:0 ~rewrite:"or" ()));
        disarming @@ fun () ->
        match M.arm (id ~file:"t/many.ml" ~line:99 ~col:0 ~rewrite:"or") with
        | Error (M.Unmatched { candidates; _ } as e) ->
            equal ~msg:"the error holds all nine" int 9 (List.length candidates);
            let rendered = Format.asprintf "%a" M.pp_arm_error e in
            List.iter
              (fun line ->
                contains ~msg:"one of the first six"
                  ~sub:(Printf.sprintf "t/many.ml:%d:0:or" line)
                  rendered)
              [ 1; 2; 3; 4; 5; 6 ];
            List.iter
              (fun line ->
                not_contains ~msg:"not one of the last three"
                  ~sub:(Printf.sprintf "t/many.ml:%d:0:or" line)
                  rendered)
              [ 7; 8; 9 ];
            contains ~msg:"the number left" ~sub:"3 more" rendered
        | Ok m -> failf "armed %a" pp_mutant m
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
    test "a dropped registration's guard never raises" (fun () ->
        let sites = [| site ~line:1 ~col:0 ~rewrite:"eq" () |] in
        register_only ~file:"t/inert.ml" ~sites;
        let inert, _warning =
          with_stderr (fun () ->
              M.register ~file:"t/inert.ml"
                ~sites:[| site ~line:1 ~col:0 ~rewrite:"eq" ~before:"x" () |])
        in
        disarming (fun () ->
            ignore
              (arm_ok ~budget:1
                 (id ~file:"t/inert.ml" ~line:1 ~col:0 ~rewrite:"eq"));
            List.iter
              (fun i ->
                is_false ~msg:"false past the budget and past the table"
                  (inert i))
              [ 0; 0; 0; 99; -1 ]));
    test "each refusal prints its message verbatim" (fun () ->
        register_only ~file:"t/seven.ml"
          ~sites:
            (Array.init 7 (fun i -> site ~line:(i + 1) ~col:0 ~rewrite:"or" ()));
        let refusal id =
          match M.arm id with
          | Ok m -> failf "armed %a" pp_mutant m
          | Error e -> Format.asprintf "%a" M.pp_arm_error e
        in
        disarming @@ fun () ->
        equal ~msg:"uncatalogued" string
          "t/absent.ml:1:0:lt: not this executable's mutant; it catalogues no \
           site in t/absent.ml (if you expected one, is the library under test \
           instrumented with ppx_windtrap.mutate?)"
          (refusal (id ~file:"t/absent.ml" ~line:1 ~col:0 ~rewrite:"lt"));
        equal ~msg:"unmatched" string
          "t/seven.ml:99:0:or: no such mutation site; t/seven.ml has these:\n\
          \    t/seven.ml:1:0:or\n\
          \    t/seven.ml:2:0:or\n\
          \    t/seven.ml:3:0:or\n\
          \    t/seven.ml:4:0:or\n\
          \    t/seven.ml:5:0:or\n\
          \    t/seven.ml:6:0:or\n\
          \    (and 1 more)"
          (refusal (id ~file:"t/seven.ml" ~line:99 ~col:0 ~rewrite:"or")));
    test "a table registered empty catalogues its file nowhere" (fun () ->
        register_only ~file:"t/empty.ml" ~sites:[||];
        disarming @@ fun () ->
        match M.arm (id ~file:"t/empty.ml" ~line:1 ~col:0 ~rewrite:"lt") with
        | Error (M.Uncatalogued _) -> ()
        | Ok m -> failf "armed %a" pp_mutant m
        | Error e -> failf "expected Uncatalogued, got %a" M.pp_arm_error e);
    test "duplicates registered twice are ambiguous once per site" (fun () ->
        let sites =
          [|
            site ~line:1 ~col:0 ~rewrite:"lt" ();
            site ~line:4 ~col:2 ~rewrite:"eq" ~before:"x" ();
            site ~line:4 ~col:2 ~rewrite:"eq" ~before:"y" ();
          |]
        in
        register_only ~file:"t/dup_twice.ml" ~sites;
        register_only ~file:"t/dup_twice.ml" ~sites:(Array.copy sites);
        disarming @@ fun () ->
        match
          M.arm (id ~file:"t/dup_twice.ml" ~line:4 ~col:2 ~rewrite:"eq")
        with
        | Error (M.Ambiguous { candidates; _ } as e) ->
            equal ~msg:"one candidate per site, in table order" (list mutant_t)
              [
                mutant ~file:"t/dup_twice.ml" ~line:4 ~col:2 ~rewrite:"eq"
                  ~before:"x" ();
                mutant ~file:"t/dup_twice.ml" ~line:4 ~col:2 ~rewrite:"eq"
                  ~before:"y" ();
              ]
              candidates;
            equal ~msg:"message" string
              "t/dup_twice.ml:4:2:eq: names 2 mutation sites, so no identifier \
               can tell them apart (a rewriter duplicating locations?); \
               dismiss the expression with [@mutate off] or exclude the file:\n\
              \    t/dup_twice.ml:4:2:eq\n\
              \    t/dup_twice.ml:4:2:eq"
              (Format.asprintf "%a" M.pp_arm_error e)
        | Ok m -> failf "armed %a" pp_mutant m
        | Error e -> failf "expected Ambiguous, got %a" M.pp_arm_error e);
    test "a runaway prints its mutant, its count and its budget" (fun () ->
        equal ~msg:"printer" string
          "Windtrap_runtime.Mutate.Runaway: t/print.ml:2:0:not evaluated 4 \
           times (budget 3)"
          (Printexc.to_string
             (M.Runaway
                {
                  id = id ~file:"t/print.ml" ~line:2 ~col:0 ~rewrite:"not";
                  hits = 4;
                  budget = 3;
                })));
    test "a refused budget names arm" (fun () ->
        raises ~msg:"message"
          (Invalid_argument
             "Windtrap_runtime.Mutate.arm: budget must be positive") (fun () ->
            M.arm ~budget:0 (id ~file:"t/x.ml" ~line:1 ~col:0 ~rewrite:"or")));
    test "a non-positive budget is a programmer error" (fun () ->
        disarming @@ fun () ->
        List.iter
          (fun n ->
            raises_match ~msg:(string_of_int n) Exn.invalid_arg (fun () ->
                M.arm ~budget:n (id ~file:"t/x.ml" ~line:1 ~col:0 ~rewrite:"or")))
          [ 0; -1 ]);
    test "the runtime reads no environment: only arm arms" (fun () ->
        let g =
          M.register ~file:"t/env.ml"
            ~sites:[| site ~line:6 ~col:2 ~rewrite:"fadd" () |]
        in
        (* The core's mirror set in the environment arms nothing here:
           the core parses the value and hands the identifier to [arm],
           and nothing in this library looks at the environment. *)
        setenv "WINDTRAP_MUTATE_ARM" (Some "t/env.ml:6:2:fadd");
        disarming @@ fun () ->
        fresh ();
        is_false ~msg:"the variable alone arms nothing" (g 0);
        (match Result.bind (M.id_of_string "t/env.ml:6:2:fadd") M.arm with
        | Ok m ->
            equal ~msg:"armed" string "t/env.ml:6:2:fadd"
              (M.id_to_string m.M.id)
        | Error e -> failf "arm: %a" M.pp_arm_error e);
        fresh ();
        is_true ~msg:"the guard answers true" (g 0);
        (match Result.bind (M.id_of_string "t/env.ml:6:2:sub") M.arm with
        | Ok _ -> fail "an unmatched identifier must be refused"
        | Error (M.Unmatched _) -> ()
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
        (match Result.bind (M.id_of_string "not an identifier") M.arm with
        | Ok _ -> fail "a malformed identifier must be refused"
        | Error (M.Malformed _) -> ()
        | Error e -> failf "expected Malformed, got %a" M.pp_arm_error e);
        ignore (drain ()));
  ]

(* The child executable *)

let child_exe = Filename.concat exe_dir "arm_child.exe"

(* The identifier travels on the child's command line: the runtime reads
   no environment, and the child arms what it is handed, as the core
   does. *)
let run_child ?arm args =
  let r = Child.run child_exe (args @ Option.to_list arm) in
  (Child.exit_code r, r.Child.out, r.Child.err)

let child_tests =
  [
    test "a run handed no identifier arms nothing" (fun () ->
        let status, out, err = run_child [ "run" ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stderr" text "" err;
        equal ~msg:"stdout" text
          "armed: none\nless 2 2 = false\nsum 3 4 = 7\npositives = 2\n" out);
    test "arming by position changes exactly one expression" (fun () ->
        let status, out, _ = run_child ~arm:"lib/child.ml:3:10:lt" [ "run" ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stdout" text
          "armed: lib/child.ml:3:10:lt a < b -> not (b < a)\n\
           less 2 2 = true\n\
           sum 3 4 = 7\n\
           positives = 2\n"
          out);
    test "a second site of the same file arms on its own position" (fun () ->
        let status, out, _ = run_child ~arm:"lib/child.ml:7:4:add" [ "run" ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stdout" text
          "armed: lib/child.ml:7:4:add a + b -> a - b\n\
           less 2 2 = false\n\
           sum 3 4 = -1\n\
           positives = 2\n"
          out);
    test "the negated condition is observable too" (fun () ->
        let status, out, _ = run_child ~arm:"lib/child.ml:11:6:not" [ "run" ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stdout" text
          "armed: lib/child.ml:11:6:not n > 0 -> not (n > 0)\n\
           less 2 2 = false\n\
           sum 3 4 = 7\n\
           positives = 1\n"
          out);
    (* The refusals below are arm_child's own policy (exit 1 on any arming
       error); the core runs on for an Uncatalogued identifier. What they
       pin is the runtime's error, as a real process prints it. *)
    test "an unmatched identifier refuses the run and names the candidates"
      (fun () ->
        let status, out, err =
          run_child ~arm:"lib/child.ml:99:0:lt" [ "run" ]
        in
        equal ~msg:"exit code" int 1 status;
        equal ~msg:"nothing ran" text "" out;
        contains ~msg:"refusal" ~sub:"no such mutation site" err;
        contains ~msg:"candidate" ~sub:"lib/child.ml:3:10:lt" err);
    test "a malformed identifier refuses the run" (fun () ->
        let status, _, err =
          run_child ~arm:"lib/child.ml:9:12:plus" [ "run" ]
        in
        equal ~msg:"exit code" int 1 status;
        contains ~msg:"refusal" ~sub:"unknown rewrite" err);
    test
      "an identifier of a file the child catalogues no site in is \
       Uncatalogued, blaming the instrumentation" (fun () ->
        let status, _, err = run_child ~arm:"lib/other.ml:1:0:lt" [ "run" ] in
        equal ~msg:"exit code" int 1 status;
        contains ~msg:"hint" ~sub:"instrumented with ppx_windtrap.mutate" err);
    test "the runaway budget raises in the program at the hit past it"
      (fun () ->
        let status, out, _ =
          run_child ~arm:"lib/child.ml:11:6:not" [ "budget"; "3"; "10" ]
        in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stdout" text
          "armed: lib/child.ml:11:6:not n > 0 -> not (n > 0)\n\
           runaway lib/child.ml:11:6:not after 4 hits (budget 3)\n"
          out);
    test "a budget nothing exceeds lets the run finish" (fun () ->
        let status, out, _ =
          run_child ~arm:"lib/child.ml:11:6:not" [ "budget"; "10"; "10" ]
        in
        equal ~msg:"exit code" int 0 status;
        contains ~msg:"no runaway" ~sub:"positives = " out;
        not_contains ~msg:"no runaway" ~sub:"runaway" out);
    test "module initialization lands in its own window, before the first"
      (fun () ->
        (* Only a fresh process can show this: in the parent's own image
           every earlier test has already bumped the epoch counter. A site
           the toplevel evaluated is drained before any window opens - so
           the loop bills it to no test, and counts it as unreached - and
           the window that follows counts its own hit, not the cumulative
           two. *)
        let status, out, err = run_child [ "reach" ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stderr" text "" err;
        equal ~msg:"stdout" text
          "module-init: lib/child.ml:3:10:lt x1\n\
           window: lib/child.ml:3:10:lt x1\n\
           drained:\n"
          out);
  ]

(* The suite *)

let () =
  exit
  @@ run "mutate"
       [
         group "identity" identity_tests;
         group "registry" registry_tests;
         group "arming" arming_tests;
         group "child" child_tests;
       ]
