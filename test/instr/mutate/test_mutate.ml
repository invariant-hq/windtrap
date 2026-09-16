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
   its own file name, and tests that arm disarm again before returning,
   so run the suite whole rather than filtered. *)

open Windtrap
module M = Windtrap_runtime.Mutate

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
   module's load — after the library's, before any test's — is not this
   suite's. Capturing it costs one list and needs no maintenance when a
   test adds a name. *)
let foreign_files =
  List.map (fun (m : M.mutant) -> m.M.id.M.file) (M.catalogue ())

let mine file = not (List.mem file foreign_files)

(* [M.drain ()] and [M.catalogue ()], restricted to this suite's files.
   The raw drain must still happen — draining is what closes a window —
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

(* Hermeticity: all paths are absolute, so the suite behaves identically
   under dune's sandbox and when run by hand from anywhere. The child
   executable sits next to this one; scratch files live in a private temp
   directory removed at exit. Nothing is ever written under
   _build/_mutants. *)
let exe_dir = Filename.dirname Sys.executable_name

let rec remove_tree path =
  match Sys.is_directory path with
  | true ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let scratch_dir =
  let dir = Filename.temp_file "windtrap_mut_scratch" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  at_exit (fun () -> remove_tree dir);
  dir

let scratch path = Filename.concat scratch_dir path

let read_file path =
  match open_in_bin path with
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> Some (really_input_string ic (in_channel_length ic)))
  | exception Sys_error _ -> None

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
  ]

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
    test "evaluations between drains are attributed to no window" (fun () ->
        (* The loop drains at Test_started too: whatever accumulated since
           the previous drain ran outside any test (module init, fixture
           release) and is reported as not armable, never as unreached. *)
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
      ]
      (fun (name, s) ->
        raises_match ~msg:name Exn.invalid_arg (fun () ->
            register_only ~file:("t/bad_" ^ name ^ ".ml") ~sites:[| s |]));
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
        let armed =
          match M.arm (id ~file:"t/twice.ml" ~line:2 ~col:4 ~rewrite:"gt") with
          | Ok m -> m
          | Error e -> failf "arm: %a" M.pp_arm_error e
        in
        equal ~msg:"armed mutant" string "t/twice.ml:2:4:gt"
          (M.id_to_string armed.M.id);
        is_true ~msg:"the first copy is armed" (g1 0);
        is_true ~msg:"the second copy is armed" (g2 0);
        M.disarm ();
        is_false ~msg:"disarm clears the first copy" (g1 0);
        is_false ~msg:"disarm clears the second copy" (g2 0);
        ignore (drain ()));
    test "a conflicting registration warns and yields an inert guard" (fun () ->
        let sites = [| site ~line:1 ~col:0 ~rewrite:"lt" () |] in
        register_only ~file:"t/conflict.ml" ~sites;
        let path = scratch "warn.txt" in
        let saved = Unix.dup Unix.stderr in
        let fd =
          Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
        in
        Unix.dup2 fd Unix.stderr;
        Unix.close fd;
        let g =
          Fun.protect
            ~finally:(fun () ->
              flush stderr;
              Unix.dup2 saved Unix.stderr;
              Unix.close saved)
            (fun () ->
              M.register ~file:"t/conflict.ml"
                ~sites:
                  [| site ~line:1 ~col:0 ~rewrite:"lt" ~before:"x < y" () |])
        in
        (match read_file path with
        | Some err ->
            contains ~msg:"warns" ~sub:"conflicting instrumentation tables" err
        | None -> fail "no stderr captured");
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
        (match M.arm (id ~file:"t/dup.ml" ~line:4 ~col:2 ~rewrite:"eq") with
        | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
        | Error (M.Ambiguous { candidates; _ } as e) ->
            equal ~msg:"both sites are named" int 2 (List.length candidates);
            let rendered = Format.asprintf "%a" M.pp_arm_error e in
            contains ~msg:"the message says they are alike"
              ~sub:"tell them apart" rendered;
            contains ~msg:"and names the remedies that do work"
              ~sub:"[@mutate off]" rendered
        | Error e -> failf "expected Ambiguous, got %a" M.pp_arm_error e);
        is_none ~msg:"nothing is armed" (M.armed ());
        fresh ();
        is_false ~msg:"the first site stays disarmed" (g 0);
        is_false ~msg:"the second site stays disarmed" (g 1);
        ignore (drain ()));
    test "a refused arming disarms whatever was armed before" (fun () ->
        let g =
          M.register ~file:"t/refuse.ml"
            ~sites:[| site ~line:1 ~col:0 ~rewrite:"or" () |]
        in
        (match M.arm (id ~file:"t/refuse.ml" ~line:1 ~col:0 ~rewrite:"or") with
        | Ok _ -> ()
        | Error e -> failf "arm: %a" M.pp_arm_error e);
        fresh ();
        is_true ~msg:"armed" (g 0);
        is_some ~msg:"armed () reports it" (M.armed ());
        (match M.arm (id ~file:"t/nothing.ml" ~line:1 ~col:0 ~rewrite:"or") with
        | Ok m -> failf "armed %a" pp_mutant m
        | Error _ -> ());
        is_false ~msg:"the previous mutant is no longer armed" (g 0);
        is_none ~msg:"armed () is None" (M.armed ());
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
        let arm_ok name ?budget sel =
          match M.arm ?budget sel with
          | Ok m -> m
          | Error e -> failf "%s: %a" name M.pp_arm_error e
        in
        let _ =
          arm_ok "first" ~budget:2
            (id ~file:"t/rearm_a.ml" ~line:1 ~col:0 ~rewrite:"lt")
        in
        fresh ();
        is_true ~msg:"the first is armed" (ga 0);
        let second =
          arm_ok "second" (id ~file:"t/rearm_b.ml" ~line:1 ~col:0 ~rewrite:"gt")
        in
        equal ~msg:"armed () names the second" (option string)
          (Some "t/rearm_b.ml:1:0:gt")
          (Option.map
             (fun (m : M.mutant) -> M.id_to_string m.M.id)
             (M.armed ()));
        equal ~msg:"and so does arm's result" string "t/rearm_b.ml:1:0:gt"
          (M.id_to_string second.M.id);
        is_false ~msg:"the first is no longer armed" (ga 0);
        (* The first arming's budget of 2 must not survive into the
           second, or the second mutant would run away at its third hit. *)
        for _ = 1 to 5 do
          is_true ~msg:"the second is armed, without an inherited budget" (gb 0)
        done;
        M.disarm ();
        ignore (drain ()));
    test "the runaway budget fires on the evaluation that exceeds it" (fun () ->
        let g =
          M.register ~file:"t/runaway.ml"
            ~sites:[| site ~line:2 ~col:0 ~rewrite:"not" () |]
        in
        (match
           M.arm ~budget:3
             (id ~file:"t/runaway.ml" ~line:2 ~col:0 ~rewrite:"not")
         with
        | Ok _ -> ()
        | Error e -> failf "arm: %a" M.pp_arm_error e);
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
                }));
        M.disarm ();
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
        (match
           M.arm ~budget:2
             (id ~file:"t/forked.ml" ~line:1 ~col:0 ~rewrite:"and")
         with
        | Ok _ -> ()
        | Error e -> failf "arm: %a" M.pp_arm_error e);
        M.reset_reach ();
        is_true ~msg:"the first armed hit is within budget" (g 0);
        is_true ~msg:"the second is too" (g 0);
        raises_match ~msg:"the third exceeds it"
          (function M.Runaway _ -> true | _ -> false)
          (fun () -> g 0);
        M.disarm ();
        ignore (drain ()));
    test "a non-positive budget is a programmer error" (fun () ->
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
        Unix.putenv "WINDTRAP_MUTATE_ARM" "t/env.ml:6:2:fadd";
        equal ~msg:"the variable alone arms nothing" (option mutant_t) None
          (M.armed ());
        fresh ();
        is_false ~msg:"and the guard answers false" (g 0);
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
        Unix.putenv "WINDTRAP_MUTATE_ARM" "";
        M.disarm ();
        ignore (drain ()));
  ]

(* The child executable *)

let child_exe = Filename.concat exe_dir "arm_child.exe"

(* The identifier travels on the child's command line: the runtime reads
   no environment, and the child arms what it is handed, as the core
   does. *)
let run_child ?arm args =
  let out = scratch "child-out.txt" and err = scratch "child-err.txt" in
  let args = args @ Option.to_list arm in
  let status =
    Sys.command (Filename.quote_command child_exe ~stdout:out ~stderr:err args)
  in
  ( status,
    Option.value ~default:"" (read_file out),
    Option.value ~default:"" (read_file err) )

let child_tests =
  [
    test "an uninstrumented-looking run arms nothing" (fun () ->
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
    test "an unknown file refuses the run and blames the instrumentation"
      (fun () ->
        let status, _, err = run_child ~arm:"lib/other.ml:1:0:lt" [ "run" ] in
        equal ~msg:"exit code" int 1 status;
        contains ~msg:"hint" ~sub:"instrumented with ppx_windtrap.mutate" err);
    test "the runaway budget kills a mutant the clock would not see" (fun () ->
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
           the toplevel evaluated is drained before any window opens - the
           loop reports it as not armable rather than unreached - and the
           window that follows counts its own hit, not the cumulative
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
