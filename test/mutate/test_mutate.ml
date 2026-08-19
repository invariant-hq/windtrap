(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Windtrap_mutate: identifiers and their one spelling (with
   every rejection class), the register/guard registry (reach counting,
   epoch and dirty-list bookkeeping across simulated tests, hit counts,
   reset, duplicate and conflicting registrations), arming (not-found
   versus ambiguous, the runaway budget), the verdict lattice (killed
   anywhere wins, and the algebraic laws that make merging any number of
   files in any order give one answer), the v2 verdict format (exact
   bytes, round trip, every corruption class), deterministic output
   filenames, and the atomic write - the last also end to end through a
   child executable.

   A windtrap suite ([run] executes tests sequentially in declaration
   order). The registry is a module global: every test registers under
   its own file name, and tests that arm disarm again before returning,
   so run the suite whole rather than filtered. *)

open Windtrap
module M = Windtrap_mutate

(* Printers and lookups the runtime does not export: they are for
   diagnostics and assertions, which is a test's business rather than a
   published surface. *)
let pp_id ppf (i : M.id) = Format.pp_print_string ppf (M.id_to_string i)

let pp_witness ppf w = Format.pp_print_string ppf (String.concat " > " w)

let pp_verdict ppf = function
  | M.Killed -> Format.pp_print_string ppf "killed"
  | M.Survived { witness; others } ->
      Format.fprintf ppf "survived by %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           pp_witness)
        (witness :: others)
  | M.Unreached -> Format.pp_print_string ppf "unreached"

let find t id =
  List.find_opt (fun (r : M.record) -> M.compare_id r.M.id id = 0) (M.records t)

let is_empty t = M.records t = []
let id_t = Testable.structural ~pp:pp_id
let verdict_t = Testable.structural ~pp:pp_verdict

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

(* A verdict-file record. The rendering defaults are the shortest that
   still round-trip; the tests that are about the rendering pass their
   own. *)
let record ?(before = "b") ?(after = "a") id verdict =
  { M.id; before; after; verdict }

let pp_record ppf (r : M.record) =
  Format.fprintf ppf "%a %s -> %s: %a" pp_id r.M.id r.M.before r.M.after
    pp_verdict r.M.verdict

let record_t = Testable.structural ~pp:pp_record

(* Most assertions below are about the verdict alone; [find] hands back
   the whole record. *)
let verdict_of t id =
  Option.map (fun (r : M.record) -> r.M.verdict) (find t id)

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

let ok_error name = function
  | Ok v -> v
  | Error e -> failf "%s: unexpected error: %a" name M.pp_error e

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
          (0 = M.compare_id
             (id ~file:"a" ~line:1 ~col:2 ~rewrite:"or")
             (id ~file:"a" ~line:1 ~col:2 ~rewrite:"or"));
        is_false ~msg:"a differing rewrite is a differing id"
          (0 = M.compare_id
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
                mutant ~file:"t/reach.ml" ~line:1 ~col:0 ~rewrite:"lt"
                  ();
              hits = 2;
            };
            {
              M.mutant =
                mutant ~file:"t/reach.ml" ~line:2 ~col:0 ~rewrite:"add"
                  ();
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
              site ~line:3 ~col:1 ~rewrite:"add" ~before:"a + b"
                ~after:"a - b" ~dismissed:"equivalent" ();
            |];
        equal ~msg:"the dismissed mutant" (list mutant_t)
          [
            mutant ~file:"t/dismiss.ml" ~line:3 ~col:1 ~rewrite:"add"
              ~before:"a + b" ~after:"a - b"
              ~dismissed:"equivalent" ();
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
          match
            M.arm (id ~file:"t/twice.ml" ~line:2 ~col:4 ~rewrite:"gt")
          with
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
        match
          M.arm
            (id ~file:"t/absent.ml" ~line:1 ~col:0 ~rewrite:"lt")
        with
        | Ok m -> failf "armed %a, expected a refusal" pp_mutant m
        | Error (M.Uncatalogued { id } as e) ->
            equal ~msg:"the identifier is returned whole" string
              "t/absent.ml:1:0:lt"
              (Format.asprintf "%a" pp_id id);
            let rendered = Format.asprintf "%a" M.pp_arm_error e in
            contains ~msg:"the message says whose mutant it is not"
              ~sub:"not this executable's mutant" rendered;
            contains ~msg:"and keeps the misconfiguration diagnosis"
              ~sub:"--instrument-with ppx_windtrap.mutate" rendered
        | Error e -> failf "expected Uncatalogued, got %a" M.pp_arm_error e);
    test "a catalogued file with no matching site is Unmatched, never declined"
      (fun () ->
        (* The other half of the distinction: this executable WAS built
           from the file, so the identifier is wrong or stale rather
           than someone else's, and a caller must refuse on it. *)
        register_only ~file:"t/stale.ml"
          ~sites:[| site ~line:5 ~col:3 ~rewrite:"lt" () |];
        match
          M.arm (id ~file:"t/stale.ml" ~line:9 ~col:0 ~rewrite:"lt")
        with
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
        match
          M.arm
            (id ~file:"t/near.ml" ~line:5 ~col:4 ~rewrite:"lt")
        with
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
        (match
           M.arm
             (id ~file:"t/refuse.ml" ~line:1 ~col:0 ~rewrite:"or")
         with
        | Ok _ -> ()
        | Error e -> failf "arm: %a" M.pp_arm_error e);
        fresh ();
        is_true ~msg:"armed" (g 0);
        is_some ~msg:"armed () reports it" (M.armed ());
        (match
           M.arm
             (id ~file:"t/nothing.ml" ~line:1 ~col:0 ~rewrite:"or")
         with
        | Ok m -> failf "armed %a" pp_mutant m
        | Error _ -> ());
        is_false ~msg:"the previous mutant is no longer armed" (g 0);
        is_none ~msg:"armed () is None" (M.armed ());
        ignore (drain ()));
    test "arming a second mutant disarms the first, budget included" (fun () ->
        (* At most one mutant is armed per process (Law 16b), and the two
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
          arm_ok "second"
            (id ~file:"t/rearm_b.ml" ~line:1 ~col:0 ~rewrite:"gt")
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
                M.arm ~budget:n
                  (id ~file:"t/x.ml" ~line:1 ~col:0 ~rewrite:"or")))
          [ 0; -1 ]);
    test "arm_from_env resolves WINDTRAP_MUTATE_ARM" (fun () ->
        let g =
          M.register ~file:"t/env.ml"
            ~sites:[| site ~line:6 ~col:2 ~rewrite:"fadd" () |]
        in
        equal ~msg:"the variable's name" string "WINDTRAP_MUTATE_ARM"
          M.arm_variable;
        Unix.putenv M.arm_variable "";
        (match M.arm_from_env () with
        | Ok None -> ()
        | Ok (Some m) -> failf "armed %a from an empty variable" pp_mutant m
        | Error e -> failf "empty variable: %a" M.pp_arm_error e);
        Unix.putenv M.arm_variable "t/env.ml:6:2:fadd";
        (match M.arm_from_env () with
        | Ok (Some m) ->
            equal ~msg:"armed" string "t/env.ml:6:2:fadd"
              (M.id_to_string m.M.id)
        | Ok None -> fail "nothing armed"
        | Error e -> failf "arm_from_env: %a" M.pp_arm_error e);
        fresh ();
        is_true ~msg:"the guard answers true" (g 0);
        Unix.putenv M.arm_variable "t/env.ml:6:2:sub";
        (match M.arm_from_env () with
        | Ok _ -> fail "an unmatched identifier must be refused"
        | Error (M.Unmatched _) -> ()
        | Error e -> failf "expected Unmatched, got %a" M.pp_arm_error e);
        Unix.putenv M.arm_variable "not an identifier";
        (match M.arm_from_env () with
        | Ok _ -> fail "a malformed identifier must be refused"
        | Error (M.Malformed _) -> ()
        | Error e -> failf "expected Malformed, got %a" M.pp_arm_error e);
        Unix.putenv M.arm_variable "";
        M.disarm ();
        ignore (drain ()));
  ]

(* The verdict lattice *)

let sample_verdicts =
  [
    M.Unreached;
    M.survived [ [ "a" ] ];
    M.survived [ [ "b"; "c" ] ];
    M.survived [ [ "a" ]; [ "b"; "c" ] ];
    M.Killed;
  ]

let verdict_tests =
  [
    test "killed anywhere wins" (fun () ->
        List.iter
          (fun killed ->
            List.iter
              (fun other ->
                equal
                  ~msg:
                    (Format.asprintf "%a merged with %a" pp_verdict killed
                       pp_verdict other)
                  verdict_t killed
                  (M.merge_verdict killed other);
                equal ~msg:"the other way round" verdict_t killed
                  (M.merge_verdict other killed))
              [
                M.Unreached;
                M.survived [ [ "a" ] ];
                M.survived [ [ "a" ]; [ "b" ] ];
              ])
          [ M.Killed ]);
    test "a survivor names at least one test" (fun () ->
        (* [Survived] with no witness would print as "no test ran this line
           and none failed when it changed", which is [Unreached]'s
           finding wearing the survivor's remedy. *)
        raises_match ~msg:"the constructor refuses it" Exn.invalid_arg
          (fun () -> M.survived []);
        (* The variant itself cannot hold one: [Survived] takes a witness
           and the rest, so the empty case has no spelling. A verdict file
           claiming otherwise is corrupt (see the parse group). *)
        equal ~msg:"one witness is one test" verdict_t (M.survived [ [ "a" ] ])
          (M.Survived { witness = [ "a" ]; others = [] }));
    test "survived only when every executable that reached it survived"
      (fun () ->
        equal ~msg:"survived and unreached" verdict_t (M.survived [ [ "a" ] ])
          (M.merge_verdict (M.survived [ [ "a" ] ]) M.Unreached);
        equal ~msg:"unreached and unreached" verdict_t M.Unreached
          (M.merge_verdict M.Unreached M.Unreached);
        equal ~msg:"witnesses union and deduplicate" verdict_t
          (M.survived [ [ "a" ]; [ "b" ]; [ "c" ] ])
          (M.merge_verdict
             (M.survived [ [ "b" ]; [ "a" ] ])
             (M.survived [ [ "c" ]; [ "b" ] ])));
    test "merge_verdict is commutative, associative and idempotent" (fun () ->
        List.iter
          (fun a ->
            equal
              ~msg:(Format.asprintf "idempotent on %a" pp_verdict a)
              verdict_t a (M.merge_verdict a a);
            equal
              ~msg:
                (Format.asprintf "unreached is the unit of %a" pp_verdict a)
              verdict_t a
              (M.merge_verdict a M.Unreached);
            List.iter
              (fun b ->
                equal
                  ~msg:
                    (Format.asprintf "commutative on %a, %a" pp_verdict a
                       pp_verdict b)
                  verdict_t (M.merge_verdict a b) (M.merge_verdict b a);
                List.iter
                  (fun c ->
                    equal
                      ~msg:
                        (Format.asprintf "associative on %a, %a, %a"
                           pp_verdict a pp_verdict b pp_verdict c)
                      verdict_t
                      (M.merge_verdict (M.merge_verdict a b) c)
                      (M.merge_verdict a (M.merge_verdict b c)))
                  sample_verdicts)
              sample_verdicts)
          sample_verdicts);
    test "a mutant killed by one executable and surviving another is killed"
      (fun () ->
        (* The whole reason the verdict file exists: reporting the
           surviving executable's view alone is a false survivor. *)
        let m = id ~file:"lib/core.ml" ~line:12 ~col:4 ~rewrite:"add" in
        let a = M.add M.empty (record m (M.Killed)) in
        let b = M.add M.empty (record m (M.survived [ [ "cli"; "runs" ] ])) in
        let c = M.add M.empty (record m M.Unreached) in
        List.iter
          (fun (name, t) ->
            equal ~msg:name (option verdict_t)
              (Some (M.Killed))
              (verdict_of t m))
          [
            ("a then b then c", M.merge (M.merge a b) c);
            ("c then b then a", M.merge (M.merge c b) a);
            ("b then c then a", M.merge (M.merge b c) a);
            ("b then a", M.merge b a);
          ];
        equal ~msg:"without the killer it is a survivor" (option verdict_t)
          (Some (M.survived [ [ "cli"; "runs" ] ]))
          (verdict_of (M.merge b c) m));
    test "add combines rather than replaces, and normalizes witnesses"
      (fun () ->
        let m = id ~file:"lib/core.ml" ~line:1 ~col:0 ~rewrite:"or" in
        let t =
          M.add M.empty (record m (M.survived [ [ "b" ]; [ "a" ]; [ "b" ] ]))
        in
        equal ~msg:"sorted and deduplicated" (option verdict_t)
          (Some (M.survived [ [ "a" ]; [ "b" ] ]))
          (verdict_of t m);
        let t = M.add t (record m (M.survived [ [ "c" ] ])) in
        equal ~msg:"a second add unions" (option verdict_t)
          (Some (M.survived [ [ "a" ]; [ "b" ]; [ "c" ] ]))
          (verdict_of t m);
        let t = M.add t (record m (M.Killed)) in
        equal ~msg:"a kill overrides" (option verdict_t)
          (Some (M.Killed)) (verdict_of t m));
    test "a record carries the rendering the report draws" (fun () ->
        (* The catalogue lives in the instrumented binary; [windtrap
           mutate] links none of them. A record that named only its
           mutant would leave the project-level report unable to draw
           [a - b  ->  a + b], which is the block's whole point. *)
        let m = id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add" in
        let r =
          record ~before:"a - b" ~after:"a + b" m
            (M.survived [ [ "calc"; "adds" ] ])
        in
        let t = M.add M.empty r in
        equal ~msg:"kept whole" (option record_t) (Some r) (find t m);
        let round_tripped, _ =
          ok_error "round trip" (M.of_string (M.to_string t))
        in
        equal ~msg:"and survives the file" (option record_t) (Some r)
          (find round_tripped m));
    test "records disagreeing on a rendering merge deterministically" (fun () ->
        (* Only two builds of one source can produce this, and the data
           says nothing about which one the reader has open. So the rule
           is a total order rather than a guess: it is what keeps [merge]
           commutative and associative. *)
        let m = id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add" in
        let older =
          record ~before:"a - b" ~after:"a + b" m M.Unreached
        and newer =
          record ~before:"a - b" ~after:"a + b" m
            (M.Killed)
        in
        let expected =
          record ~before:"a - b" ~after:"a + b" m
            (M.Killed)
        in
        equal ~msg:"older then newer" (option record_t) (Some expected)
          (find (M.add (M.add M.empty older) newer) m);
        equal ~msg:"newer then older" (option record_t) (Some expected)
          (find (M.add (M.add M.empty newer) older) m);
        equal ~msg:"through merge, either way" text
          (M.to_string (M.merge (M.add M.empty older) (M.add M.empty newer)))
          (M.to_string (M.merge (M.add M.empty newer) (M.add M.empty older))));
    test "merging collections is commutative, associative and idempotent"
      (fun () ->
        (* The per-verdict laws above are not enough: [merge] folds one
           collection into another, and a left- or right-biased union
           would satisfy them and still make two files' answer depend on
           the order the reporting command happened to read them in. The
           three collections overlap on every kind of pair. *)
        let of_list entries =
          List.fold_left
            (fun t (file, line, v) ->
              M.add t (record (id ~file ~line ~col:0 ~rewrite:"lt") v))
            M.empty entries
        in
        let a =
          of_list
            [
              ("lib/a.ml", 1, M.Killed);
              ("lib/a.ml", 2, M.survived [ [ "p" ] ]);
              ("lib/a.ml", 3, M.Unreached);
              ("lib/only_a.ml", 1, M.Killed);
            ]
        and b =
          of_list
            [
              ("lib/a.ml", 1, M.Killed);
              ("lib/a.ml", 2, M.Unreached);
              ("lib/a.ml", 3, M.survived [ [ "q" ] ]);
              ("lib/only_b.ml", 1, M.Unreached);
            ]
        and c =
          of_list
            [
              ("lib/a.ml", 1, M.Unreached);
              ("lib/a.ml", 2, M.survived [ [ "p" ]; [ "r" ] ]);
              ("lib/a.ml", 3, M.Killed);
            ]
        in
        let bytes = M.to_string in
        equal ~msg:"idempotent" text (bytes a) (bytes (M.merge a a));
        equal ~msg:"empty is the unit" text (bytes a)
          (bytes (M.merge a M.empty));
        equal ~msg:"empty is the unit on the left" text (bytes a)
          (bytes (M.merge M.empty a));
        equal ~msg:"commutative" text
          (bytes (M.merge a b))
          (bytes (M.merge b a));
        equal ~msg:"associative" text
          (bytes (M.merge (M.merge a b) c))
          (bytes (M.merge a (M.merge b c)));
        (* And the answer itself, not merely its stability. *)
        equal ~msg:"the merged verdicts" (list string)
          [
            "lib/a.ml:1:0:lt killed";
            "lib/a.ml:2:0:lt survived by p, r";
            "lib/a.ml:3:0:lt killed";
            "lib/only_a.ml:1:0:lt killed";
            "lib/only_b.ml:1:0:lt unreached";
          ]
          (List.map
             (fun (r : M.record) ->
               Format.asprintf "%a %a" pp_id r.M.id pp_verdict r.M.verdict)
             (M.records (M.merge (M.merge c b) a))));
    test "collections order their bindings by identifier" (fun () ->
        let t =
          List.fold_left
            (fun t (file, line, v) ->
              M.add t (record (id ~file ~line ~col:0 ~rewrite:"lt") v))
            M.empty
            [
              ("lib/z.ml", 1, M.Unreached);
              ("lib/a.ml", 9, M.survived [ [ "t" ] ]);
              ("lib/a.ml", 2, M.Killed);
            ]
        in
        is_false ~msg:"not empty" (is_empty t);
        is_true ~msg:"empty is empty" (is_empty M.empty);
        equal ~msg:"bindings" (list string)
          [ "lib/a.ml:2:0:lt"; "lib/a.ml:9:0:lt"; "lib/z.ml:1:0:lt" ]
          (List.map (fun (r : M.record) -> M.id_to_string r.M.id) (M.records t));
        is_none ~msg:"an absent identifier"
          (find t (id ~file:"lib/a.ml" ~line:3 ~col:0 ~rewrite:"lt")));
  ]

(* The verdict file format *)

let sample_collection () =
  M.add
    (M.add
       (M.add M.empty
          (record ~before:"a - b" ~after:"a + b"
             (id ~file:"lib/a.ml" ~line:1 ~col:2 ~rewrite:"add")
             M.Unreached))
       (record ~before:"p && q" ~after:"not (p && q)"
          (id ~file:"lib/b.ml" ~line:3 ~col:4 ~rewrite:"not")
          M.Killed))
    (record ~before:"a || b" ~after:"a && b"
       (id ~file:"lib/b.ml" ~line:5 ~col:0 ~rewrite:"or")
       (M.survived [ [ "x" ]; [ "y"; "z" ] ]))

let sample_bytes =
  "windtrap-mutants-v3\n\
   3\n\
   8 lib/a.ml 1 2 3 add 5 a - b 5 a + b unreached\n\
   8 lib/b.ml 3 4 3 not 6 p && q 12 not (p && q) killed\n\
   8 lib/b.ml 5 0 2 or 6 a || b 6 a && b survived 2 1 1 x 2 1 y 1 z\n"

let digest = String.make 32 'a'

let format_tests =
  [
    test "to_string is the documented v3 encoding" (fun () ->
        equal ~msg:"exact bytes" text sample_bytes
          (M.to_string (sample_collection ())));
    test "an identity is recorded after the magic line" (fun () ->
        equal ~msg:"exact bytes" text
          ("windtrap-mutants-v3\nexe " ^ digest ^ " 10 test/a.exe\n0\n")
          (M.to_string ~identity:{ M.exe = "test/a.exe"; digest } M.empty);
        raises_match ~msg:"an empty exe is refused" Exn.invalid_arg (fun () ->
            M.to_string ~identity:{ M.exe = ""; digest } M.empty);
        raises_match ~msg:"a short digest is refused" Exn.invalid_arg (fun () ->
            M.to_string ~identity:{ M.exe = "a"; digest = "abc" } M.empty);
        raises_match ~msg:"a non-hex digest is refused" Exn.invalid_arg
          (fun () ->
            M.to_string
              ~identity:{ M.exe = "a"; digest = String.make 32 'X' }
              M.empty));
    test "of_string inverts to_string, identity included" (fun () ->
        let t = sample_collection () in
        let identity = { M.exe = "_build/test/a.exe"; digest } in
        let parsed, recorded =
          ok_error "round trip" (M.of_string (M.to_string ~identity t))
        in
        equal ~msg:"the collection" text (M.to_string t) (M.to_string parsed);
        equal ~msg:"the identity"
          (option (pair string string))
          (Some ("_build/test/a.exe", digest))
          (Option.map (fun (i : M.identity) -> (i.M.exe, i.M.digest)) recorded);
        let parsed, recorded =
          ok_error "no identity" (M.of_string (M.to_string t))
        in
        is_none ~msg:"none recorded" recorded;
        equal ~msg:"the collection" text (M.to_string t) (M.to_string parsed));
    test "witnesses holding spaces and newlines survive the round trip"
      (fun () ->
        let t =
          M.add M.empty
            (record ~before:"a\n= b" ~after:"a\n<> b"
               (id ~file:"lib/odd names.ml" ~line:1 ~col:0 ~rewrite:"eq")
               (M.survived [ [ "a group"; "a test\nwith a newline" ]; [ "" ] ]))
        in
        let parsed, _ = ok_error "round trip" (M.of_string (M.to_string t)) in
        equal ~msg:"identical" text (M.to_string t) (M.to_string parsed));
    test "serialization does not depend on construction order" (fun () ->
        let ids =
          [
            (id ~file:"lib/b.ml" ~line:2 ~col:0 ~rewrite:"lt", M.Unreached);
            ( id ~file:"lib/a.ml" ~line:1 ~col:0 ~rewrite:"or",
              M.survived [ [ "q" ]; [ "p" ] ] );
            ( id ~file:"lib/a.ml" ~line:9 ~col:0 ~rewrite:"sub",
              M.Killed );
          ]
        in
        let build order =
          M.to_string
            (List.fold_left
               (fun t (i, v) -> M.add t (record i v))
               M.empty order)
        in
        equal ~msg:"reversed insertion" text (build ids) (build (List.rev ids)));
    test "empty collections round-trip" (fun () ->
        equal ~msg:"bytes" text "windtrap-mutants-v3\n0\n" (M.to_string M.empty);
        let parsed, _ =
          ok_error "parse" (M.of_string "windtrap-mutants-v3\n0\n")
        in
        is_true ~msg:"still empty" (is_empty parsed));
  ]

(* Rejections *)

let check_corrupt name ~sub s =
  match M.of_string s with
  | Error (M.Corrupt { reason; _ }) -> contains ~msg:name ~sub reason
  | Error e -> failf "%s: expected Corrupt, got %a" name M.pp_error e
  | Ok _ -> failf "%s: parsed, expected a rejection mentioning %S" name sub

let rejection_tests =
  [
    cases "an unknown header is refused, never converted"
      ~name:(fun (name, _) -> name)
      [
        ("empty", "");
        ("coverage file", "windtrap-coverage-v3\n1\n");
        ("future version", "windtrap-mutants-v4\n0\n");
        ("uppercase", "WINDTRAP-MUTANTS-V1\n0\n");
        ("prefix without separator", "windtrap-mutants-v30\n0\n");
        ("plain text", "hello\nworld\n");
      ]
      (fun (name, s) ->
        match M.of_string ~path:"f.mutants" s with
        | Error (M.Unknown_format { path; header }) ->
            equal ~msg:"path" string "f.mutants" path;
            let rendered =
              Format.asprintf "%a" M.pp_error
                (M.Unknown_format { path; header })
            in
            contains ~msg:"the message names the expected magic"
              ~sub:"windtrap-mutants-v3" rendered
        | Error e ->
            failf "%s: expected Unknown_format, got %a" name M.pp_error e
        | Ok _ -> failf "%s: parsed, expected a rejection" name);
    cases "corrupt data is refused"
      ~name:(fun (name, _, _) -> name)
      [
        ( "truncated records",
          "windtrap-mutants-v3\n2\n8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n",
          "expected" );
        ( "record count exceeds data",
          "windtrap-mutants-v3\n99999999\n",
          "exceeds data" );
        ("negative record count", "windtrap-mutants-v3\n-1\n", "negative");
        (* The magic alone is not an empty collection: a truncated file
           must not read as "this executable killed nothing". *)
        ("magic only", "windtrap-mutants-v3", "expected record count");
        ( "negative witness count",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 1 -1\n",
          "negative test path length" );
        ( "line 0",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 0 2 3 add 1 b 1 a unreached\n",
          "1-based" );
        ( "negative line",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml -3 2 3 add 0 0 1 b 1 a unreached\n",
          "negative line" );
        ( "negative column",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 -2 3 add 0 0 1 b 1 a unreached\n",
          "negative column" );
        ( "empty file name",
          "windtrap-mutants-v3\n1\n0  1 2 3 add 0 0 1 b 1 a unreached\n",
          "empty file name" );
        ( "truncated file name",
          "windtrap-mutants-v3\n\
           1\n\
           80 lib/a.ml 1 2 3 add 1 b 1 a unreached\n",
          "truncated" );
        ( "unknown rewrite",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 4 plus 1 b 1 a unreached\n",
          "unknown rewrite" );
        ( "truncated rendering",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 80 b 1 a unreached\n",
          "truncated before" );
        ( "missing rendering",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add unreached\n",
          "before" );
        ( "unknown verdict",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a errored\n",
          "unknown verdict" );
        ( "missing verdict",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a\n",
          "expected verdict" );
        ( "truncated witness",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 2 1 1 g\n",
          "expected" );
        ( "witness count exceeds data",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 99999999\n",
          "exceeds data" );
        (* A survivor with no witness is not a survivor: it is what an
           unreached mutant looks like when a writer confuses the two, and
           it would render as "0 tests ran this line and none failed". *)
        ( "survivor with no witness",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 0\n",
          "names no test" );
        ( "duplicate record",
          "windtrap-mutants-v3\n\
           2\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a killed\n",
          "duplicate record" );
        ("trailing data", "windtrap-mutants-v3\n0\nextra\n", "trailing data");
        ( "short identity digest",
          "windtrap-mutants-v3\nexe abcd 1 a\n0\n",
          "digest" );
        ( "empty identity exe",
          "windtrap-mutants-v3\nexe " ^ String.make 32 'a' ^ " 0 \n0\n",
          "empty executable identity" );
      ]
      (fun (name, s, sub) -> check_corrupt name ~sub s);
    test "load reports an unreadable file" (fun () ->
        match M.load (scratch "does-not-exist.mutants") with
        | Error (M.Unreadable { path; _ }) ->
            contains ~msg:"path" ~sub:"does-not-exist.mutants" path
        | Error e -> failf "expected Unreadable, got %a" M.pp_error e
        | Ok _ -> fail "a missing file must not parse");
    test "pp_error names the fix for a foreign file" (fun () ->
        contains ~msg:"suggests deleting the stale files" ~sub:"_build/_mutants"
          (Format.asprintf "%a" M.pp_error
             (M.Unknown_format { path = "f"; header = "junk" })));
  ]

(* Output paths *)

let filename_tests =
  [
    test "build_root is the parent of the topmost _build" (fun () ->
        equal ~msg:"nested _build" (option string) (Some "/home/p")
          (M.build_root ~path:"/home/p/_build/default/test/t.exe");
        equal ~msg:"the topmost one wins" (option string) (Some "/home/p")
          (M.build_root ~path:"/home/p/_build/default/_build/t.exe");
        equal ~msg:"no _build component" (option string) None
          (M.build_root ~path:"/usr/local/bin/t"));
    test "exe_identity is sandbox-invariant" (fun () ->
        equal ~msg:"under _build" string "default/test/t.exe"
          (M.exe_identity ~exe:"/home/p/_build/default/test/t.exe");
        equal ~msg:"under a sandbox" string "default/test/t.exe"
          (M.exe_identity
             ~exe:"/home/p/_build/.sandbox/deadbeef/default/test/t.exe");
        equal ~msg:"outside _build" string "/usr/local/bin/t"
          (M.exe_identity ~exe:"/usr/local/bin/t"));
    test "output_file is deterministic and sandbox-invariant" (fun () ->
        let direct = M.output_file ~exe:"/home/p/_build/default/test/t.exe" in
        let sandboxed =
          M.output_file
            ~exe:"/home/p/_build/.sandbox/deadbeef/default/test/t.exe"
        in
        equal ~msg:"the same file either way" string direct sandboxed;
        is_true ~msg:"under the project's _build/_mutants"
          (String.starts_with ~prefix:"/home/p/_build/_mutants/windtrap-" direct);
        is_true ~msg:"named .mutants" (Filename.check_suffix direct ".mutants");
        not_equal ~msg:"a different executable gets a different file" string
          direct
          (M.output_file ~exe:"/home/p/_build/default/test/other.exe"));
    test "writer_identity digests the executable" (fun () ->
        let exe = Filename.concat exe_dir "arm_child.exe" in
        match M.writer_identity ~exe with
        | None -> fail "the child executable must be readable"
        | Some i ->
            equal ~msg:"exe" string (M.exe_identity ~exe) i.M.exe;
            equal ~msg:"digest" string
              (Digest.to_hex (Digest.file exe))
              i.M.digest);
    test "writer_identity is None for an unreadable executable" (fun () ->
        is_none ~msg:"absent" (M.writer_identity ~exe:(scratch "no-such-exe")));
  ]

(* The atomic write, in this process and in another *)

let file_tests =
  [
    test "save writes atomically and load reads it back" (fun () ->
        let path = scratch "sub/dir/verdicts.mutants" in
        let t = sample_collection () in
        let identity = { M.exe = "test/a.exe"; digest } in
        M.save ~identity path t;
        let parsed, recorded = ok_error "load" (M.load path) in
        equal ~msg:"round trip" text (M.to_string t) (M.to_string parsed);
        equal ~msg:"identity" (option string) (Some "test/a.exe")
          (Option.map (fun (i : M.identity) -> i.M.exe) recorded);
        equal ~msg:"no temporary files are left behind" (list string) []
          (Sys.readdir (Filename.dirname path)
          |> Array.to_list
          |> List.filter (fun n -> Filename.check_suffix n ".tmp"));
        (* Re-saving replaces; verdicts never accumulate on disk. *)
        M.save path M.empty;
        let parsed, recorded = ok_error "reload" (M.load path) in
        is_true ~msg:"replaced" (is_empty parsed);
        is_none ~msg:"the identity is gone too" recorded);
    test "save refuses a malformed identity before touching the disk" (fun () ->
        let path = scratch "unwritten.mutants" in
        raises_match ~msg:"refused" Exn.invalid_arg (fun () ->
            M.save ~identity:{ M.exe = "a"; digest = "short" } path M.empty);
        is_false ~msg:"nothing was written" (Sys.file_exists path));
  ]

(* The child executable *)

let child_exe = Filename.concat exe_dir "arm_child.exe"

let run_child ?arm args =
  let out = scratch "child-out.txt" and err = scratch "child-err.txt" in
  Unix.putenv M.arm_variable (match arm with None -> "" | Some spec -> spec);
  let status =
    Sys.command (Filename.quote_command child_exe ~stdout:out ~stderr:err args)
  in
  Unix.putenv M.arm_variable "";
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
        contains ~msg:"hint" ~sub:"--instrument-with ppx_windtrap.mutate" err);
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
    test "a child writes a verdict file the parent can load" (fun () ->
        let path = scratch "child.mutants" in
        let status, _, err = run_child [ "save"; path ] in
        equal ~msg:"exit code" int 0 status;
        equal ~msg:"stderr" text "" err;
        let t, recorded = ok_error "load" (M.load path) in
        equal ~msg:"the record" (option record_t)
          (Some
             (record ~before:"l < r" ~after:"not (r < l)"
                (id ~file:"lib/child.ml" ~line:3 ~col:10 ~rewrite:"lt")
                (M.survived [ [ "child"; "less" ] ])))
          (find t (id ~file:"lib/child.ml" ~line:3 ~col:10 ~rewrite:"lt"));
        equal ~msg:"the writer identity"
          (option (pair string string))
          (Some
             ( M.exe_identity ~exe:child_exe,
               Digest.to_hex (Digest.file child_exe) ))
          (Option.map (fun (i : M.identity) -> (i.M.exe, i.M.digest)) recorded);
        equal ~msg:"no temporary files are left behind" (list string) []
          (Sys.readdir scratch_dir |> Array.to_list
          |> List.filter (fun n -> Filename.check_suffix n ".tmp")));
  ]

(* The suite *)

let () =
  run "mutate"
    [
      group "identity" identity_tests;
      group "registry" registry_tests;
      group "arming" arming_tests;
      group "verdicts" verdict_tests;
      group "format" format_tests;
      group "parse" rejection_tests;
      group "filenames" filename_tests;
      group "files" file_tests;
      group "child" child_tests;
    ]
