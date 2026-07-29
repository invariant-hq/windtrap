(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Snapshot: the CI-guarded mode resolution, name identity and
   collision rules, scope resolution and containment, canonicalization,
   read-only checking payloads, atomic acceptance, orphan reporting and
   prune refusals. Each test builds its own registry over a throwaway
   project root. The registry sits below the runner's snapshot wiring, so
   testing it under the runner is not circular: everything here drives
   [S.check] directly with explicit modes and scopes. *)

open Windtrap
open Windtrap.Private
module S = Snapshot

(* Each [let () = reg name @@ fun () -> ...] block below registers one
   windtrap test; [tests] collects them in declaration order. *)
let registered = ref []
let reg name body = registered := Windtrap.test name body :: !registered
let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_int name ~expected ~actual = equal ~msg:name int expected actual

(* ───── Filesystem scaffolding ───── *)

(* A fresh directory that stands in for the project root; reconstruction
   is lexical, so scoping source files need not exist under it. The
   runner removes it with the attempt. *)
let with_root f = f (temp_dir ())

let write_raw path contents =
  Path_ops.mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let read_raw path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let sorted_entries dir =
  if Sys.file_exists dir then
    Sys.readdir dir |> Array.to_list |> List.sort String.compare
  else []

(* ───── Failure-catching helpers ───── *)

(* Runs [f]; records whether it raised [Check_failure] and returns the
   failure for payload inspection. *)
let expect_check_failure label f =
  match f () with
  | () ->
      check (label ^ ": raises Check_failure") false;
      None
  | exception Failure.Check_failure fl ->
      check (label ^ ": raises Check_failure") true;
      Some fl

let expect_pass label f =
  match f () with
  | () -> check (label ^ ": passes") true
  | exception Failure.Check_failure _ ->
      check (label ^ ": passes (got Check_failure)") false

let expect_invalid_arg label f =
  match f () with
  | () -> check (label ^ ": raises Invalid_argument") false
  | exception Invalid_argument _ ->
      check (label ^ ": raises Invalid_argument") true

let snapshot_payload label fl =
  match fl with
  | None -> None
  | Some fl -> (
      match fl.Failure.kind with
      | Failure.Snapshot { name; path; state } -> Some (name, path, state)
      | _ ->
          check (label ^ ": kind is Snapshot") false;
          None)

let scope_a = "test/suite_a.ml"
let scope_b = "test/suite_b.ml"
let dir_a root = root ^ "/test/__snapshots__/suite_a"
let path_a root name = dir_a root ^ "/" ^ name ^ ".snap"
let loc_1 = Loc.of_pos (scope_a, 10, 2, 20)
let loc_2 = Loc.of_pos (scope_a, 42, 4, 30)

(* ───── resolve_mode: the CI guard ───── *)

let () =
  reg "resolve_mode: the CI guard" @@ fun () ->
  check "resolve_mode: No_update outside CI is Check"
    (S.resolve_mode ~ci:false Env.No_update = S.Mode S.Check);
  check "resolve_mode: No_update under CI is Check"
    (S.resolve_mode ~ci:true Env.No_update = S.Mode S.Check);
  check "resolve_mode: Update outside CI is Update"
    (S.resolve_mode ~ci:false Env.Update = S.Mode S.Update);
  check "resolve_mode: Update under CI is refused"
    (S.resolve_mode ~ci:true Env.Update = S.Refused_in_ci);
  check "resolve_mode: Force_update outside CI is Update"
    (S.resolve_mode ~ci:false Env.Force_update = S.Mode S.Update);
  check "resolve_mode: Force_update under CI is Update"
    (S.resolve_mode ~ci:true Env.Force_update = S.Mode S.Update)

(* ───── Registry basics ───── *)

let () =
  reg "registry basics" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Check () in
  check "mode: reports Check" (S.mode t = S.Check);
  check "mode: reports Update"
    (S.mode (S.create ~root ~mode:S.Update ()) = S.Update);
  check "writes: empty on a fresh registry" (S.writes t = []);
  check "orphans: empty on a fresh registry" (S.orphans t = [])

(* ───── Name validation ───── *)

let () =
  reg "name validation" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Check () in
  let attempt name () = S.check t ~test:"t" ~scope:(Some scope_a) ~name "x" in
  expect_invalid_arg "name: empty" (attempt "");
  expect_invalid_arg "name: space" (attempt "a b");
  expect_invalid_arg "name: slash" (attempt "a/b");
  expect_invalid_arg "name: backslash" (attempt "a\\b");
  expect_invalid_arg "name: non-ASCII" (attempt "caf\xc3\xa9");
  expect_invalid_arg "name: reserved temp prefix" (attempt ".tmp-x");
  (* Valid names get past validation: they fail Missing, not Invalid_argument. *)
  let valid name label =
    let fl = expect_check_failure label (attempt name) in
    match snapshot_payload label fl with
    | Some (_, _, Failure.Missing _) -> check (label ^ ": state Missing") true
    | _ -> check (label ^ ": state Missing") false
  in
  valid "help" "name: plain is valid";
  valid "A.b-c_9" "name: full alphabet is valid";
  valid "..." "name: dots only is valid"

(* ───── Scope resolution: unresolvable and containment ───── *)

let () =
  reg "scope resolution: unresolvable and containment" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Check () in
  let fl =
    expect_check_failure "scope: None" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:None ~name:"n" "x")
  in
  (match snapshot_payload "scope: None" fl with
  | Some (name, path, Failure.Unresolvable) ->
      check_string "scope: None keeps the name" ~expected:"n" ~actual:name;
      check_string "scope: None has empty path" ~expected:"" ~actual:path
  | _ -> check "scope: None is Unresolvable" false);
  (match fl with
  | Some fl -> check "scope: None carries loc" (fl.Failure.loc = Some loc_1)
  | None -> ());

  (* A scope escaping the root cannot be proven contained: both modes fail
     and neither writes anything. *)
  let escape = "../outside.ml" in
  let fl =
    expect_check_failure "scope: escapes root (check)" (fun () ->
        S.check t ~test:"t" ~scope:(Some escape) ~name:"n" "x")
  in
  (match snapshot_payload "scope: escapes root (check)" fl with
  | Some (_, path, Failure.Unresolvable) ->
      check "scope: escape reports the candidate path" (path <> "")
  | _ -> check "scope: escapes root is Unresolvable" false);
  let u = S.create ~root ~mode:S.Update () in
  let _ =
    expect_check_failure "scope: escapes root (update)" (fun () ->
        S.check u ~test:"t" ~scope:(Some escape) ~name:"n" "x")
  in
  check "scope: update mode wrote nothing for an unproven path" (S.writes u = []);
  check "scope: no directories created under root" (sorted_entries root = [])

(* ───── Check mode: missing baseline is read-only ───── *)

let () =
  reg "check mode: missing baseline is read-only" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Check () in
  let fl =
    expect_check_failure "missing: check mode" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
          "hello\r\nworld")
  in
  (match snapshot_payload "missing: check mode" fl with
  | Some (name, path, Failure.Missing { proposed }) ->
      check_string "missing: name" ~expected:"greeting" ~actual:name;
      check_string "missing: resolved path" ~expected:(path_a root "greeting")
        ~actual:path;
      check_string "missing: proposed content is canonicalized"
        ~expected:"hello\nworld\n" ~actual:proposed
  | _ -> check "missing: state is Missing" false);
  check "missing: nothing was created (Law 1)"
    (not (Sys.file_exists (root ^ "/test")));
  (* A recheck from the same site is Missing again, not Duplicate. *)
  let fl =
    expect_check_failure "missing: same-site recheck" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
          "hi")
  in
  match snapshot_payload "missing: same-site recheck" fl with
  | Some (_, _, Failure.Missing _) ->
      check "missing: recheck state is Missing" true
  | _ -> check "missing: recheck state is Missing" false

(* ───── Check mode: match and canonicalization round-trip ───── *)

let () =
  reg "check mode: match and canonicalization round-trip" @@ fun () ->
  with_root @@ fun root ->
  (* Baseline on disk with CRLF endings and no trailing newline: the read
     side canonicalizes exactly like the produced side. *)
  write_raw (path_a root "greeting") "hello\r\nworld";
  let t = S.create ~root ~mode:S.Check () in
  expect_pass "match: CRLF baseline vs LF actual" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
        "hello\nworld\n");
  expect_pass "match: actual without trailing newline" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
        "hello\nworld");
  check_string "match: baseline file untouched (Law 1)"
    ~expected:"hello\r\nworld"
    ~actual:(read_raw (path_a root "greeting"));
  check "match: no writes recorded" (S.writes t = [])

(* ───── Check mode: mismatch payload ───── *)

let () =
  reg "check mode: mismatch payload" @@ fun () ->
  with_root @@ fun root ->
  write_raw (path_a root "greeting") "hello\n";
  let t = S.create ~root ~mode:S.Check () in
  let fl =
    expect_check_failure "mismatch: check mode" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
          "bye")
  in
  (match snapshot_payload "mismatch: check mode" fl with
  | Some (_, path, Failure.Mismatch { expected; actual }) ->
      check_string "mismatch: path" ~expected:(path_a root "greeting")
        ~actual:path;
      check_string "mismatch: expected is the canonical baseline"
        ~expected:"hello\n" ~actual:expected;
      check_string "mismatch: actual is canonicalized" ~expected:"bye\n" ~actual
  | _ -> check "mismatch: state is Mismatch" false);
  check_string "mismatch: baseline file untouched (Law 1)" ~expected:"hello\n"
    ~actual:(read_raw (path_a root "greeting"))

(* ───── Same-baseline rule: rechecks use the run's cached baseline ───── *)

let () =
  reg "same-baseline rule: rechecks use the cached baseline" @@ fun () ->
  with_root @@ fun root ->
  write_raw (path_a root "cached") "one\n";
  let t = S.create ~root ~mode:S.Check () in
  expect_pass "cache: first check reads the file" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"cached" "one");
  (* The on-disk file changes mid-run; the recheck still compares against
     the baseline read at the first check. *)
  write_raw (path_a root "cached") "two\n";
  expect_pass "cache: recheck compares against the same baseline" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"cached" "one");
  let fl =
    expect_check_failure "cache: divergence is a mismatch" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"cached"
          "three")
  in
  match snapshot_payload "cache: divergence is a mismatch" fl with
  | Some (_, _, Failure.Mismatch { expected; _ }) ->
      check_string "cache: mismatch is against the first-read baseline"
        ~expected:"one\n" ~actual:expected
  | _ -> check "cache: divergence state is Mismatch" false

(* ───── Duplicate names ───── *)

let () =
  reg "duplicate names" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Check () in
  let check_name ?loc name () =
    S.check t ?loc ~test:"t" ~scope:(Some scope_a) ~name "x"
  in
  (* First site registers (via a Missing failure). *)
  let _ =
    expect_check_failure "duplicate: first site" (check_name ~loc:loc_1 "dup")
  in
  let fl =
    expect_check_failure "duplicate: second site" (check_name ~loc:loc_2 "dup")
  in
  (match snapshot_payload "duplicate: second site" fl with
  | Some (name, _, Failure.Duplicate { first; first_test }) ->
      check_string "duplicate: name" ~expected:"dup" ~actual:name;
      check "duplicate: first is the first site" (first = Some loc_1);
      check_string "duplicate: first_test is the first checker" ~expected:"t"
        ~actual:first_test
  | _ -> check "duplicate: state is Duplicate" false);
  (match fl with
  | Some fl ->
      check "duplicate: failure loc is the second site"
        (fl.Failure.loc = Some loc_2)
  | None -> ());
  (* Identity is case-insensitive. *)
  let fl =
    expect_check_failure "duplicate: case-insensitive"
      (check_name ~loc:loc_2 "DUP")
  in
  (match snapshot_payload "duplicate: case-insensitive" fl with
  | Some (_, _, Failure.Duplicate { first; _ }) ->
      check "duplicate: case collision reports first site" (first = Some loc_1)
  | _ -> check "duplicate: case collision state is Duplicate" false);
  (* An unknown site checked by the registered test: a recheck. *)
  let fl = expect_check_failure "duplicate: unknown site" (check_name "dup") in
  (match snapshot_payload "duplicate: unknown site" fl with
  | Some (_, _, Failure.Missing _) ->
      check "duplicate: unknown site by the same test is a recheck" true
  | _ -> check "duplicate: unknown site by the same test is a recheck" false);
  (* Same name under a different scoping file: an independent namespace. *)
  let fl =
    expect_check_failure "duplicate: other file" (fun () ->
        S.check t ~loc:loc_2 ~test:"t" ~scope:(Some scope_b) ~name:"dup" "x")
  in
  match snapshot_payload "duplicate: other file" fl with
  | Some (_, path, Failure.Missing _) ->
      check "duplicate: other file resolves its own dir"
        (path = root ^ "/test/__snapshots__/suite_b/dup.snap")
  | _ -> check "duplicate: other file is Missing, not Duplicate" false

let () =
  reg "duplicate names: cross-test with unknown sites" @@ fun () ->
  with_root @@ fun root ->
  (* The frame-free backstop (D4): both call frames gone, two different
     tests checking one name — [test] alone catches the duplicate. *)
  let t = S.create ~root ~mode:S.Check () in
  let _ =
    expect_check_failure "cross-test: first check" (fun () ->
        S.check t ~test:"a" ~scope:(Some scope_a) ~name:"dup" "x")
  in
  let fl =
    expect_check_failure "cross-test: second check" (fun () ->
        S.check t ~test:"b" ~scope:(Some scope_a) ~name:"dup" "x")
  in
  match snapshot_payload "cross-test: second check" fl with
  | Some (_, _, Failure.Duplicate { first = None; first_test }) ->
      check_string "cross-test: first_test names the first checker"
        ~expected:"a" ~actual:first_test
  | _ -> check "cross-test: unknown sites still yield Duplicate" false

let () =
  reg "duplicate names: distinct declaration-grade sites across tests"
  @@ fun () ->
  with_root @@ fun root ->
  (* The ppx/F-3 tail shape after the facade ladder: each test's fallback
     site is its own declaration, so the sites are provably distinct. *)
  let t = S.create ~root ~mode:S.Check () in
  let _ =
    expect_check_failure "declaration-grade: first check" (fun () ->
        S.check t ~loc:loc_1 ~test:"first" ~scope:(Some scope_a) ~name:"dup" "x")
  in
  let fl =
    expect_check_failure "declaration-grade: second check" (fun () ->
        S.check t ~loc:loc_2 ~test:"second" ~scope:(Some scope_a) ~name:"dup"
          "x")
  in
  match snapshot_payload "declaration-grade: second check" fl with
  | Some (_, _, Failure.Duplicate { first; first_test }) ->
      check "declaration-grade: first site reported" (first = Some loc_1);
      check_string "declaration-grade: first checker reported" ~expected:"first"
        ~actual:first_test
  | _ -> check "declaration-grade: state is Duplicate" false

let () =
  reg "recheck: retry by the same test is not a duplicate" @@ fun () ->
  with_root @@ fun root ->
  (* Retry attempts re-execute the same call with the same run-unique
     [test]; sites unknown on both attempts must not read as a duplicate. *)
  let t = S.create ~root ~mode:S.Check () in
  let attempt () = S.check t ~test:"t" ~scope:(Some scope_a) ~name:"n" "x" in
  let first = expect_check_failure "retry: first attempt" attempt in
  let second = expect_check_failure "retry: second attempt" attempt in
  let is_missing label fl =
    match snapshot_payload label fl with
    | Some (_, _, Failure.Missing _) -> true
    | _ -> false
  in
  check "retry: first attempt is Missing, not Duplicate"
    (is_missing "retry: first attempt" first);
  check "retry: second attempt is Missing, not Duplicate"
    (is_missing "retry: second attempt" second)

let () =
  reg "recheck: one site shared by two tests is a cases-family recheck"
  @@ fun () ->
  with_root @@ fun root ->
  (* A [cases] family: one textual call site checked by every generated
     test. Equal sites bless the recheck whatever [test] says. *)
  write_raw (path_a root "shared") "x\n";
  let t = S.create ~root ~mode:S.Check () in
  expect_pass "cases-family: first test checks" (fun () ->
      S.check t ~loc:loc_1 ~test:"family › a" ~scope:(Some scope_a)
        ~name:"shared" "x");
  expect_pass "cases-family: second test rechecks the same site" (fun () ->
      S.check t ~loc:loc_1 ~test:"family › b" ~scope:(Some scope_a)
        ~name:"shared" "x")

(* ───── Update mode: acceptance ───── *)

let () =
  reg "update mode: acceptance" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "accept: missing baseline is created" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
        "hello\r\nworld");
  check_string "accept: written content is canonical" ~expected:"hello\nworld\n"
    ~actual:(read_raw (path_a root "greeting"));
  check "accept: recorded as Created"
    (S.writes t = [ (path_a root "greeting", S.Created) ]);
  check "accept: no temporary residue"
    (sorted_entries (dir_a root) = [ "greeting.snap" ]);
  (* Same site, same content: recheck, no second write. *)
  expect_pass "accept: same-site equal recheck" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
        "hello\nworld\n");
  check_int "accept: equal recheck writes nothing" ~expected:1
    ~actual:(List.length (S.writes t));
  (* Same site, different content: never last-write-wins. *)
  let fl =
    expect_check_failure "accept: one name, two contents" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"greeting"
          "other")
  in
  (match snapshot_payload "accept: one name, two contents" fl with
  | Some (_, _, Failure.Mismatch { expected; actual }) ->
      check_string "accept: mismatch against the accepted content"
        ~expected:"hello\nworld\n" ~actual:expected;
      check_string "accept: divergent content reported" ~expected:"other\n"
        ~actual
  | _ -> check "accept: two contents is a Mismatch" false);
  check_string "accept: baseline still holds the first accepted content"
    ~expected:"hello\nworld\n"
    ~actual:(read_raw (path_a root "greeting"));
  check_int "accept: divergence writes nothing" ~expected:1
    ~actual:(List.length (S.writes t))

let () =
  reg "update mode: existing baselines updated or untouched" @@ fun () ->
  with_root @@ fun root ->
  (* An existing baseline that differs is Updated; an equal one (after
     canonicalization) is left alone. *)
  write_raw (path_a root "stale") "old\n";
  write_raw (path_a root "same") "keep\r\n";
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "accept: differing baseline is updated" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"stale" "new");
  expect_pass "accept: canonically equal baseline untouched" (fun () ->
      S.check t ~loc:loc_2 ~test:"t" ~scope:(Some scope_a) ~name:"same" "keep");
  check_string "accept: updated content" ~expected:"new\n"
    ~actual:(read_raw (path_a root "stale"));
  check_string "accept: equal baseline bytes unchanged on disk"
    ~expected:"keep\r\n"
    ~actual:(read_raw (path_a root "same"));
  check "accept: only the differing path is recorded, as Updated"
    (S.writes t = [ (path_a root "stale", S.Updated) ])

(* ───── Writes report order ───── *)

let () =
  reg "writes report in occurrence order" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "writes: first" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"zz" "1");
  expect_pass "writes: second" (fun () ->
      S.check t ~loc:loc_2 ~test:"t" ~scope:(Some scope_a) ~name:"aa" "2");
  check "writes: occurrence order, not sorted"
    (S.writes t
    = [ (path_a root "zz", S.Created); (path_a root "aa", S.Created) ])

(* ───── Scope spellings resolve to one identity ───── *)

let () =
  reg "scope spellings resolve to one identity" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "spelling: relative scope" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"one" "x");
  (* The same file spelled through the build dir or absolutely is the same
     scoping file: same site + same content = recheck, no new write. *)
  expect_pass "spelling: build-dir scope" (fun () ->
      S.check t ~loc:loc_1 ~test:"t"
        ~scope:(Some ("_build/default/" ^ scope_a))
        ~name:"one" "x");
  expect_pass "spelling: absolute scope" (fun () ->
      S.check t ~loc:loc_1 ~test:"t"
        ~scope:(Some (root ^ "/" ^ scope_a))
        ~name:"one" "x");
  check_int "spelling: one baseline written" ~expected:1
    ~actual:(List.length (S.writes t));
  check "spelling: single snapshot file"
    (sorted_entries (dir_a root) = [ "one.snap" ])

(* ───── Registry isolation (Law 9) ───── *)

let () =
  reg "registry isolation (Law 9)" @@ fun () ->
  with_root @@ fun root ->
  let r1 = S.create ~root ~mode:S.Update () in
  expect_pass "isolation: run 1 accepts" (fun () ->
      S.check r1 ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"iso" "x");
  (* A fresh registry has no memory of run 1's sites: a different site
     reads the committed baseline and passes. *)
  let r2 = S.create ~root ~mode:S.Check () in
  expect_pass "isolation: run 2 is a fresh namespace" (fun () ->
      S.check r2 ~loc:loc_2 ~test:"t" ~scope:(Some scope_a) ~name:"iso" "x")

(* ───── Orphans ───── *)

let () =
  reg "orphans" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "orphans: reference a" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"a" "x");
  write_raw (dir_a root ^ "/stale2.snap") "s2\n";
  write_raw (dir_a root ^ "/stale1.snap") "s1\n";
  write_raw (dir_a root ^ "/.tmp-leftover.snap") "t\n";
  write_raw (dir_a root ^ "/notes.txt") "n\n";
  (* An unconsulted snapshot directory is out of scope. *)
  write_raw (root ^ "/test/__snapshots__/other/unseen.snap") "u\n";
  check "orphans: stale .snap files, sorted; temps and non-.snap skipped"
    (S.orphans t = [ dir_a root ^ "/stale1.snap"; dir_a root ^ "/stale2.snap" ]);

  (* Case-insensitive referencing: a baseline whose on-disk spelling
     differs only by case from a checked name is not an orphan. *)
  let r2 = S.create ~root ~mode:S.Update () in
  expect_pass "orphans: recheck under different case" (fun () ->
      S.check r2 ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"A" "x");
  let stale_only =
    List.filter
      (fun p ->
        Filename.basename p <> "stale1.snap"
        && Filename.basename p <> "stale2.snap")
      (S.orphans r2)
  in
  check "orphans: case-differing spelling of a checked name is referenced"
    (stale_only = [])

(* ───── Prune ───── *)

let () =
  reg "prune: refusal conditions" @@ fun () ->
  with_root @@ fun root ->
  let refusal label result pred =
    match result with
    | Ok _ -> check (label ^ ": refused") false
    | Error r -> check (label ^ ": refused") (pred r)
  in
  let c = S.create ~root ~mode:S.Check () in
  refusal "prune: check-mode run"
    (S.prune c ~filtered:false ~skipped:0 ~failed:0 ~focused:0) (fun r ->
      r.S.not_update_run && (not r.S.filtered) && r.S.skipped = 0
      && r.S.failed = 0 && r.S.focused = 0);
  let u = S.create ~root ~mode:S.Update () in
  refusal "prune: filtered run"
    (S.prune u ~filtered:true ~skipped:0 ~failed:0 ~focused:0) (fun r ->
      r.S.filtered && not r.S.not_update_run);
  refusal "prune: skipped tests"
    (S.prune u ~filtered:false ~skipped:2 ~failed:0 ~focused:0) (fun r ->
      r.S.skipped = 2);
  refusal "prune: failed tests"
    (S.prune u ~filtered:false ~skipped:0 ~failed:1 ~focused:0) (fun r ->
      r.S.failed = 1);
  refusal "prune: focused tests"
    (S.prune u ~filtered:false ~skipped:0 ~failed:0 ~focused:3) (fun r ->
      r.S.focused = 3);
  refusal "prune: all blockers recorded together"
    (S.prune c ~filtered:true ~skipped:1 ~failed:2 ~focused:3) (fun r ->
      r.S.not_update_run && r.S.filtered && r.S.skipped = 1 && r.S.failed = 2
      && r.S.focused = 3);
  expect_invalid_arg "prune: negative count" (fun () ->
      ignore (S.prune u ~filtered:false ~skipped:(-1) ~failed:0 ~focused:0))

let () =
  reg "prune: clean full update run deletes orphans" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "prune: reference kept" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"kept" "x");
  write_raw (dir_a root ^ "/stale.snap") "s\n";
  write_raw (dir_a root ^ "/.tmp-leftover.snap") "t\n";
  write_raw (dir_a root ^ "/notes.txt") "n\n";
  (match S.prune t ~filtered:false ~skipped:0 ~failed:0 ~focused:0 with
  | Ok deleted ->
      check "prune: clean full update run deletes the orphans"
        (deleted = [ dir_a root ^ "/stale.snap" ])
  | Error _ -> check "prune: clean full update run deletes the orphans" false);
  check "prune: only orphaned .snap files removed"
    (sorted_entries (dir_a root)
    = [ ".tmp-leftover.snap"; "kept.snap"; "notes.txt" ])

(* ───── Duplicate detection has priority over content ───── *)

let () =
  reg "duplicate detection has priority over content" @@ fun () ->
  with_root @@ fun root ->
  (* A second call site fails Duplicate even when its content matches the
     baseline: the RFC's duplicate rule is unconditional, and an
     implementation that compared content first would pass silently. *)
  write_raw (path_a root "settled") "x\n";
  let t = S.create ~root ~mode:S.Check () in
  expect_pass "dup-priority: first site matches" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"settled" "x");
  let fl =
    expect_check_failure "dup-priority: second site with equal content"
      (fun () ->
        S.check t ~loc:loc_2 ~test:"t" ~scope:(Some scope_a) ~name:"settled" "x")
  in
  match snapshot_payload "dup-priority: second site with equal content" fl with
  | Some (_, _, Failure.Duplicate { first; _ }) ->
      check "dup-priority: reports the first site" (first = Some loc_1)
  | _ -> check "dup-priority: state is Duplicate" false

let () =
  reg "duplicate in update mode never writes" @@ fun () ->
  with_root @@ fun root ->
  (* In update mode a duplicate fails before any acceptance: one write,
     baseline untouched by the second site. *)
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "dup-update: first site accepts" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"once" "x");
  let fl =
    expect_check_failure "dup-update: second site" (fun () ->
        S.check t ~loc:loc_2 ~test:"t" ~scope:(Some scope_a) ~name:"once" "y")
  in
  (match snapshot_payload "dup-update: second site" fl with
  | Some (_, _, Failure.Duplicate _) ->
      check "dup-update: state is Duplicate" true
  | _ -> check "dup-update: state is Duplicate" false);
  check "dup-update: duplicate never writes"
    (S.writes t = [ (path_a root "once", S.Created) ]);
  check_string "dup-update: baseline holds the first site's content"
    ~expected:"x\n"
    ~actual:(read_raw (path_a root "once"))

(* ───── Missing is frozen at the first check ───── *)

let () =
  reg "missing is frozen at the first check" @@ fun () ->
  with_root @@ fun root ->
  (* A baseline appearing on disk mid-run (say, written by a parallel
     executable) is not re-read: the run observed "absent" at the first
     check and every later check compares against that same absence. *)
  let t = S.create ~root ~mode:S.Check () in
  let _ =
    expect_check_failure "frozen-absent: first check" (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"late" "late")
  in
  write_raw (path_a root "late") "late\n";
  let fl =
    expect_check_failure "frozen-absent: recheck after mid-run creation"
      (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"late" "late")
  in
  match snapshot_payload "frozen-absent: recheck after mid-run creation" fl with
  | Some (_, _, Failure.Missing _) -> check "frozen-absent: still Missing" true
  | _ -> check "frozen-absent: still Missing" false

(* ───── The run baseline never changes once established ───── *)

let () =
  reg "the run baseline never changes once established" @@ fun () ->
  with_root @@ fun root ->
  (* An acceptance whose content equals the disk baseline writes nothing
     but still establishes the run's baseline for the name; a later check
     with different content is a Mismatch against it, never a late
     acceptance that would invalidate the earlier comparison. *)
  write_raw (path_a root "held") "disk\n";
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "established: equal acceptance writes nothing" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"held" "disk");
  check "established: equal acceptance unrecorded" (S.writes t = []);
  let fl =
    expect_check_failure "established: differing recheck cannot re-accept"
      (fun () ->
        S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"held" "new")
  in
  (match
     snapshot_payload "established: differing recheck cannot re-accept" fl
   with
  | Some (_, _, Failure.Mismatch { expected; _ }) ->
      check_string "established: compared against the established baseline"
        ~expected:"disk\n" ~actual:expected
  | _ -> check "established: differing recheck state is Mismatch" false);
  check "established: nothing written" (S.writes t = []);
  check_string "established: baseline bytes untouched" ~expected:"disk\n"
    ~actual:(read_raw (path_a root "held"))

(* ───── Content edge cases: binary bytes, lone CR, empty ───── *)

let () =
  reg "content edge case: binary bytes" @@ fun () ->
  with_root @@ fun root ->
  (* Snapshots are byte strings apart from newline canonicalization: NUL
     and non-UTF-8 bytes round-trip exactly. *)
  let content = "byte\x00s \xff\xfe raw" in
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "binary: acceptance" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"bin" content);
  check_string "binary: on-disk bytes exact plus trailing newline"
    ~expected:(content ^ "\n")
    ~actual:(read_raw (path_a root "bin"));
  let c = S.create ~root ~mode:S.Check () in
  expect_pass "binary: fresh run matches" (fun () ->
      S.check c ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"bin" content)

let () =
  reg "content edge case: lone CR" @@ fun () ->
  with_root @@ fun root ->
  (* Lone CR is a line ending: both sides normalize it to LF. *)
  write_raw (path_a root "cr") "a\rb";
  let t = S.create ~root ~mode:S.Check () in
  expect_pass "lone CR: baseline CR vs actual LF" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"cr" "a\nb");
  expect_pass "lone CR: actual CR spelling matches too" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"cr" "a\rb")

let () =
  reg "content edge case: empty string" @@ fun () ->
  with_root @@ fun root ->
  (* The empty string canonicalizes to a single newline. *)
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "empty: acceptance" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"empty" "");
  check_string "empty: written as one newline" ~expected:"\n"
    ~actual:(read_raw (path_a root "empty"));
  let c = S.create ~root ~mode:S.Check () in
  expect_pass "empty: empty actual matches" (fun () ->
      S.check c ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"empty" "");
  expect_pass "empty: explicit newline matches" (fun () ->
      S.check c ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"empty" "\n")

(* ───── Degenerate scopes never resolve to the root itself ───── *)

let () =
  reg "degenerate scopes never resolve to the root" @@ fun () ->
  with_root @@ fun root ->
  (* "" and "." normalize to the root, which is not strictly under the
     root: Unresolvable, not a baseline dir at [<root>/__snapshots__]. *)
  let t = S.create ~root ~mode:S.Update () in
  let degenerate label scope =
    let fl =
      expect_check_failure label (fun () ->
          S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope) ~name:"n" "x")
    in
    match snapshot_payload label fl with
    | Some (_, _, Failure.Unresolvable) -> check (label ^ ": Unresolvable") true
    | _ -> check (label ^ ": Unresolvable") false
  in
  degenerate "degenerate scope: empty string" "";
  degenerate "degenerate scope: dot" ".";
  degenerate "degenerate scope: root itself" root;
  check "degenerate scope: nothing created" (sorted_entries root = [])

(* ───── Prune: refusal has no side effects; failed removals omitted ───── *)

let () =
  reg "prune refusal has no side effects" @@ fun () ->
  with_root @@ fun root ->
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "prune-refusal: reference kept" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"kept" "x");
  write_raw (dir_a root ^ "/stale.snap") "s\n";
  (match S.prune t ~filtered:false ~skipped:0 ~failed:1 ~focused:0 with
  | Error _ -> check "prune-refusal: refused" true
  | Ok _ -> check "prune-refusal: refused" false);
  check "prune-refusal: orphan file untouched by a refusal"
    (Sys.file_exists (dir_a root ^ "/stale.snap"))

let () =
  reg "prune omits failed removals" @@ fun () ->
  with_root @@ fun root ->
  (* A directory named [*.snap] is reported as an orphan but its removal
     fails: it is omitted from the deleted list and left in place. *)
  let t = S.create ~root ~mode:S.Update () in
  expect_pass "prune-undeletable: reference kept" (fun () ->
      S.check t ~loc:loc_1 ~test:"t" ~scope:(Some scope_a) ~name:"kept" "x");
  write_raw (dir_a root ^ "/stale.snap") "s\n";
  Unix.mkdir (dir_a root ^ "/dir.snap") 0o700;
  check "prune-undeletable: both reported as orphans"
    (S.orphans t = [ dir_a root ^ "/dir.snap"; dir_a root ^ "/stale.snap" ]);
  (match S.prune t ~filtered:false ~skipped:0 ~failed:0 ~focused:0 with
  | Ok deleted ->
      check "prune-undeletable: only the removable file is deleted"
        (deleted = [ dir_a root ^ "/stale.snap" ])
  | Error _ ->
      check "prune-undeletable: only the removable file is deleted" false);
  check "prune-undeletable: directory left in place"
    (sorted_entries (dir_a root) = [ "dir.snap"; "kept.snap" ])

(* ───── Summary ───── *)

(* ───── Suite ───── *)

let tests = List.rev !registered
