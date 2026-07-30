(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Test_tree: inert construction, path derivation and the frozen
   separator, tag inheritance, focus propagation and sites, declaration-file
   capture (?pos preferred, backtrace fallback), cases naming, bracket kept
   as three independent closures, and xfail annotation propagation
   (innermost wins). The trees under test are inert data built with
   [Test_tree] directly — never executed by the hosting runner. *)

open Windtrap
open Windtrap.Private
module T = Test_tree

(* Each [let () = reg name @@ fun () -> ...] block below registers one
   windtrap test; [tests] collects them in declaration order. *)
let registered = ref []
let reg name body = registered := Windtrap.test name body :: !registered
let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let check_paths name ~expected ~actual =
  equal ~msg:name (list (list string)) expected actual

let expect_invalid_arg name fn =
  raises_match ~msg:name Check.Exn.invalid_arg (fun () -> fn ())

let nop () = ()

exception Boom

(* ───── Construction and path derivation ───── *)

let () =
  reg "flatten derives depth-first paths" @@ fun () ->
  let tree =
    [
      T.test "alpha" nop;
      T.group "outer"
        [ T.test "beta" nop; T.group "inner" [ T.test "gamma" nop ] ];
      T.test "delta" nop;
    ]
  in
  let cases = T.flatten tree in
  check_paths "flatten derives depth-first paths in declaration order"
    ~expected:
      [
        [ "alpha" ];
        [ "outer"; "beta" ];
        [ "outer"; "inner"; "gamma" ];
        [ "delta" ];
      ]
    ~actual:(List.map (fun (c : T.case) -> c.path) cases);
  check "empty group contributes no cases" (T.flatten [ T.group "g" [] ] = [])

let () =
  reg "path_to_string joins with the frozen separator" @@ fun () ->
  check_string "path_to_string joins with the frozen separator"
    ~expected:"users › sessions after login"
    ~actual:(T.path_to_string [ "users"; "sessions after login" ]);
  check_string "path_to_string of a single segment is the segment"
    ~expected:"alpha"
    ~actual:(T.path_to_string [ "alpha" ])

(* ───── Declaration is inert; bodies run when invoked ───── *)

let () =
  reg "declaration is inert; bodies run when invoked" @@ fun () ->
  let ran = ref 0 in
  let tree = [ T.test "t" (fun () -> incr ran) ] in
  check_int "declaring runs no body" ~expected:0 ~actual:!ran;
  match T.flatten tree with
  | [ { T.body = T.Body fn; _ } ] ->
      fn ();
      check_int "flattened body runs on invocation" ~expected:1 ~actual:!ran
  | _ -> check "flattened body runs on invocation" false

(* ───── Defaults ───── *)

let () =
  reg "defaults" @@ fun () ->
  match T.flatten [ T.test "t" nop ] with
  | [ c ] ->
      check "default: no tags" (Tag.is_empty c.T.tags);
      check "default: not focused" (not c.T.focused);
      check "default: no timeout" (c.T.timeout = None);
      check_int "default: zero retries" ~expected:0 ~actual:c.T.retries;
      check "default: loc captured from the declaration backtrace"
        (match c.T.loc with
        | Some loc -> Filename.basename loc.Loc.file = "test_test_tree.ml"
        | None -> false)
  | _ -> check "default flatten shape" false

let () =
  reg "timeout and retries recorded" @@ fun () ->
  match T.flatten [ T.test ~timeout:2.5 ~retries:3 "t" nop ] with
  | [ c ] ->
      check "timeout recorded" (c.T.timeout = Some 2.5);
      check_int "retries recorded" ~expected:3 ~actual:c.T.retries
  | _ -> check "timeout/retries flatten shape" false

(* ───── Validation ───── *)

let () =
  reg "validation rejects bad retries and timeouts" @@ fun () ->
  expect_invalid_arg "negative retries rejected" (fun () ->
      T.test ~retries:(-1) "t" nop);
  expect_invalid_arg "zero timeout rejected" (fun () ->
      T.test ~timeout:0. "t" nop);
  expect_invalid_arg "negative timeout rejected" (fun () ->
      T.test ~timeout:(-1.) "t" nop);
  expect_invalid_arg "nan timeout rejected" (fun () ->
      T.test ~timeout:Float.nan "t" nop);
  expect_invalid_arg "infinite timeout rejected" (fun () ->
      T.test ~timeout:Float.infinity "t" nop);
  expect_invalid_arg "bracket validates retries too" (fun () ->
      T.bracket ~retries:(-2) ~setup:nop ~teardown:ignore "t" ignore)

(* ───── Tags ───── *)

let () =
  reg "tag inheritance" @@ fun () ->
  let tree =
    [
      T.group ~tags:[ "db" ] "g"
        [ T.test ~tags:[ "net" ] "a" nop; T.test "b" nop ];
    ]
  in
  match T.flatten tree with
  | [ a; b ] ->
      check "child unions its own tags with the group's"
        (Tag.mem "db" a.T.tags && Tag.mem "net" a.T.tags);
      check "sibling gets only inherited tags"
        (Tag.mem "db" b.T.tags && not (Tag.mem "net" b.T.tags))
  | _ -> check "tag flatten shape" false

let () =
  reg "slow pre-applies the slow tag" @@ fun () ->
  match T.flatten [ T.slow ~tags:[ "x" ] "s" nop ] with
  | [ c ] ->
      check "slow pre-applies the slow tag"
        (Tag.mem Tag.slow c.T.tags && Tag.mem "x" c.T.tags)
  | _ -> check "slow flatten shape" false

(* ───── Focus ───── *)

let () =
  reg "has_focus" @@ fun () ->
  check "no focus by default" (not (T.has_focus [ T.test "t" nop ]));
  check "ftest sets focus" (T.has_focus [ T.ftest "t" nop ]);
  check "fgroup sets focus" (T.has_focus [ T.fgroup "g" [] ]);
  check "focus found in nested groups"
    (T.has_focus [ T.group "g" [ T.group "h" [ T.ftest "t" nop ] ] ])

let () =
  reg "focus propagation" @@ fun () ->
  let tree =
    [
      T.fgroup "g" [ T.test "in-focused-group" nop ];
      T.group "h" [ T.ftest "focused" nop; T.test "plain" nop ];
    ]
  in
  match T.flatten tree with
  | [ a; b; c ] ->
      check "fgroup focuses its descendants" a.T.focused;
      check "ftest is focused" b.T.focused;
      check "sibling of a focused test is not focused" (not c.T.focused)
  | _ -> check "focus flatten shape" false

let () =
  reg "focus_sites records kinds and locations" @@ fun () ->
  let pos_t = ("test/fake_t.ml", 31, 2, 10) in
  let pos_g = ("test/fake_g.ml", 7, 0, 4) in
  let tree =
    [
      T.group "outer" [ T.ftest ~pos:pos_t "t" nop; T.fgroup ~pos:pos_g "g" [] ];
      T.test "plain" nop;
    ]
  in
  match T.focus_sites tree with
  | [ (`Ftest, Some lt); (`Fgroup, Some lg) ] ->
      check_string "focus site records the ftest file"
        ~expected:"test/fake_t.ml" ~actual:lt.Loc.file;
      check_int "focus site records the ftest line" ~expected:31
        ~actual:lt.Loc.line;
      check_string "focus site records the fgroup file"
        ~expected:"test/fake_g.ml" ~actual:lg.Loc.file
  | _ -> check "focus_sites shape (declaration order, kinds, locs)" false

(* ───── Declaration files ───── *)

let () =
  reg "?pos wins for the declaration file" @@ fun () ->
  let pos = ("src/elsewhere.ml", 12, 0, 8) in
  match T.flatten [ T.test ~pos "t" nop ] with
  | [ c ] ->
      check "?pos wins for the declaration file"
        (c.T.file = Some "src/elsewhere.ml");
      check "?pos wins for the location"
        (match c.T.loc with
        | Some loc -> loc.Loc.file = "src/elsewhere.ml" && loc.Loc.line = 12
        | None -> false)
  | _ -> check "?pos flatten shape" false

let () =
  reg "backtrace fallback records the declaring file" @@ fun () ->
  match T.flatten [ T.test "t" nop ] with
  | [ c ] ->
      check "backtrace fallback records this file"
        (match c.T.file with
        | Some file -> Filename.basename file = "test_test_tree.ml"
        | None -> false)
  | _ -> check "backtrace flatten shape" false

let () =
  reg "nested tests keep their own declaration file" @@ fun () ->
  (* The declaration file belongs to the test, not its group: a child keeps
     its own capture even when nested. *)
  match T.flatten [ T.group "g" [ T.test "t" nop ] ] with
  | [ c ] ->
      check "nested test still records its own declaration file"
        (match c.T.file with
        | Some file -> Filename.basename file = "test_test_tree.ml"
        | None -> false)
  | _ -> check "nested declaration-file shape" false

(* ───── cases ───── *)

let () =
  reg "cases derives indexed sub-paths" @@ fun () ->
  let seen = ref [] in
  let tree = T.cases "double" [ 1; 2; 3 ] (fun n -> seen := n :: !seen) in
  let flat = T.flatten [ tree ] in
  check_paths "cases derives <name>.<index> sub-paths"
    ~expected:
      [
        [ "double"; "double.0" ];
        [ "double"; "double.1" ];
        [ "double"; "double.2" ];
      ]
    ~actual:(List.map (fun (c : T.case) -> c.path) flat);
  check_int "cases bodies do not run at declaration" ~expected:0
    ~actual:(List.length !seen);
  List.iter
    (fun (c : T.case) ->
      match c.T.body with T.Body fn -> fn () | T.Bracket _ -> ())
    flat;
  check "each cases body receives its own input" (List.rev !seen = [ 1; 2; 3 ])

let () =
  reg "cases ?name names sub-tests from values" @@ fun () ->
  let tree = T.cases ~name:string_of_int "n" [ 10; 20 ] ignore in
  check_paths "cases ?name names sub-tests from values"
    ~expected:[ [ "n"; "10" ]; [ "n"; "20" ] ]
    ~actual:(List.map (fun (c : T.case) -> c.path) (T.flatten [ tree ]))

let () =
  reg "cases metadata and empty input" @@ fun () ->
  let pos = ("test/fake_cases.ml", 5, 0, 0) in
  let flat = T.flatten [ T.cases ~pos ~tags:[ "tbl" ] "c" [ 0; 1 ] ignore ] in
  check "cases tags reach every sub-test"
    (List.for_all (fun (c : T.case) -> Tag.mem "tbl" c.T.tags) flat);
  check "cases sub-tests share the declaration file"
    (List.for_all
       (fun (c : T.case) -> c.T.file = Some "test/fake_cases.ml")
       flat);
  check "cases with no inputs flattens to nothing"
    (T.flatten [ T.cases "empty" [] ignore ] = [])

(* ───── bracket ───── *)

let () =
  reg "bracket phases run in order with the resource" @@ fun () ->
  let log = ref [] in
  let mark step = log := step :: !log in
  let tree =
    T.bracket
      ~setup:(fun () ->
        mark "setup";
        42)
      ~teardown:(fun r -> mark (Printf.sprintf "teardown %d" r))
      "b"
      (fun r -> mark (Printf.sprintf "body %d" r))
  in
  check "declaring a bracket runs nothing" (!log = []);
  match T.flatten [ tree ] with
  | [ { T.body = T.Bracket { setup; body; teardown }; _ } ] ->
      let resource = setup () in
      body resource;
      teardown resource;
      check "bracket phases run in order with the exact resource"
        (List.rev !log = [ "setup"; "body 42"; "teardown 42" ])
  | _ -> check "bracket flatten shape" false

let () =
  reg "bracket teardown is reachable after a body failure" @@ fun () ->
  (* The three closures are independent: a body failure cannot prevent the
     runner from running teardown, because they were never composed. *)
  let log = ref [] in
  let mark step = log := step :: !log in
  let tree =
    T.bracket
      ~setup:(fun () ->
        mark "setup";
        ref 1)
      ~teardown:(fun _ -> mark "teardown")
      "b"
      (fun _ ->
        mark "body";
        raise Boom)
  in
  match T.flatten [ tree ] with
  | [ { T.body = T.Bracket { setup; body; teardown }; _ } ] ->
      let resource = setup () in
      let body_failed =
        match body resource with () -> false | exception Boom -> true
      in
      teardown resource;
      check "bracket teardown is reachable after a body failure"
        (body_failed && List.rev !log = [ "setup"; "body"; "teardown" ])
  | _ -> check "bracket independence shape" false

let () =
  reg "bracket records its metadata" @@ fun () ->
  match
    T.flatten
      [
        T.bracket ~pos:("f.ml", 1, 0, 0) ~tags:[ "db" ] ~timeout:1.5 ~retries:2
          ~setup:nop ~teardown:ignore "b" ignore;
      ]
  with
  | [ c ] ->
      check "bracket records tags" (Tag.mem "db" c.T.tags);
      check "bracket records timeout" (c.T.timeout = Some 1.5);
      check_int "bracket records retries" ~expected:2 ~actual:c.T.retries;
      check "bracket records the declaration file" (c.T.file = Some "f.ml")
  | _ -> check "bracket metadata shape" false

(* ───── xfail (amendment B12) ───── *)

let () =
  reg "xfail defaults to None" @@ fun () ->
  match T.flatten [ T.test "t" nop ] with
  | [ c ] -> check "default: not expected to fail" (c.T.xfail = None)
  | _ -> check "default xfail shape" false

let () =
  reg "xfail marks a leaf with its reason" @@ fun () ->
  match T.flatten [ T.xfail ~reason:"issue #42" (T.test "t" nop) ] with
  | [ c ] ->
      check "xfail marks a leaf with its reason"
        (c.T.xfail = Some { T.reason = Some "issue #42" })
  | _ -> check "xfail leaf shape" false

let () =
  reg "xfail without a reason still marks" @@ fun () ->
  match T.flatten [ T.xfail (T.test "t" nop) ] with
  | [ c ] ->
      check "xfail without a reason still marks"
        (c.T.xfail = Some { T.reason = None })
  | _ -> check "xfail reasonless shape" false

let () =
  reg "xfail on a group reaches every descendant" @@ fun () ->
  let tree =
    [
      T.xfail ~reason:"backend bug"
        (T.group "g" [ T.test "a" nop; T.test "b" nop ]);
    ]
  in
  match T.flatten tree with
  | [ a; b ] ->
      check "xfail on a group reaches every descendant"
        (a.T.xfail = Some { T.reason = Some "backend bug" }
        && b.T.xfail = Some { T.reason = Some "backend bug" })
  | _ -> check "xfail group shape" false

let () =
  reg "the innermost xfail annotation wins" @@ fun () ->
  (* Innermost annotation wins: re-marking inside an xfail group refines the
     reason; unmarked siblings inherit the group's. *)
  let tree =
    [
      T.xfail ~reason:"outer"
        (T.group "g"
           [
             T.xfail ~reason:"inner" (T.test "refined" nop); T.test "plain" nop;
           ]);
    ]
  in
  match T.flatten tree with
  | [ refined; plain ] ->
      check "the innermost annotation wins"
        (refined.T.xfail = Some { T.reason = Some "inner" });
      check "siblings inherit the group annotation"
        (plain.T.xfail = Some { T.reason = Some "outer" })
  | _ -> check "nested xfail shape" false

let () =
  reg "xfail composes with node attributes" @@ fun () ->
  (* xfail composes with the other node attributes instead of clobbering
     them. *)
  match
    T.flatten
      [
        T.xfail ~reason:"r"
          (T.test ~tags:[ "db" ] ~timeout:1.5 ~retries:2 "t" nop);
      ]
  with
  | [ c ] ->
      check "xfail keeps tags" (Tag.mem "db" c.T.tags);
      check "xfail keeps timeout and retries"
        (c.T.timeout = Some 1.5 && c.T.retries = 2)
  | _ -> check "xfail attribute shape" false

let () =
  reg "xfail preserves focus flags" @@ fun () ->
  check "xfail preserves focus flags"
    (T.has_focus [ T.xfail (T.ftest "t" nop) ]
    && T.has_focus [ T.xfail (T.fgroup "g" []) ])

(* ───── Suite ───── *)

let tests = List.rev !registered
