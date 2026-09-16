(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Test_tree: inert construction, path derivation and the frozen
   separator, the optional arguments (tags, timeout, retries) on every
   constructor — group-level defaults, innermost-wins, tag union, [slow] —
   the two annotations (focus, xfail) and the sites they preserve,
   declaration-site capture (?__POS__ preferred, backtrace fallback), cases
   naming, bracket as a scope derived from its setup and teardown, and scoped
   kept as a scope and a body (with the argument order that keeps its
   optionals through a partial application), and the tag sets and selection
   predicates of [Test_tree.Tag]. The trees under test are inert
   data built with [Test_tree] directly — never executed by the hosting
   runner. *)

open Windtrap
open Windtrap.Private
module Tag = Test_tree.Tag
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

(* The one flattened case of a single-test tree. *)
let only name tree =
  match T.flatten [ tree ] with
  | [ c ] -> c
  | cases -> failf "%s: expected one case, got %d" name (List.length cases)

let loc_file (c : T.case) =
  match c.T.loc with Some loc -> loc.Loc.file | None -> "<none>"

(* Construction and path derivation *)

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

(* Declaration is inert; bodies run when invoked *)

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

(* Defaults *)

let () =
  reg "defaults" @@ fun () ->
  let c = only "defaults" (T.test "t" nop) in
  check "default: no tags" (not (Tag.mem "slow" c.T.tags));
  check "default: not focused" (not c.T.focused);
  check "default: no timeout" (c.T.timeout = None);
  check_int "default: zero retries" ~expected:0 ~actual:c.T.retries;
  check "default: not expected to fail" (c.T.xfail = None);
  check "default: loc captured from the declaration backtrace"
    (Filename.basename (loc_file c) = "test_test_tree.ml")

(* The optional arguments, one at a time *)

let () =
  reg "each optional argument records on the case" @@ fun () ->
  let c = only "timeout" (T.test ~timeout:2.5 "t" nop) in
  check "timeout recorded" (c.T.timeout = Some 2.5);
  let c = only "retries" (T.test ~retries:3 "t" nop) in
  check_int "retries recorded" ~expected:3 ~actual:c.T.retries;
  let c = only "tags" (T.test ~tags:[ "db"; "slow" ] "t" nop) in
  check "tags recorded" (Tag.mem "db" c.T.tags && Tag.mem Tag.slow c.T.tags);
  let c = only "slow" (T.slow ~tags:[ "x" ] "s" nop) in
  check "slow pre-applies the slow tag and keeps the declared ones"
    (Tag.mem Tag.slow c.T.tags && Tag.mem "x" c.T.tags)

let () =
  reg "every constructor takes the optional arguments" @@ fun () ->
  let carries name tree =
    List.iter
      (fun (c : T.case) ->
        check (name ^ ": timeout") (c.T.timeout = Some 1.5);
        check (name ^ ": retries") (c.T.retries = 2);
        check (name ^ ": tag") (Tag.mem "x" c.T.tags))
      (T.flatten [ tree ])
  in
  carries "test" (T.test ~tags:[ "x" ] ~timeout:1.5 ~retries:2 "t" nop);
  carries "slow" (T.slow ~tags:[ "x" ] ~timeout:1.5 ~retries:2 "s" nop);
  carries "group"
    (T.group ~tags:[ "x" ] ~timeout:1.5 ~retries:2 "g" [ T.test "t" nop ]);
  carries "cases"
    (T.cases ~tags:[ "x" ] ~timeout:1.5 ~retries:2 ~name:string_of_int "c"
       [ 0; 1 ] ignore);
  carries "bracket"
    (T.bracket ~tags:[ "x" ] ~timeout:1.5 ~retries:2 ~setup:nop ~teardown:ignore
       "b" ignore);
  carries "scoped"
    (T.scoped
       (fun fn -> fn ())
       ~tags:[ "x" ] ~timeout:1.5 ~retries:2 "s" ignore)

(* Validation *)

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
  expect_invalid_arg "a group validates retries too" (fun () ->
      T.group ~retries:(-2) "g" []);
  expect_invalid_arg "a group validates timeouts too" (fun () ->
      T.group ~timeout:0. "g" []);
  expect_invalid_arg "bracket validates retries too" (fun () ->
      T.bracket ~retries:(-2) ~setup:nop ~teardown:ignore "t" ignore);
  expect_invalid_arg "scoped validates timeouts too" (fun () ->
      T.scoped (fun fn -> fn ()) ~timeout:0. "t" ignore)

(* Tags *)

let () =
  reg "tags union with the enclosing groups'" @@ fun () ->
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

(* Group-level defaults and innermost-wins *)

let () =
  reg "a group's timeout and retries are defaults for every descendant"
  @@ fun () ->
  let tree =
    [
      T.group ~timeout:2. ~retries:2 "g"
        [ T.test "a" nop; T.group "h" [ T.test "b" nop ] ];
    ]
  in
  match T.flatten tree with
  | [ a; b ] ->
      check "direct child inherits the timeout" (a.T.timeout = Some 2.);
      check "nested child inherits the timeout" (b.T.timeout = Some 2.);
      check "direct child inherits the retries" (a.T.retries = 2);
      check "nested child inherits the retries" (b.T.retries = 2)
  | _ -> check "group default shape" false

let () =
  reg "the innermost timeout and retries win" @@ fun () ->
  let tree =
    [
      T.group ~timeout:5. ~retries:5 "g"
        [
          T.test ~timeout:1. ~retries:1 "own" nop;
          T.test "inherits" nop;
          T.group ~timeout:2. "h" [ T.test "inner group" nop ];
        ];
    ]
  in
  match T.flatten tree with
  | [ own; inherits; inner ] ->
      check "a test's own timeout beats the group's" (own.T.timeout = Some 1.);
      check "a test's own retries beat the group's" (own.T.retries = 1);
      check "an undeclared sibling takes the group's timeout"
        (inherits.T.timeout = Some 5.);
      check "an undeclared sibling takes the group's retries"
        (inherits.T.retries = 5);
      check "an inner group's timeout beats the outer group's"
        (inner.T.timeout = Some 2.);
      check "an inner group without retries passes the outer's through"
        (inner.T.retries = 5)
  | _ -> check "innermost shape" false

(* Focus *)

let () =
  reg "focus_sites finds every focused node" @@ fun () ->
  let sites tests = List.length (T.focus_sites tests) in
  check_int "no focus by default" ~expected:0 ~actual:(sites [ T.test "t" nop ]);
  check_int "focus on a test" ~expected:1
    ~actual:(sites [ T.focus (T.test "t" nop) ]);
  check_int "focus on a group" ~expected:1
    ~actual:(sites [ T.focus (T.group "g" []) ]);
  check_int "focus found in nested groups" ~expected:1
    ~actual:(sites [ T.group "g" [ T.group "h" [ T.focus (T.test "t" nop) ] ] ])

let () =
  reg "focus propagation" @@ fun () ->
  let tree =
    [
      T.focus (T.group "g" [ T.test "in-focused-group" nop ]);
      T.group "h" [ T.focus (T.test "focused" nop); T.test "plain" nop ];
    ]
  in
  match T.flatten tree with
  | [ a; b; c ] ->
      check "a focused group focuses its descendants" a.T.focused;
      check "a focused test is focused" b.T.focused;
      check "sibling of a focused test is not focused" (not c.T.focused)
  | _ -> check "focus flatten shape" false

let () =
  reg "focus_sites records the constructors' locations" @@ fun () ->
  let pos_t = ("test/fake_t.ml", 31, 2, 10) in
  let pos_g = ("test/fake_g.ml", 7, 0, 4) in
  let tree =
    [
      T.group "outer"
        [
          T.focus (T.test ~__POS__:pos_t "t" nop);
          T.focus (T.group ~__POS__:pos_g "g" []);
        ];
      T.test "plain" nop;
    ]
  in
  match T.focus_sites tree with
  | [ Some lt; Some lg ] ->
      check_string "focus site records the test's file"
        ~expected:"test/fake_t.ml" ~actual:lt.Loc.file;
      check_int "focus site records the test's line" ~expected:31
        ~actual:lt.Loc.line;
      check_string "focus site records the group's file"
        ~expected:"test/fake_g.ml" ~actual:lg.Loc.file
  | _ -> check "focus_sites shape (declaration order, locs)" false

let () =
  reg "focus applies to every constructor's result" @@ fun () ->
  let focused tree =
    List.for_all (fun (c : T.case) -> c.T.focused) (T.flatten [ T.focus tree ])
  in
  check "focus on a slow test" (focused (T.slow "s" nop));
  check "focus on cases"
    (focused (T.cases ~name:string_of_int "c" [ 0 ] ignore));
  check "focus on a bracket"
    (focused (T.bracket ~setup:nop ~teardown:ignore "b" ignore));
  check "focus on a scoped test"
    (focused (T.scoped (fun fn -> fn ()) "s" ignore))

(* Declaration sites *)

let () =
  reg "?__POS__ wins for the declaration site" @@ fun () ->
  let pos = ("src/elsewhere.ml", 12, 0, 8) in
  let c = only "?__POS__" (T.test ~__POS__:pos "t" nop) in
  check "?__POS__ wins for the declaration site"
    (match c.T.loc with
    | Some loc -> loc.Loc.file = "src/elsewhere.ml" && loc.Loc.line = 12
    | None -> false)

let () =
  reg "backtrace fallback records the declaring file" @@ fun () ->
  let c = only "backtrace" (T.test "t" nop) in
  check "backtrace fallback records this file"
    (Filename.basename (loc_file c) = "test_test_tree.ml")

let () =
  reg "nested tests keep their own declaration site" @@ fun () ->
  (* The declaration site belongs to the test, not its group: a child keeps
     its own capture even when nested. *)
  let c = only "nested" (T.group "g" [ T.test "t" nop ]) in
  check "nested test still records its own declaration site"
    (Filename.basename (loc_file c) = "test_test_tree.ml")

let () =
  reg "annotations keep the site the constructor captured" @@ fun () ->
  let pos = ("src/declared.ml", 12, 0, 8) in
  let c =
    only "wrapped test"
      (T.xfail
         (T.focus (T.test ~__POS__:pos ~tags:[ "x" ] ~timeout:1. "t" nop)))
  in
  check "a wrapped test keeps its constructor's site"
    (c.T.loc = Some (Loc.of_pos pos));
  (match T.focus_sites [ T.xfail (T.focus (T.group ~__POS__:pos "g" [])) ] with
  | [ Some site ] ->
      check "a wrapped group keeps its constructor's site"
        (site = Loc.of_pos pos)
  | _ -> check "wrapped group focus site shape" false);
  (* The fallback capture happens at construction, before any annotation
     runs, so an annotation applied later in another function cannot move
     it either. *)
  let c = only "wrapped fallback" (T.focus (T.test "t" nop)) in
  check "the fallback site is captured at construction"
    (Filename.basename (loc_file c) = "test_test_tree.ml")

let () =
  reg "backtrace fallback attributes every constructor to the declaring file"
  @@ fun () ->
  (* The fallback walks past windtrap's own frames — [make_test], the
     derived bracket's scope builder, the [cases] child loop — and lands on
     the user frame that applied the constructor, whichever constructor it
     was and whatever optional arguments it took. *)
  let declared_here name tree =
    List.iter
      (fun (c : T.case) ->
        check
          (name ^ ": the fallback site is this file")
          (Filename.basename (loc_file c) = "test_test_tree.ml"))
      (T.flatten [ tree ])
  in
  declared_here "group child" (T.group ~tags:[ "g" ] "g" [ T.test "t" nop ]);
  declared_here "cases"
    (T.cases ~timeout:1. ~name:string_of_int "c" [ 0; 1 ] ignore);
  declared_here "bracket"
    (T.bracket ~retries:1 ~setup:nop ~teardown:ignore "b" ignore);
  declared_here "scoped" (T.scoped (fun fn -> fn ()) ~tags:[ "s" ] "s" ignore);
  (* A partially applied bracket — the [with_db] idiom — captures where the
     resulting constructor is applied, still user code. *)
  let with_unit = T.bracket ~setup:nop ~teardown:ignore in
  declared_here "partially applied bracket" (with_unit ~timeout:1. "b" ignore);
  match T.focus_sites [ T.focus (T.group ~retries:1 "g" []) ] with
  | [ Some site ] ->
      check "a group's own fallback site is this file"
        (Filename.basename site.Loc.file = "test_test_tree.ml")
  | _ -> check "group fallback site shape" false

(* cases *)

let () =
  reg "cases derives sub-paths from the naming function" @@ fun () ->
  let seen = ref [] in
  let tree =
    T.cases ~name:string_of_int "double" [ 1; 2; 3 ] (fun n ->
        seen := n :: !seen)
  in
  let flat = T.flatten [ tree ] in
  check_paths "cases derives <base>/<name input> sub-paths"
    ~expected:[ [ "double"; "1" ]; [ "double"; "2" ]; [ "double"; "3" ] ]
    ~actual:(List.map (fun (c : T.case) -> c.path) flat);
  check_int "cases bodies do not run at declaration" ~expected:0
    ~actual:(List.length !seen);
  List.iter
    (fun (c : T.case) ->
      match c.T.body with T.Body fn -> fn () | T.Scoped _ -> ())
    flat;
  check "each cases body receives its own input" (List.rev !seen = [ 1; 2; 3 ])

let () =
  reg "cases forwards its optional arguments to every child" @@ fun () ->
  let pos = ("test/fake_cases.ml", 5, 0, 0) in
  let flat =
    T.flatten
      [
        T.cases ~__POS__:pos ~tags:[ "tbl" ] ~timeout:2.5 ~retries:3
          ~name:string_of_int "c" [ 0; 1 ] ignore;
      ]
  in
  check_int "two children" ~expected:2 ~actual:(List.length flat);
  List.iter
    (fun (c : T.case) ->
      check "child carries the declared budget"
        (c.T.timeout = Some 2.5 && c.T.retries = 3);
      check "child carries the tags" (Tag.mem "tbl" c.T.tags);
      check "child shares the declaration site"
        (loc_file c = "test/fake_cases.ml"))
    flat;
  expect_invalid_arg "cases rejects a zero timeout" (fun () ->
      T.cases ~timeout:0. ~name:string_of_int "t" [ 0 ] ignore);
  expect_invalid_arg "cases rejects negative retries" (fun () ->
      T.cases ~retries:(-1) ~name:string_of_int "t" [ 0 ] ignore);
  check "cases with no inputs flattens to nothing"
    (T.flatten [ T.cases ~name:string_of_int "empty" [] ignore ] = [])

(* bracket: a scope derived from setup and teardown *)

(* The derived scope, applied to its body by hand — what the runner does
   through [Run]'s scoped path. *)
let scope_of name tree =
  match T.flatten [ tree ] with
  | [ { T.body = T.Scoped { scope; body }; _ } ] -> fun () -> scope body
  | _ -> failf "%s: expected one scoped case" name

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
  scope_of "bracket" tree ();
  check "bracket phases run in order with the exact resource"
    (List.rev !log = [ "setup"; "body 42"; "teardown 42" ])

let () =
  reg "bracket runs the teardown after a body failure and re-raises"
  @@ fun () ->
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
  let body_failed =
    match scope_of "bracket" tree () with () -> false | exception Boom -> true
  in
  check "the body's exception comes back out of the scope" body_failed;
  check "the teardown ran on the failure path"
    (List.rev !log = [ "setup"; "body"; "teardown" ])

let () =
  reg "bracket skips the teardown when setup fails" @@ fun () ->
  let log = ref [] in
  let mark step = log := step :: !log in
  let tree =
    T.bracket
      ~setup:(fun () ->
        mark "setup";
        raise Boom)
      ~teardown:(fun () -> mark "teardown")
      "b"
      (fun () -> mark "body")
  in
  let setup_failed =
    match scope_of "bracket" tree () with () -> false | exception Boom -> true
  in
  check "the setup failure comes back out of the scope" setup_failed;
  check "neither the body nor the teardown ran" (List.rev !log = [ "setup" ])

let () =
  reg "bracket skips the teardown on a fatal exception" @@ fun () ->
  let log = ref [] in
  let mark step = log := step :: !log in
  let tree =
    T.bracket
      ~setup:(fun () -> mark "setup")
      ~teardown:(fun () -> mark "teardown")
      "b"
      (fun () ->
        mark "body";
        raise Stack_overflow)
  in
  let fatal =
    match scope_of "bracket" tree () with
    | () -> false
    | exception Stack_overflow -> true
  in
  check "the fatal exception propagates" fatal;
  check "the teardown did not run" (List.rev !log = [ "setup"; "body" ])

exception Teardown_boom

let () =
  reg "bracket lets a teardown failure replace the body's" @@ fun () ->
  (* The runner has already recorded the body's failure inside the callback
     by the time the teardown raises; the scope's own exception is then the
     teardown's, which the runner attributes to the teardown phase. *)
  let tree =
    T.bracket ~setup:nop
      ~teardown:(fun () -> raise Teardown_boom)
      "b"
      (fun () -> raise Boom)
  in
  check "the teardown's exception is the one that escapes"
    (match scope_of "bracket" tree () with
    | () -> false
    | exception Teardown_boom -> true
    | exception _ -> false)

let () =
  reg "bracket records its metadata" @@ fun () ->
  let c =
    only "bracket"
      (T.bracket ~__POS__:("f.ml", 1, 0, 0) ~tags:[ "db" ] ~timeout:1.5
         ~retries:2 ~setup:nop ~teardown:ignore "b" ignore)
  in
  check "bracket records tags" (Tag.mem "db" c.T.tags);
  check "bracket records timeout" (c.T.timeout = Some 1.5);
  check_int "bracket records retries" ~expected:2 ~actual:c.T.retries;
  check "bracket records the declaration site" (loc_file c = "f.ml")

(* scoped *)

let () =
  reg "scoped stores the scope and the body unrun" @@ fun () ->
  let log = ref [] in
  let mark step = log := step :: !log in
  let tree =
    T.scoped
      (fun fn ->
        mark "acquire";
        fn 42;
        mark "release")
      "s"
      (fun r -> mark (Printf.sprintf "body %d" r))
  in
  check "declaring a scoped test runs nothing" (!log = []);
  scope_of "scoped" tree ();
  check "the scope brackets the body around the resource it supplies"
    (List.rev !log = [ "acquire"; "body 42"; "release" ])

let () =
  reg "scoped records its metadata" @@ fun () ->
  let c =
    only "scoped"
      (T.scoped
         (fun fn -> fn ())
         ~__POS__:("f.ml", 1, 0, 0) ~tags:[ "eio" ] ~timeout:1.5 ~retries:2 "s"
         ignore)
  in
  check "scoped records tags" (Tag.mem "eio" c.T.tags);
  check "scoped records timeout" (c.T.timeout = Some 1.5);
  check_int "scoped records retries" ~expected:2 ~actual:c.T.retries;
  check "scoped records the declaration site" (loc_file c = "f.ml")

let () =
  (* [scope] precedes the optional arguments so that applying it does not
     erase them: this block would not compile if it did, which is the whole
     point of the argument order. *)
  reg "a partially applied scoped keeps its optional arguments" @@ fun () ->
  let with_unit = T.scoped (fun fn -> fn ()) in
  let c =
    only "partial"
      (with_unit ~__POS__:("f.ml", 3, 0, 0) ~tags:[ "eio" ] ~timeout:3. "s"
         ignore)
  in
  check "the partial application still takes ~tags" (Tag.mem "eio" c.T.tags);
  check "the partial application still takes ~timeout" (c.T.timeout = Some 3.);
  check "the partial application still takes ?__POS__" (loc_file c = "f.ml")

(* xfail *)

let () =
  reg "xfail marks a leaf with its reason" @@ fun () ->
  let c = only "xfail" (T.xfail ~reason:"issue #42" (T.test "t" nop)) in
  check "xfail marks a leaf with its reason"
    (c.T.xfail = Some { T.reason = Some "issue #42" });
  let c = only "xfail" (T.xfail (T.test "t" nop)) in
  check "xfail without a reason still marks"
    (c.T.xfail = Some { T.reason = None })

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
  reg "xfail keeps the node's arguments and composes with focus" @@ fun () ->
  let c =
    only "xfail over arguments"
      (T.xfail ~reason:"r"
         (T.test ~tags:[ "db" ] ~timeout:1.5 ~retries:2 "t" nop))
  in
  check "xfail keeps tags" (Tag.mem "db" c.T.tags);
  check "xfail keeps timeout and retries"
    (c.T.timeout = Some 1.5 && c.T.retries = 2);
  let a = only "xfail then focus" (T.xfail (T.focus (T.test "t" nop))) in
  let b = only "focus then xfail" (T.focus (T.xfail (T.test "t" nop))) in
  check "focus and xfail compose in either order"
    (a.T.focused && b.T.focused && a.T.xfail = b.T.xfail
    && a.T.xfail = Some { T.reason = None });
  check "xfail preserves focus sites"
    (T.focus_sites [ T.xfail (T.focus (T.group "g" [])) ] <> [])

(* Tag sets and selection predicates (Test_tree.Tag) *)

let () =
  reg "tags: tag sets" @@ fun () ->
  is_false ~msg:"empty has no tags" (Tag.mem "a" Tag.empty);
  is_true ~msg:"of_list mem" (Tag.mem "a" (Tag.of_list [ "a"; "b" ]));
  is_false ~msg:"mem absent" (Tag.mem "c" (Tag.of_list [ "a"; "b" ]));
  let u = Tag.union (Tag.of_list [ "a" ]) (Tag.of_list [ "b" ]) in
  is_true ~msg:"union keeps the left side" (Tag.mem "a" u);
  is_true ~msg:"union keeps the right side" (Tag.mem "b" u);
  is_false ~msg:"union invents nothing" (Tag.mem "c" u);
  is_true ~msg:"union with empty is identity"
    (Tag.mem "a" (Tag.union Tag.empty (Tag.of_list [ "a" ])));
  equal ~msg:"well-known slow" string "slow" Tag.slow

let () =
  reg "tags: any accepts every tag set" @@ fun () ->
  is_true ~msg:"accepts untagged" (Tag.accepts Tag.any Tag.empty);
  is_true ~msg:"accepts ordinary tags"
    (Tag.accepts Tag.any (Tag.of_list [ "slow" ]));
  is_true ~msg:"accepts several"
    (Tag.accepts Tag.any (Tag.of_list [ "a"; "b" ]))

let () =
  reg "tags: require and drop semantics" @@ fun () ->
  let p = Tag.require "net" Tag.any in
  is_false ~msg:"require rejects missing tag" (Tag.accepts p Tag.empty);
  is_true ~msg:"require accepts present tag"
    (Tag.accepts p (Tag.of_list [ "net" ]));
  is_true ~msg:"require accepts superset"
    (Tag.accepts p (Tag.of_list [ "net"; "x" ]));
  let p = Tag.require "a" (Tag.require "b" Tag.any) in
  is_false ~msg:"multiple requires need all"
    (Tag.accepts p (Tag.of_list [ "a" ]));
  is_true ~msg:"multiple requires satisfied"
    (Tag.accepts p (Tag.of_list [ "a"; "b" ]));
  let p = Tag.drop Tag.slow Tag.any in
  is_false ~msg:"drop rejects tagged" (Tag.accepts p (Tag.of_list [ "slow" ]));
  is_true ~msg:"drop accepts untagged" (Tag.accepts p (Tag.of_list [ "fast" ]))

let () =
  reg "tags: last flag wins when a tag is both required and dropped"
  @@ fun () ->
  let p = Tag.drop "x" (Tag.require "x" Tag.any) in
  is_false ~msg:"drop after require rejects the tag"
    (Tag.accepts p (Tag.of_list [ "x" ]));
  is_true ~msg:"drop after require does not still require it"
    (Tag.accepts p Tag.empty);
  let p = Tag.require "x" (Tag.drop "x" Tag.any) in
  is_true ~msg:"require after drop accepts the tag"
    (Tag.accepts p (Tag.of_list [ "x" ]));
  is_false ~msg:"require after drop still requires it" (Tag.accepts p Tag.empty)

(* Suite *)

let tests = List.rev !registered
let () = exit @@ Windtrap.run "test_tree" tests
