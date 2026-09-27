(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Test_tree = Windtrap.Private.Test_tree
module Tag = Test_tree.Tag

let strf = Printf.sprintf

exception Boom
exception Teardown_boom

(* What a flattened test holds *)

let known_tags = [ "a"; "b"; "c"; "db"; "eio"; "g"; "net"; "slow"; "tbl"; "x" ]

let tag_names tags =
  let names = List.filter (fun name -> Tag.mem name tags) known_tags in
  strf "[%s]" (String.concat ", " names)

let limit = function None -> "no limit" | Some s -> strf "limit %gs" s

let site = function
  | None -> "no site"
  | Some (loc : Loc.t) -> strf "%s:%d" loc.file loc.line

let xfail_mark = function
  | None -> "no xfail"
  | Some { Test_tree.reason = None } -> "xfail"
  | Some { reason = Some reason } -> "xfail " ^ reason

let settings (c : Test_tree.case) =
  strf "tags %s, %s, %d retries" (tag_names c.tags) (limit c.timeout) c.retries

let marks (c : Test_tree.case) =
  strf "%s, %s"
    (if c.focused then "focused" else "not focused")
    (xfail_mark c.xfail)

(* [column f tests] is a row per test of [tests]: its path, then [f] of it. *)
let column f tests =
  let row (c : Test_tree.case) = String.concat "/" c.path ^ ": " ^ f c in
  List.map row (Test_tree.flatten tests)

let paths tests =
  List.map (fun (c : Test_tree.case) -> c.path) (Test_tree.flatten tests)

let single tree =
  require_match
    (function [ c ] -> Some c | _ -> None)
    (Test_tree.flatten [ tree ])

let focus_sites tests = List.map site (Test_tree.focus_sites tests)
let here line = strf "%s:%d" __FILE__ line

(* Tags *)

type call = Require of string | Drop of string

let predicate calls =
  let apply p = function
    | Require name -> Tag.require name p
    | Drop name -> Tag.drop name p
  in
  List.fold_left apply Tag.any calls

(* Through a test that declares the names, since the set constructors of [Tag]
   have no client outside the tests. *)
let tag_set names = (single (Test_tree.test ~tags:names "t" ignore)).tags

let selection_name (calls, tags) =
  let call = function Require n -> "require " ^ n | Drop n -> "drop " ^ n in
  let calls =
    match calls with
    | [] -> "any"
    | calls -> String.concat " then " (List.map call calls)
  in
  strf "%s, tags [%s]" calls (String.concat ", " tags)

let accepts_row (selection, accepted) =
  let calls, tags = selection in
  equal bool accepted (Tag.accepts (predicate calls) (tag_set tags))

let memberships =
  [
    (("db", [ "db"; "net" ]), true);
    (("net", [ "db"; "net" ]), true);
    (("x", [ "db"; "net" ]), false);
    (("db", []), false);
  ]

let selections =
  [
    (([], []), true);
    (([], [ "slow" ]), true);
    (([], [ "a"; "b" ]), true);
    (([ Require "net" ], []), false);
    (([ Require "net" ], [ "net" ]), true);
    (([ Require "net" ], [ "net"; "x" ]), true);
    (([ Require "a"; Require "b" ], [ "a" ]), false);
    (([ Require "a"; Require "b" ], [ "a"; "b" ]), true);
    (([ Drop "slow" ], [ "slow" ]), false);
    (([ Drop "slow" ], [ "net" ]), true);
    (([ Drop "slow" ], [ "net"; "slow" ]), false);
    (([ Require "a"; Drop "b" ], [ "a" ]), true);
    (([ Require "a"; Drop "b" ], [ "a"; "b" ]), false);
  ]

let later_calls =
  [
    (([ Require "x"; Drop "x" ], [ "x" ]), false);
    (([ Require "x"; Drop "x" ], []), true);
    (([ Drop "x"; Require "x" ], [ "x" ]), true);
    (([ Drop "x"; Require "x" ], []), false);
  ]

(* A tag passes when the last call that names it agrees with the set, and a
   tag that no call names always passes. *)
let passes calls tags name =
  let names = function Require n | Drop n -> String.equal n name in
  match List.find_opt names (List.rev calls) with
  | None -> true
  | Some (Require _) -> List.mem name tags
  | Some (Drop _) -> not (List.mem name tags)

let selection =
  let name = Gen.of_list [ "a"; "b"; "c" ] in
  let call =
    Gen.map
      (fun (r, n) -> if r then Require n else Drop n)
      (Gen.pair Gen.bool name)
  in
  let calls = Gen.list ~size:(Gen.int_range 0 6) call in
  let tags = Gen.list ~size:(Gen.int_range 0 3) name in
  Gen.with_pp
    (fun ppf s -> Format.pp_print_string ppf (selection_name s))
    (Gen.pair calls tags)

let last_call_decides (calls, tags) =
  let expected = List.for_all (passes calls tags) [ "a"; "b"; "c" ] in
  equal bool expected (Tag.accepts (predicate calls) (tag_set tags))

let tags =
  group "Tags"
    [
      test "slow is \"slow\"" (fun () -> equal string "slow" Tag.slow);
      test "prop is \"prop\"" (fun () -> equal string "prop" Tag.prop);
      cases "mem is true iff the name is in the set"
        ~name:(fun ((name, tags), _) ->
          strf "%s in [%s]" name (String.concat ", " tags))
        memberships
        (fun ((name, tags), mem) ->
          equal bool mem (Tag.mem name (tag_set tags)));
      cases
        "accepts is true iff the set holds every required tag and no dropped \
         one"
        ~name:(fun (s, _) -> selection_name s)
        selections accepts_row;
      cases "for a tag given to require and to drop, the later call decides"
        ~name:(fun (s, _) -> selection_name s)
        later_calls accepts_row;
      prop "accepts agrees with the last call that names each tag" selection
        last_call_decides;
    ]

(* Trees *)

let names =
  [
    ("the empty string", "");
    ("the separator", "a › b");
    ("a line feed", "line\nbreak");
    ("an escape sequence", "\x1b[31mred");
  ]

let named name =
  let tests =
    [
      Test_tree.test name ignore;
      Test_tree.group name [ Test_tree.test name ignore ];
    ]
  in
  equal (list (list string)) [ [ name ]; [ name; name ] ] (paths tests)

let trees =
  group "Trees"
    [
      cases "a name is any string, kept as given" ~name:fst names (fun (_, n) ->
          named n);
    ]

(* Declaring tests *)

type constructor = Test | Slow | Group | Cases | Scoped | Bracket

let constructors = [ Test; Slow; Group; Cases; Scoped; Bracket ]

let constructor_name = function
  | Test -> "test"
  | Slow -> "slow"
  | Group -> "group"
  | Cases -> "cases"
  | Scoped -> "scoped"
  | Bracket -> "bracket"

(* [declare c] is one test declared with the constructor [c]. *)
let declare c ?__POS__ ?tags ?timeout ?retries () =
  match c with
  | Test -> Test_tree.test ?__POS__ ?tags ?timeout ?retries "t" ignore
  | Slow -> Test_tree.slow ?__POS__ ?tags ?timeout ?retries "s" ignore
  | Group ->
      let t = Test_tree.test "t" ignore in
      Test_tree.group ?__POS__ ?tags ?timeout ?retries "g" [ t ]
  | Cases ->
      Test_tree.cases ?__POS__ ?tags ?timeout ?retries ~name:string_of_int "c"
        [ 0 ] ignore
  | Scoped ->
      let scope k = k () in
      Test_tree.scoped scope ?__POS__ ?tags ?timeout ?retries "s" ignore
  | Bracket ->
      Test_tree.bracket ?__POS__ ?tags ?timeout ?retries ~setup:ignore
        ~teardown:ignore "b" ignore

let each values =
  List.concat_map (fun c -> List.map (fun v -> (c, v)) values) constructors

let bad_timeouts =
  each [ 0.; -0.; -1.; Float.nan; Float.infinity; Float.neg_infinity ]

let bad_retries = each [ -1; min_int ]
let good_timeouts = each [ Float.succ 0.; Float.max_float ]

let accepted (c, timeout) =
  let case = single (declare c ~timeout ~retries:0 ()) in
  equal
    (pair (option float_exact) int)
    (Some timeout, 0)
    (case.timeout, case.retries)

(* The tags hold slow, which the slow constructor adds, so every row reads
   alike. *)
let own_settings c =
  let own = declare c ~tags:[ "slow"; "x" ] ~timeout:1.5 ~retries:2 () in
  let tree =
    Test_tree.group ~tags:[ "g" ] ~timeout:9. ~retries:9 "outer" [ own ]
  in
  equal string "tags [g, slow, x], limit 1.5s, 2 retries"
    (settings (single tree))

let declared_site c =
  let tree = declare c ~__POS__:("f.ml", 7, 2, 9) () in
  equal (list string) [ "f.ml:7" ] (focus_sites [ Test_tree.focus tree ])

let inert () =
  let calls = ref [] in
  let note name () = calls := name :: !calls in
  let tests =
    [
      Test_tree.test "test" (note "test body");
      Test_tree.slow "slow" (note "slow body");
      Test_tree.cases ~name:string_of_int "cases" [ 0 ] (fun _ -> note "row" ());
      Test_tree.scoped
        (fun k ->
          note "scope" ();
          k ())
        "scoped" (note "body");
      Test_tree.bracket ~setup:(note "setup") ~teardown:(note "teardown")
        "bracket" (note "bracket body");
    ]
  in
  equal (list string) [] !calls;
  ignore (Test_tree.flatten [ Test_tree.group "g" tests ] : Test_tree.case list);
  equal (list string) [] !calls

let body_runs () =
  let ran = ref 0 in
  let case = single (Test_tree.test "t" (fun () -> incr ran)) in
  let body =
    require_match
      (function Test_tree.Body fn -> Some fn | Scoped _ -> None)
      case.body
  in
  body ();
  equal int 1 !ran

let defaults () =
  let c = single (Test_tree.test "t" ignore) in
  equal string "tags [], no limit, 0 retries" (settings c);
  equal string "not focused, no xfail" (marks c)

(* Each declares its tree on the line it returns, the line that a capture of
   the call stack gives. *)
let test_here () = (__LINE__, Test_tree.test "t" ignore)
let slow_here () = (__LINE__, Test_tree.slow ~tags:[ "s" ] "s" ignore)

let cases_here () =
  (__LINE__, Test_tree.cases ~name:string_of_int "c" [ 0; 1 ] ignore)

let nested_here () =
  (__LINE__, Test_tree.group ~tags:[ "g" ] "g" [ Test_tree.test "t" ignore ])

let scoped_here () =
  (__LINE__, Test_tree.scoped (fun k -> k ()) ~tags:[ "s" ] "s" ignore)

let bracket_here () =
  (__LINE__, Test_tree.bracket ~setup:ignore ~teardown:ignore "b" ignore)

let partial_here () =
  let with_unit = Test_tree.bracket ~setup:ignore ~teardown:ignore in
  (__LINE__, with_unit ~timeout:1. "b" ignore)

let group_here () = (__LINE__, Test_tree.group ~retries:1 "g" [])

let captured =
  [
    ("test", (test_here, [ "t" ]));
    ("slow", (slow_here, [ "s" ]));
    ("cases", (cases_here, [ "c/0"; "c/1" ]));
    ("a test in a group", (nested_here, [ "g/t" ]));
    ("scoped", (scoped_here, [ "s" ]));
    ("bracket", (bracket_here, [ "b" ]));
  ]

(* The capture is best-effort: flambda can attribute the call to the line of
   the partial application, the line before the one [partial_here] returns. *)
let partial_site () =
  let line, tree = partial_here () in
  let either = [ "b: " ^ here (line - 1); "b: " ^ here line ] in
  satisfies ~claim:"the line of the partial or of the full application"
    (list string)
    (function [ site ] -> List.mem site either | _ -> false)
    (column (fun c -> site c.loc) [ tree ])

let captured_site (_, (declare, paths)) =
  let line, tree = declare () in
  let row path = path ^ ": " ^ here line in
  equal (list string) (List.map row paths)
    (column (fun c -> site c.loc) [ tree ])

let group_site () =
  let line, tree = group_here () in
  equal (list string) [ here line ] (focus_sites [ Test_tree.focus tree ])

let given_sites () =
  let t = Test_tree.test ~__POS__:("src/elsewhere.ml", 12, 0, 8) "t" ignore in
  let g = Test_tree.group ~__POS__:("src/group.ml", 1, 0, 0) "g" [ t ] in
  equal string "src/elsewhere.ml:12" (site (single t).loc);
  equal string "src/elsewhere.ml:12" (site (single g).loc)

(* The delimiter stops the walk, and the tail call leaves no frame of this
   file above it. *)
let unknown_site () = Loc.delimit (fun () -> Test_tree.test "t" ignore)

let no_site () =
  let t = unknown_site () in
  equal string "no site" (site (single t).loc);
  equal (list string) [ "no site" ] (focus_sites [ Test_tree.focus t ])

let slow_tags = [ ("no tags", ([], "[slow]")); ("x", ([ "x" ], "[slow, x]")) ]

let row_bodies () =
  let seen = ref [] in
  let tree =
    Test_tree.cases ~name:string_of_int "double" [ 1; 2; 3 ] (fun n ->
        seen := n :: !seen)
  in
  let body (c : Test_tree.case) =
    require_match
      (function Test_tree.Body fn -> Some fn | Scoped _ -> None)
      c.body
  in
  equal
    (list (list string))
    [ [ "double"; "1" ]; [ "double"; "2" ]; [ "double"; "3" ] ]
    (paths [ tree ]);
  List.iter (fun c -> body c ()) (Test_tree.flatten [ tree ]);
  equal (list int) [ 1; 2; 3 ] (List.rev !seen)

let row_options () =
  let tree =
    Test_tree.cases ~__POS__:("f.ml", 5, 0, 0) ~tags:[ "tbl" ] ~timeout:2.5
      ~retries:3 ~name:string_of_int "c" [ 0; 1 ] ignore
  in
  equal (list string)
    [
      "c/0: tags [tbl], limit 2.5s, 3 retries";
      "c/1: tags [tbl], limit 2.5s, 3 retries";
    ]
    (column settings [ tree ]);
  equal (list string)
    [ "c/0: f.ml:5"; "c/1: f.ml:5" ]
    (column (fun c -> site c.loc) [ tree ])

let names_once () =
  let named = ref [] in
  let name i =
    named := i :: !named;
    string_of_int i
  in
  let tree = Test_tree.cases ~name "c" [ 3; 1; 2 ] ignore in
  equal (list int) [ 3; 1; 2 ] (List.rev !named);
  ignore (Test_tree.flatten [ tree ] : Test_tree.case list);
  equal int 3 (List.length !named)

let scope_of tree =
  require_match
    (function
      | [ { Test_tree.body = Scoped { scope; body }; _ } ] ->
          Some (fun () -> scope body)
      | _ -> None)
    (Test_tree.flatten [ tree ])

let scope_calls_body () =
  let calls = ref [] in
  let note s = calls := s :: !calls in
  let scope k =
    note "acquire";
    k 42;
    note "release"
  in
  let tree = Test_tree.scoped scope "s" (fun r -> note (strf "body %d" r)) in
  scope_of tree ();
  equal (list string) [ "acquire"; "body 42"; "release" ] (List.rev !calls)

(* The partial application would not take the optional arguments if [scope]
   came after them, and the suite would not compile. *)
let partial_scoped () =
  let with_unit = Test_tree.scoped (fun k -> k ()) in
  let c =
    single
      (with_unit ~__POS__:("f.ml", 3, 0, 0) ~tags:[ "eio" ] ~timeout:3. "s"
         ignore)
  in
  equal string "tags [eio], limit 3s, 0 retries" (settings c);
  equal string "f.ml:3" (site c.loc)

let escaped = function
  | Boom -> "Boom"
  | Teardown_boom -> "Teardown_boom"
  | Failure.Check_failure _ -> "a check failure"
  | Failure.Control (`Skip _) -> "a skip"
  | Failure.Control (`Timeout _) -> "a timeout"
  | e -> Printexc.to_string e

type phases = {
  setup : unit -> int;
  body : unit -> unit;
  teardown : unit -> unit;
}

let phases = { setup = (fun () -> 42); body = ignore; teardown = ignore }
let raising e () = raise e

(* The scope that [bracket] derives, called with its body as the runner calls
   it: the phases that ran, then how the scope ended. *)
let bracket_trace p =
  let calls = ref [] in
  let note s = calls := s :: !calls in
  let setup () =
    note "setup";
    p.setup ()
  in
  let teardown r =
    note (strf "teardown %d" r);
    p.teardown ()
  in
  let body r =
    note (strf "body %d" r);
    p.body ()
  in
  let tree = Test_tree.bracket ~setup ~teardown "b" body in
  let ending =
    match scope_of tree () with
    | () -> "returns"
    | exception e -> "raises " ^ escaped e
  in
  String.concat ", " (List.rev (ending :: !calls))

let outcomes =
  [
    ("a body that returns", (phases, "setup, body 42, teardown 42, returns"));
    ( "a body that raises",
      ( { phases with body = raising Boom },
        "setup, body 42, teardown 42, raises Boom" ) );
    ( "a body that fails a check",
      ( {
          phases with
          body = raising (Failure.Check_failure (Failure.message "no"));
        },
        "setup, body 42, teardown 42, raises a check failure" ) );
    ( "a body that skips",
      ( { phases with body = raising (Failure.Control (`Skip None)) },
        "setup, body 42, teardown 42, raises a skip" ) );
    ( "a body that times out",
      ( { phases with body = raising (Failure.Control (`Timeout 1.)) },
        "setup, body 42, teardown 42, raises a timeout" ) );
    ( "a body that overflows the stack",
      ( { phases with body = raising Stack_overflow },
        "setup, body 42, teardown 42, raises Stack overflow" ) );
    ( "a body out of memory",
      ( { phases with body = raising Out_of_memory },
        "setup, body 42, raises Out of memory" ) );
    ( "a body interrupted",
      ( { phases with body = raising Sys.Break },
        "setup, body 42, raises Stdlib.Sys.Break" ) );
    ( "a setup that raises",
      ({ phases with setup = raising Boom }, "setup, raises Boom") );
    ( "a teardown that raises after the body returned",
      ( { phases with teardown = raising Teardown_boom },
        "setup, body 42, teardown 42, raises Teardown_boom" ) );
    ( "a teardown that raises after the body raised",
      ( { phases with body = raising Boom; teardown = raising Teardown_boom },
        "setup, body 42, teardown 42, raises Teardown_boom" ) );
  ]

(* Never inlined, so that a backtrace names their frames. *)
let[@inline never] fail_in_body () = raise Boom

let[@inline never] catch_in_teardown () =
  try raise Not_found with Not_found -> ()

(* A teardown that raises and catches would overwrite the backtrace of a plain
   re-raise. *)
let body_backtrace () =
  let tree =
    Test_tree.bracket ~setup:ignore ~teardown:catch_in_teardown "b" fail_in_body
  in
  let raised =
    match scope_of tree () with
    | () -> None
    | exception Boom -> Some (Printexc.get_backtrace ())
  in
  let backtrace = require_some raised in
  contains ~sub:"fail_in_body" backtrace;
  not_contains ~sub:"catch_in_teardown" backtrace

let declaring =
  group "Declaring tests"
    [
      test "declaring and flattening call no body, scope, setup or teardown"
        inert;
      test "the body of a test runs when its case's body is called" body_runs;
      test "a test declares no tag, no limit, no retries, no focus and no xfail"
        defaults;
      cases
        "every constructor refuses a timeout that is not finite and positive"
        ~name:(fun (c, v) -> strf "%s, %g" (constructor_name c) v)
        bad_timeouts
        (fun (c, timeout) ->
          raises_match Exn.invalid_arg (fun () -> declare c ~timeout ()));
      cases "every constructor refuses negative retries"
        ~name:(fun (c, n) -> strf "%s, %d" (constructor_name c) n)
        bad_retries
        (fun (c, retries) ->
          raises_match Exn.invalid_arg (fun () -> declare c ~retries ()));
      cases "every constructor accepts 0 retries and a finite positive timeout"
        ~name:(fun (c, v) -> strf "%s, %g" (constructor_name c) v)
        good_timeouts accepted;
      cases
        "every constructor's own tags, timeout and retries reach its test over \
         those of a group"
        ~name:constructor_name constructors own_settings;
      cases "every constructor's __POS__ is its site" ~name:constructor_name
        constructors declared_site;
      cases "without __POS__, a site is the line that applies the constructor"
        ~name:fst captured captured_site;
      test
        "without __POS__, a partial application's site is the line of either \
         application"
        partial_site;
      test "without __POS__, a group's site is the line that applies it"
        group_site;
      test "a test keeps its own site, not its group's" given_sites;
      test "a node declared where no site is known has none" no_site;
      cases "slow adds the slow tag to the declared ones" ~name:fst slow_tags
        (fun (_, (declared, expected)) ->
          let c = single (Test_tree.slow ~tags:declared "s" ignore) in
          equal string expected (tag_names c.tags));
      test "cases names each test by its input and calls fn with it" row_bodies;
      test
        "every test of cases has the site of the call and its tags, timeout \
         and retries"
        row_options;
      test
        "cases applies name to every input in order, once, when it is applied"
        names_once;
      test "what name raises escapes when cases is applied" (fun () ->
          raises Boom (fun () ->
              Test_tree.cases ~name:(fun _ -> raise Boom) "c" [ 0 ] ignore));
      test "cases over no inputs declares no test" (fun () ->
          equal
            (list (list string))
            []
            (paths [ Test_tree.cases ~name:string_of_int "empty" [] ignore ]));
      test "scoped stores a scope that calls the body with its resource"
        scope_calls_body;
      test "a partial application of scoped still takes the optional arguments"
        partial_scoped;
      cases
        "bracket runs teardown iff setup returned, on every outcome of the body"
        ~name:fst outcomes (fun (_, (p, trace)) ->
          equal string trace (bracket_trace p));
      test "bracket raises the body's exception again with its backtrace"
        body_backtrace;
    ]

(* Annotations *)

let[@inline never] annotate t = Test_tree.xfail (Test_tree.focus t)

let annotated_sites () =
  let pos = ("src/declared.ml", 12, 0, 8) in
  let t = Test_tree.test ~__POS__:pos ~tags:[ "x" ] ~timeout:1. "t" ignore in
  equal string "src/declared.ml:12" (site (single (annotate t)).loc);
  let g = Test_tree.group ~__POS__:pos "g" [] in
  equal (list string) [ "src/declared.ml:12" ] (focus_sites [ annotate g ]);
  let line, t = test_here () in
  equal string (here line) (site (single (annotate t)).loc)

let listed_sites () =
  let p n = ("f.ml", n, 0, 0) in
  let tests =
    [
      Test_tree.focus
        (Test_tree.group ~__POS__:(p 1) "g"
           [
             Test_tree.test "a" ignore;
             Test_tree.focus (Test_tree.test ~__POS__:(p 2) "b" ignore);
             Test_tree.test "c" ignore;
           ]);
      Test_tree.test "plain" ignore;
      Test_tree.group "h"
        [
          Test_tree.group "i"
            [ Test_tree.focus (Test_tree.test ~__POS__:(p 3) "d" ignore) ];
        ];
      Test_tree.focus (Test_tree.group ~__POS__:(p 4) "empty" []);
    ]
  in
  equal (list string)
    [ "f.ml:1"; "f.ml:2"; "f.ml:3"; "f.ml:4" ]
    (focus_sites tests)

let focus_reach () =
  let tests =
    [
      Test_tree.focus
        (Test_tree.group "g"
           [ Test_tree.group "h" [ Test_tree.test "a" ignore ] ]);
      Test_tree.group "i"
        [
          Test_tree.focus (Test_tree.test "b" ignore); Test_tree.test "c" ignore;
        ];
    ]
  in
  equal (list string)
    [ "g/h/a: true"; "i/b: true"; "i/c: false" ]
    (column (fun c -> string_of_bool c.focused) tests)

let reasons =
  [
    ("with a reason", (Some "issue #42", "xfail issue #42"));
    ("without a reason", (None, "xfail"));
  ]

let nearest_xfail () =
  let tests =
    [
      Test_tree.xfail ~reason:"outer"
        (Test_tree.group "g"
           [
             Test_tree.xfail ~reason:"inner" (Test_tree.test "refined" ignore);
             Test_tree.test "plain" ignore;
           ]);
      Test_tree.test "free" ignore;
    ]
  in
  equal (list string)
    [ "g/refined: xfail inner"; "g/plain: xfail outer"; "free: no xfail" ]
    (column (fun c -> xfail_mark c.xfail) tests)

let composed () =
  let t () = Test_tree.test ~tags:[ "db" ] ~timeout:1.5 ~retries:2 "t" ignore in
  let row tree =
    let c = single tree in
    settings c ^ ", " ^ marks c
  in
  let expected = "tags [db], limit 1.5s, 2 retries, focused, xfail r" in
  equal string expected
    (row (Test_tree.xfail ~reason:"r" (Test_tree.focus (t ()))));
  equal string expected
    (row (Test_tree.focus (Test_tree.xfail ~reason:"r" (t ()))))

let annotations =
  group "Annotations"
    [
      test "an annotation leaves the site of its node alone" annotated_sites;
      cases "focus marks the tests of every constructor" ~name:constructor_name
        constructors (fun c ->
          let focused = Test_tree.flatten [ Test_tree.focus (declare c ()) ] in
          equal (list bool) [ true ]
            (List.map (fun (c : Test_tree.case) -> c.focused) focused));
      test "a focus on a group reaches every test under it and no sibling"
        focus_reach;
      cases "xfail marks a test with its reason, or with none" ~name:fst reasons
        (fun (_, (reason, mark)) ->
          let c =
            single (Test_tree.xfail ?reason (Test_tree.test "t" ignore))
          in
          equal string mark (xfail_mark c.xfail));
      test "an xfail on a group reaches every test under it" (fun () ->
          let g =
            Test_tree.group "g"
              [ Test_tree.test "a" ignore; Test_tree.test "b" ignore ]
          in
          equal (list string)
            [ "g/a: xfail bug"; "g/b: xfail bug" ]
            (column
               (fun c -> xfail_mark c.xfail)
               [ Test_tree.xfail ~reason:"bug" g ]));
      test "the xfail nearest a test is the one its case carries" nearest_xfail;
      test "on one node the first xfail applied stays" (fun () ->
          let t = Test_tree.xfail ~reason:"a" (Test_tree.test "t" ignore) in
          equal string "xfail a"
            (xfail_mark (single (Test_tree.xfail ~reason:"b" t)).xfail));
      test "focus and xfail compose in either order and keep the settings"
        composed;
      test "focus_sites is empty when no node is focused" (fun () ->
          equal (list string) []
            (focus_sites
               [
                 Test_tree.test "t" ignore;
                 Test_tree.group "g" [ Test_tree.test "t" ignore ];
               ]));
      test
        "focus_sites lists each focused node once, in declaration order, a \
         group before what it holds"
        listed_sites;
    ]

(* Flattening *)

let depth_first () =
  let tests =
    [
      Test_tree.test "alpha" ignore;
      Test_tree.group "outer"
        [
          Test_tree.test "beta" ignore;
          Test_tree.group "inner" [ Test_tree.test "gamma" ignore ];
        ];
      Test_tree.test "delta" ignore;
    ]
  in
  equal
    (list (list string))
    [
      [ "alpha" ];
      [ "outer"; "beta" ];
      [ "outer"; "inner"; "gamma" ];
      [ "delta" ];
    ]
    (paths tests)

let group_defaults () =
  let tests =
    [
      Test_tree.group ~timeout:2. ~retries:2 "g"
        [
          Test_tree.test "a" ignore;
          Test_tree.group "h" [ Test_tree.test "b" ignore ];
        ];
    ]
  in
  equal (list string)
    [
      "g/a: tags [], limit 2s, 2 retries"; "g/h/b: tags [], limit 2s, 2 retries";
    ]
    (column settings tests)

let nearest_wins () =
  let tests =
    [
      Test_tree.group ~timeout:5. ~retries:5 "g"
        [
          Test_tree.test ~timeout:1. ~retries:1 "own" ignore;
          Test_tree.test "inherits" ignore;
          Test_tree.group ~timeout:2. "h" [ Test_tree.test "inner" ignore ];
        ];
    ]
  in
  equal (list string)
    [
      "g/own: tags [], limit 1s, 1 retries";
      "g/inherits: tags [], limit 5s, 5 retries";
      "g/h/inner: tags [], limit 2s, 5 retries";
    ]
    (column settings tests)

let inherited_tags () =
  let tests =
    [
      Test_tree.group ~tags:[ "db" ] "g"
        [
          Test_tree.test ~tags:[ "net" ] "a" ignore;
          Test_tree.group ~tags:[ "x" ] "h" [ Test_tree.test "b" ignore ];
          Test_tree.test "c" ignore;
        ];
      Test_tree.test "d" ignore;
    ]
  in
  equal (list string)
    [ "g/a: [db, net]"; "g/h/b: [db, x]"; "g/c: [db]"; "d: []" ]
    (column (fun c -> tag_names c.tags) tests)

let joined =
  [
    ("one component", ([ "alpha" ], "alpha"));
    ( "two components",
      ([ "users"; "sessions after login" ], "users › sessions after login") );
    ( "three components, by their bytes",
      ([ "a"; "b"; "c" ], "a \xe2\x80\xba b \xe2\x80\xba c") );
  ]

let flattening =
  group "Flattening"
    [
      test
        "flatten lists the tests depth first in declaration order, each path \
         its groups' names outermost first, then its own"
        depth_first;
      test "a group without tests adds no test" (fun () ->
          equal
            (list (list string))
            []
            (paths
               [
                 Test_tree.group "g" [];
                 Test_tree.group "h" [ Test_tree.group "i" [] ];
               ]));
      test "flatten keeps two tests of one path, in order" (fun () ->
          let t = Test_tree.test "same" ignore in
          equal
            (list (list string))
            [ [ "same" ]; [ "same" ] ]
            (paths [ t; Test_tree.group "g" []; t ]));
      test
        "a group's timeout and retries are the defaults of every test under it"
        group_defaults;
      test "the timeout and retries declared nearest a test win" nearest_wins;
      test "a test's tags are its own and those of its ancestors" inherited_tags;
      cases
        "path_to_string joins the components with a space, U+203A and a space"
        ~name:fst joined (fun (_, (path, s)) ->
          equal string s (Test_tree.path_to_string path));
    ]

let () =
  exit (run "test_tree" [ tags; trees; declaring; annotations; flattening ])
