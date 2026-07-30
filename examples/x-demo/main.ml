(* The visual validation playground: every windtrap output surface in one
   deliberately failing run. Groups are ordered as a guided tour — compact
   machinery, one exemplary failure per assertion verb, properties,
   snapshots, then runtime surfaces. See README.md for what to look for.

   This suite FAILS BY DESIGN (exit 1). Do not wire it into runtest. *)

open Windtrap

(* helpers *)

(* Busy wait without depending on unix: [Sys.time] is processor time,
   close enough to wall time for a spinning loop, and the loop body hits
   safepoints so the runner's SIGALRM timeout can interrupt it. *)
let busy_wait seconds =
  let start = Sys.time () in
  while Sys.time () -. start < seconds do
    ignore (Sys.opaque_identity 0)
  done

(* The -v slowest-tests list renders only when a run's summed durations
   reach five seconds; X_DEMO_SLEEP_SCALE=2 stretches the two slow tests
   past that bar without taxing the default tour. *)
let sleep_scale =
  match Sys.getenv_opt "X_DEMO_SLEEP_SCALE" with
  | None -> 1.0
  | Some s -> ( match float_of_string_opt s with Some f -> f | None -> 1.0)

let paragraph =
  "He who controls the spice controls the universe. The mystery of life is not \
   a problem to solve, but a reality to experience. Fear is the mind-killer."

type point = { x : int; y : int }

let pp_point ppf p = Format.fprintf ppf "(%d, %d)" p.x p.y
let point : point testable = Testable.make ~pp:pp_point ~equal:( = )

module Version = struct
  type t = int * int

  let pp ppf (major, minor) = Format.fprintf ppf "%d.%d" major minor
  let equal = ( = )
end

let version = Testable.of_module (module Version)

type user = { id : int; name : string }

let by_id = contramap (fun u -> u.id) int

type addr = Tcp of int | Unix_socket of string

let pp_addr ppf = function
  | Tcp p -> Format.fprintf ppf "Tcp %d" p
  | Unix_socket s -> Format.fprintf ppf "Unix_socket %S" s

let expected_grid = Array.init 300 (fun i -> i * i)

(* Same-width corruptions near the end: the unified diff stays a tight
   hunk while the element-grain summary line does the diagnosis. *)
let actual_grid =
  let a = Array.init 300 (fun i -> i * i) in
  a.(290) <- 84101;
  a.(293) <- 85850;
  a.(297) <- 88210;
  a

let usage_text =
  String.concat "\n"
    [
      "usage: mytool [OPTION]... FILE...";
      "";
      "  -o DIR      write output under DIR";
      "  -j JOBS     run JOBS rewrites in parallel";
      "  --trace     log every rewrite to stderr";
    ]

let release_notes =
  String.concat "\n"
    [
      "mytool 0.2.0";
      "";
      "- rewrite engine now runs in parallel";
      "- new --trace flag for rewrite logging";
      "- dropped OCaml 4 support";
    ]

(* Run-scoped shared resource: acquired on first use inside a test,
   released by the runner after the last test (the "releasing" note). *)
let server_port = fixture ~teardown:(fun (_ : int) -> ()) (fun () -> 43117)

(* 1. compact machinery

   41 table cases + 19 named passes fill the first 60-glyph row exactly,
   so the [60/100] counter renders; then S (skip), x (expected failure),
   and the first loud failure (an xfail that PASSED) land mid-row-2 —
   the buffered header and row visibly flush at that point. *)

let compact_tests =
  [
    cases
      ~name:(fun (a, b) -> Printf.sprintf "%d + %d" a b)
      "addition table"
      (List.init 41 (fun i -> (i, 41 - i)))
      (fun (a, b) -> equal int 41 (a + b));
    test "string and char witnesses" (fun () ->
        equal string "spice" "spice";
        equal char 'w' 'w');
    test "unit, bool, and bytes witnesses" (fun () ->
        equal unit () ();
        equal bool true true;
        equal bytes (Bytes.of_string "raw") (Bytes.of_string "raw"));
    test "fixed-width integer witnesses" (fun () ->
        equal int32 7l 7l;
        equal int64 7L 7L;
        equal nativeint 7n 7n);
    test "float with absolute tolerance" (fun () ->
        equal (float 1e-9) 0.1 (0.05 +. 0.05));
    test "float_rel with combined tolerance" (fun () ->
        equal (float_rel ~rel:1e-6 ~abs:1e-9) 1e9 (1e9 +. 1.));
    test "float_exact treats NaN as equal to NaN" (fun () ->
        equal float_exact nan (0. /. 0.));
    test "option and result witnesses" (fun () ->
        equal (option int) (Some 3) (Some 3);
        equal (result int string) (Error "nope") (Error "nope"));
    test "either witness" (fun () ->
        equal (either int string) (Either.Left 1) (Either.Left 1));
    test "slist compares as a multiset" (fun () ->
        equal (slist int compare) [ 3; 1; 2 ] [ 2; 3; 1 ]);
    test "pair, triple, and quad witnesses" (fun () ->
        equal (pair string int) ("a", 1) ("a", 1);
        equal (triple string int bool) ("a", 1, true) ("a", 1, true);
        equal (quad string int bool char) ("a", 1, true, 'z') ("a", 1, true, 'z'));
    test "pass ignores a component" (fun () ->
        equal (pair string pass) ("key", 1) ("key", 999));
    test "of_equal compares without a printer" (fun () ->
        equal (of_equal (fun a b -> a mod 3 = b mod 3)) 4 7);
    test "contramap compares users by id" (fun () ->
        equal by_id { id = 1; name = "alice" } { id = 1; name = "al" });
    test "custom witnesses: Testable.make and of_module" (fun () ->
        equal point { x = 1; y = 2 } { x = 1; y = 2 };
        equal version (0, 2) (0, 2));
    test "is_false" (fun () -> is_false (3 > 5));
    test "current_test knows its own path" (fun () ->
        equal (list string)
          [ "compact"; "current_test knows its own path" ]
          (current_test ()));
    test "srandom is stable within a test" (fun () ->
        let a = Random.State.int (srandom ()) 1_000_000 in
        let b = Random.State.int (srandom ()) 1_000_000 in
        equal int a b);
    test "temp_dir and temp_file are runner-cleaned" (fun () ->
        let dir = temp_dir () in
        is_true (Sys.is_directory dir);
        let file = temp_file ~suffix:".json" () in
        let oc = open_out file in
        output_string oc "{}";
        close_out oc;
        is_true (Sys.file_exists file));
    test "output () reads back captured stdout" (fun () ->
        print_string "hello, capture\n";
        equal string "hello, capture\n" (output ()));
    test "talks to a real postgres" (fun () ->
        skip ~reason:"no database in the demo environment" ());
    xfail ~reason:"issue #42"
      (test "unicode width of combining marks" (fun () ->
           equal int 1 (String.length "e\xcc\x81")));
    xfail ~reason:"issue #57"
      (test "already fixed upstream" (fun () -> equal int 4 (2 + 2)));
  ]

(* 2. every failure kind, one exemplary test each *)

let assertion_tests =
  [
    (* Intended failures sit in non-tail position (a follow-up statement
       after each) so the captured location is the assertion's own line —
       a tail-position assertion falls back to the test's declaration. *)
    test "equal on nested structure" (fun () ->
        let sessions =
          [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4; 5 ]); ("carol", []) ]
        in
        equal
          (list (pair string (list int)))
          [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
          sessions;
        equal int 3 (List.length sessions));
    test "equal on strings (char-level highlight)" (fun () ->
        let rendered = "the quick brawn fox jumped" in
        equal string "the quick brown fox jumps" rendered;
        equal int 26 (String.length rendered));
    test "equal on a large array (first-mismatch summary)" (fun () ->
        equal (array int) expected_grid actual_grid;
        equal int 300 (Array.length actual_grid));
    test "not_equal when both sides are equal" (fun () ->
        not_equal (list int) [ 1; 2; 3 ] [ 1; 2; 3 ];
        not_equal int 1 2);
    test "is_true hides the data" (fun () ->
        is_true (List.mem 42 [ 2; 3; 5 ]);
        is_false (List.mem 41 [ 2; 3; 5 ]));
    test "satisfies shows the data" (fun () ->
        satisfies ~msg:"positive" int (fun n -> n > 0) (-3);
        satisfies ~msg:"even" int (fun n -> n mod 2 = 0) 4);
    test "contains with a missing needle" (fun () ->
        contains ~sub:"tempest" paragraph;
        contains ~sub:"spice" paragraph);
    test "not_contains with a present needle" (fun () ->
        not_contains ~sub:"spice" paragraph;
        not_contains ~sub:"tempest" paragraph);
    test "require_some on None" (fun () ->
        let (_ : int) =
          require_some (List.assoc_opt "carol" [ ("alice", 1); ("bob", 2) ])
        in
        ());
    test "require_ok renders the error with ~pp_error" (fun () ->
        let (_ : int) =
          require_ok ~pp_error:Format.pp_print_string
            (Error "parse error at line 3: unexpected ']'")
        in
        ());
    test "require_error renders the ok with ~pp_ok" (fun () ->
        let (_ : string) = require_error ~pp_ok:Format.pp_print_int (Ok 42) in
        ());
    test "require_match renders the scrutinee with ~pp" (fun () ->
        let (_ : int) =
          require_match ~pp:pp_addr
            (function Tcp p -> Some p | Unix_socket _ -> None)
            (Unix_socket "/var/run/mytool.sock")
        in
        ());
    test "raises with the wrong message (message diff)" (fun () ->
        raises (Invalid_argument "index out of bounds") (fun () ->
            invalid_arg "negative length");
        raises Not_found (fun () -> List.assoc "x" []));
    test "raises with the wrong exception (both printed)" (fun () ->
        raises Not_found (fun () -> failwith "connection reset");
        raises Not_found (fun () -> List.assoc "x" []));
    test "raises when nothing was raised" (fun () ->
        raises (Invalid_argument "negative length") (fun () ->
            String.make 3 'x');
        equal string "xxx" (String.make 3 'x'));
    test "raises_match prints the raised exception" (fun () ->
        raises_match (Exn.invalid_arg ~substring:"unhandled op") (fun () ->
            failwith "stack underflow");
        raises_match (Exn.failure ~exact:"boom") (fun () -> failwith "boom"));
    test "raises_match when nothing was raised" (fun () ->
        raises_match (Exn.failure ~substring:"overflow") (fun () -> min_int - 1);
        equal int min_int (min_int - 1 + 1));
    test "an uncaught exception fails the test" (fun () ->
        (* Not an assertion: the body raises [Not_found] itself. The runner
           catches any exception at the test boundary; the location falls
           back to the test's declaration site, and backtrace lines render
           (faint) when the runtime records them (OCAMLRUNPARAM=b). *)
        let sessions = Hashtbl.create 8 in
        Hashtbl.replace sessions "alice" 1;
        equal int 1 (Hashtbl.find sessions "bob");
        equal int 1 (Hashtbl.length sessions));
    test "fail marks unreachable branches" (fun () ->
        let dispatch = [ ("push", 1); ("pop", 2) ] in
        (match List.assoc_opt "halt" dispatch with
        | Some code -> equal int 3 code
        | None -> fail "unreachable: no dispatch entry for \"halt\"");
        equal int 2 (List.length dispatch));
    test ~retries:1 "failf formats its message (retried once)" (fun () ->
        let workers = 7 in
        if workers <> 4 then
          failf "expected %d workers, launcher reported %d" 4 workers;
        equal int 4 workers);
  ]

(* 3. properties *)

let property_tests =
  [
    prop "concat preserves length" (Gen.pair Gen.string Gen.string)
      (fun (a, b) ->
        assume (String.length a <= 4096);
        if String.equal a "windtrap!" then reject ();
        equal int (String.length a + String.length b) (String.length (a ^ b)));
    prop "reverse is the identity (it is not)" (Gen.list Gen.int) (fun l ->
        let round = List.rev l in
        equal (list int) l round;
        equal int (List.length l) (List.length round));
    prop ~examples:[ 3; 8 ] "pinned examples run first" Gen.int (fun n ->
        is_true ~msg:"even" (n mod 2 = 0);
        equal int 0 (abs n mod 2));
    prop "assume filters too hard (gives up)" Gen.int (fun n ->
        (* Full-range ints essentially never land in [-2, 2]: the discard
           budget (2 x count) exhausts and the property gives up rather
           than silently passing on zero cases. *)
        assume (abs n < 3);
        equal int n n);
    prop "insert keeps lists sorted (under-covered)"
      (Gen.list ~size:(Gen.int_range 0 6) Gen.small_int)
      (fun l ->
        let sorted = List.sort compare l in
        classify "empty" (l = []);
        classify "nonempty" (l <> []);
        collect (if List.length l > 3 then "len > 3" else "len <= 3");
        cover ~label:"long (> 10 elements)" ~at_least:75.0 (List.length l > 10);
        equal (list int) sorted (List.sort compare sorted));
  ]

(* 4. snapshots *)

let snapshot_tests =
  [
    test "point renders stably (green snapshot_pp)" (fun () ->
        snapshot_pp "origin" pp_point { x = 0; y = 0 });
    test "usage text drifted from its baseline" (fun () ->
        snapshot "usage" usage_text;
        equal int 5 (List.length (String.split_on_char '\n' usage_text)));
    test "release notes have no baseline yet" (fun () ->
        snapshot "release-notes" release_notes;
        contains ~sub:"0.2.0" release_notes);
  ]

(* 5. runtime surfaces *)

let runtime_tests =
  [
    test "tokenizer trace precedes the failure" (fun () ->
        let tokens =
          [
            "INT 1";
            "PLUS";
            "INT 2";
            "STAR";
            "INT 3";
            "LPAREN";
            "INT 4";
            "RPAREN";
            "MINUS";
            "INT 5";
            "SLASH";
            "INT 6";
            "CARET";
            "EOF";
          ]
        in
        List.iter print_endline tokens;
        equal ~msg:"token count" int 13 (List.length tokens);
        equal string "EOF" (List.nth tokens 13));
    test "backend contract (sibling subtests keep running)" (fun () ->
        subtest "disk" (fun () -> equal string "ready" "degraded");
        subtest "s3" (fun () ->
            subtest "auth" (fun () -> fail "credentials expired 2026-07-01"));
        subtest "memory" (fun () -> equal int 0 0));
    bracket
      ~setup:(fun () -> ref [ "alice"; "bob" ])
      ~teardown:(fun db -> db := [])
      "counts rows in a bracketed db"
      (fun db -> equal int 2 (List.length !db));
    bracket
      ~setup:(fun () -> "stale")
      ~teardown:(fun conn ->
        if conn = "stale" then fail "teardown: connection already closed";
        ())
      "body and teardown failures report independently"
      (fun conn ->
        equal string "open" conn;
        equal int 5 (String.length conn));
    test "first hit acquires the shared server" (fun () ->
        equal int 43117 (server_port ()));
    test "second hit reuses the shared server" (fun () ->
        equal int 43117 (server_port ()));
    test ~timeout:0.2 "spins forever (times out at 0.2s)" (fun () ->
        busy_wait 5.0);
    test "reindex is untagged and slow (warns)" (fun () ->
        busy_wait (1.3 *. sleep_scale));
    slow "nightly compaction is tagged slow (silent)" (fun () ->
        busy_wait (1.15 *. sleep_scale));
  ]

let () =
  run "x-demo"
    [
      group "compact" compact_tests;
      group "assertions" assertion_tests;
      group "properties" property_tests;
      group "snapshots" snapshot_tests;
      group ~tags:[ "io" ] "runtime" runtime_tests;
    ]
