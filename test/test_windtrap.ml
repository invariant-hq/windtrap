(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap

let testable_tests =
  group "Testable"
    [
      group "int" [ test "equal values" (fun () -> equal Testable.int 42 42) ];
      group "float"
        [
          test "exact equality" (fun () -> equal (Testable.float 0.0) 1.5 1.5);
          test "epsilon comparison" (fun () ->
              equal (Testable.float 0.01) 1.0 1.005);
          test "NaN equals NaN" (fun () -> equal (Testable.float 0.0) nan nan);
        ];
      group "float_rel"
        [
          test "relative tolerance" (fun () ->
              equal (Testable.float_rel ~rel:0.01 ~abs:0.0) 100.0 100.5);
          test "absolute tolerance" (fun () ->
              equal (Testable.float_rel ~rel:0.0 ~abs:0.1) 0.0 0.05);
        ];
      group "string"
        [
          test "equal strings" (fun () -> equal Testable.string "hello" "hello");
        ];
      group "option"
        [
          test "Some equals Some" (fun () ->
              equal (Testable.option Testable.int) (Some 1) (Some 1));
          test "None equals None" (fun () ->
              equal (Testable.option Testable.int) None None);
          test "Some differs from None" (fun () ->
              not_equal (Testable.option Testable.int) (Some 1) None);
        ];
      group "list"
        [
          test "equal lists" (fun () ->
              equal (Testable.list Testable.int) [ 1; 2; 3 ] [ 1; 2; 3 ]);
          test "different lengths" (fun () ->
              not_equal (Testable.list Testable.int) [ 1; 2 ] [ 1; 2; 3 ]);
        ];
      group "pass"
        [
          test "always succeeds" (fun () ->
              equal Testable.pass () ();
              equal Testable.pass 1 2);
        ];
      group "slist"
        [
          test "ignores order" (fun () ->
              equal
                (Testable.slist Testable.int Int.compare)
                [ 3; 1; 2 ] [ 1; 2; 3 ]);
          test "detects missing elements" (fun () ->
              not_equal
                (Testable.slist Testable.int Int.compare)
                [ 1; 2 ] [ 1; 2; 3 ]);
          test "handles duplicates" (fun () ->
              equal
                (Testable.slist Testable.int Int.compare)
                [ 1; 1; 2 ] [ 1; 2; 1 ]);
        ];
      group "contramap"
        [
          test "transforms before comparing" (fun () ->
              let by_length = Testable.contramap String.length Testable.int in
              equal by_length "foo" "bar");
          test "detects differences after transform" (fun () ->
              let by_length = Testable.contramap String.length Testable.int in
              not_equal by_length "foo" "quux");
        ];
      group "of_equal"
        [
          test "uses custom equality" (fun () ->
              let case_insensitive =
                Testable.of_equal (fun a b ->
                    String.lowercase_ascii a = String.lowercase_ascii b)
              in
              equal case_insensitive "Hello" "HELLO");
        ];
      group "seq"
        [
          test "compares sequences" (fun () ->
              equal
                (Testable.seq Testable.int)
                (List.to_seq [ 1; 2; 3 ])
                (List.to_seq [ 1; 2; 3 ]));
          test "detects differences" (fun () ->
              not_equal
                (Testable.seq Testable.int)
                (List.to_seq [ 1; 2 ])
                (List.to_seq [ 1; 2; 3 ]));
        ];
      group "lazy_t"
        [
          test "forces and compares" (fun () ->
              equal (Testable.lazy_t Testable.int) (lazy 42) (lazy (40 + 2)));
        ];
      group "nativeint"
        [
          test "equal values" (fun () -> equal Testable.nativeint 42n 42n);
          test "different values" (fun () -> not_equal Testable.nativeint 0n 1n);
        ];
      group "either"
        [
          test "Left equals Left" (fun () ->
              equal
                (Testable.either Testable.int Testable.string)
                (Either.Left 1) (Either.Left 1));
          test "Right equals Right" (fun () ->
              equal
                (Testable.either Testable.int Testable.string)
                (Either.Right "hello") (Either.Right "hello"));
          test "Left differs from Right" (fun () ->
              not_equal
                (Testable.either Testable.int Testable.int)
                (Either.Left 1) (Either.Right 1));
        ];
      group "to_string"
        [
          test "converts int to string" (fun () ->
              equal Testable.string "42" (Testable.to_string Testable.int 42));
          test "converts list to string" (fun () ->
              equal Testable.string "[1; 2; 3]"
                (Testable.to_string Testable.(list int) [ 1; 2; 3 ]));
        ];
    ]

let check_tests =
  group "Check"
    [
      group "boolean"
        [
          test "is_true passes for true" (fun () -> is_true true);
          test "is_false passes for false" (fun () -> is_false false);
        ];
      group "option"
        [
          test "is_some passes for Some" (fun () -> is_some (Some 42));
          test "is_none passes for None" (fun () -> is_none None);
          test "some checks value" (fun () -> some Testable.int 42 (Some 42));
        ];
      group "result"
        [
          test "is_ok passes for Ok" (fun () -> is_ok (Ok 42));
          test "is_error passes for Error" (fun () -> is_error (Error "oops"));
          test "ok checks value" (fun () -> ok Testable.int 42 (Ok 42));
          test "error checks value" (fun () ->
              error Testable.string "oops" (Error "oops"));
        ];
      group "exceptions"
        [
          test "raises catches expected" (fun () ->
              raises (Failure "boom") (fun () -> failwith "boom"));
          test "raises_match with predicate" (fun () ->
              raises_match
                (function Failure s -> String.length s > 0 | _ -> false)
                (fun () -> failwith "error"));
          test "no_raise returns value" (fun () ->
              let result = no_raise (fun () -> 42) in
              equal Testable.int 42 result);
        ];
      group "custom messages"
        [
          test "equal with msg" (fun () ->
              equal ~msg:"custom equality message" Testable.int 42 42);
          test "is_some with msg" (fun () ->
              is_some ~msg:"custom is_some message" (Some 42));
          test "is_ok with msg" (fun () ->
              is_ok ~msg:"custom is_ok message" (Ok 42));
          test "raises with msg" (fun () ->
              raises ~msg:"custom raises message" (Failure "boom") (fun () ->
                  failwith "boom"));
        ];
    ]

let tag_tests =
  group "Tag"
    [
      group "basic"
        [
          test "empty has Quick speed" (fun () ->
              is_true (Tag.get_speed Tag.empty = Tag.Quick));
          test "speed Slow creates slow tag" (fun () ->
              let t = Tag.speed Tag.Slow in
              is_true (Tag.get_speed t = Tag.Slow));
          test "labels adds labels" (fun () ->
              let t = Tag.labels [ "a"; "b" ] in
              is_true (Tag.has_label "a" t);
              is_true (Tag.has_label "b" t);
              is_false (Tag.has_label "c" t));
        ];
      group "merge semantics"
        [
          test "merge combines labels" (fun () ->
              let t1 = Tag.labels [ "a" ] in
              let t2 = Tag.labels [ "b" ] in
              let merged = Tag.merge t1 t2 in
              is_true (Tag.has_label "a" merged);
              is_true (Tag.has_label "b" merged));
          test "explicit speed in second arg wins" (fun () ->
              let t1 = Tag.speed Tag.Quick in
              let t2 = Tag.speed Tag.Slow in
              let merged = Tag.merge t1 t2 in
              is_true (Tag.get_speed merged = Tag.Slow));
          test "labels don't reset speed" (fun () ->
              let t1 = Tag.speed Tag.Slow in
              let t2 = Tag.labels [ "integration" ] in
              let merged = Tag.merge t1 t2 in
              is_true (Tag.get_speed merged = Tag.Slow);
              is_true (Tag.has_label "integration" merged));
          test "unset speed inherits from first arg" (fun () ->
              let t1 = Tag.speed Tag.Slow in
              let t2 = Tag.empty in
              let merged = Tag.merge t1 t2 in
              is_true (Tag.get_speed merged = Tag.Slow));
        ];
      group "predicate"
        [
          test "initial_predicate drops disabled" (fun () ->
              let t = Tag.labels [ "disabled" ] in
              let filter = Tag.filter_predicate Tag.initial_predicate in
              is_true (filter t = `Skip));
          test "initial_predicate runs normal tests" (fun () ->
              let t = Tag.labels [ "unit" ] in
              let filter = Tag.filter_predicate Tag.initial_predicate in
              is_true (filter t = `Run));
          test "require_tag runs matching" (fun () ->
              let t = Tag.labels [ "integration" ] in
              let pred = Tag.require_tag "integration" Tag.empty_predicate in
              let filter = Tag.filter_predicate pred in
              is_true (filter t = `Run));
          test "require_tag skips non-matching" (fun () ->
              let t = Tag.labels [ "unit" ] in
              let pred = Tag.require_tag "integration" Tag.empty_predicate in
              let filter = Tag.filter_predicate pred in
              is_true (filter t = `Skip));
          test "drop_tag skips matching" (fun () ->
              let t = Tag.labels [ "slow" ] in
              let pred = Tag.drop_tag "slow" Tag.empty_predicate in
              let filter = Tag.filter_predicate pred in
              is_true (filter t = `Skip));
          test "drop_tag runs non-matching" (fun () ->
              let t = Tag.labels [ "fast" ] in
              let pred = Tag.drop_tag "slow" Tag.empty_predicate in
              let filter = Tag.filter_predicate pred in
              is_true (filter t = `Run));
        ];
    ]

let skip_tests =
  group "Skip"
    [
      test "skip from inside test" (fun () ->
          if true then skip ~reason:"testing skip" ();
          fail "should not reach here");
    ]

let bracket_tests =
  let setup_called = ref false in
  let teardown_called = ref false in
  group "bracket"
    [
      bracket
        ~setup:(fun () ->
          setup_called := true;
          42)
        ~teardown:(fun _ -> teardown_called := true)
        "passes resource to test"
        (fun x -> equal Testable.int 42 x);
      test "partial application creates reusable constructor" (fun () ->
          setup_called := false;
          teardown_called := false;
          let with_int =
            bracket ~setup:(fun () -> 123) ~teardown:(fun _ -> ())
          in
          (* with_int now has type: string -> (int -> 'a) -> test *)
          let _t1 = with_int "test1" (fun n -> equal Testable.int 123 n) in
          let _t2 = with_int "test2" (fun n -> equal Testable.int 123 n) in
          is_true true);
    ]

let timeout_tests =
  group "Timeout"
    [ test ~timeout:10.0 "test with timeout passes" (fun () -> is_true true) ]

let property_tests =
  group "Property"
    [
      prop "reverse is involutive"
        Testable.(list int)
        (fun l -> List.rev (List.rev l) = l);
      prop "append length"
        Testable.(pair (list int) (list int))
        (fun (l1, l2) ->
          List.length (l1 @ l2) = List.length l1 + List.length l2);
      prop "division with precondition"
        Testable.(pair int int)
        (fun (a, b) ->
          assume (b <> 0);
          (a / b * b) + (a mod b) = a);
      prop' "sorted list is sorted"
        Testable.(list int)
        (fun l ->
          let sorted = List.sort Int.compare l in
          let rec is_sorted = function
            | [] | [ _ ] -> true
            | x :: y :: rest -> x <= y && is_sorted (y :: rest)
          in
          is_true (is_sorted sorted));
    ]

let property_regression_tests =
  group "Property regressions"
    [
      group "generator independence"
        [
          test "pair components are not locked to the same RNG stream"
            (fun () ->
              let gen = Gen.pair (Gen.int_range 0 9) (Gen.int_range 0 9) in
              let rand = Random.State.make [| 42 |] in
              let found =
                Gen.find ~count:100 ~f:(fun (a, b) -> a <> b) gen rand
              in
              is_some found);
          test "triple components are not locked to the same RNG stream"
            (fun () ->
              let gen =
                Gen.triple (Gen.int_range 0 9) (Gen.int_range 0 9)
                  (Gen.int_range 0 9)
              in
              let rand = Random.State.make [| 43 |] in
              let found =
                Gen.find ~count:100
                  ~f:(fun (a, b, c) -> not (a = b && b = c))
                  gen rand
              in
              is_some found);
        ];
      group "range behavior"
        [
          test "int generator reaches values beyond 30-bit range" (fun () ->
              if Sys.word_size <= 32 then
                skip ~reason:"requires a wide int platform" ()
              else
                let bound = (1 lsl 30) - 1 in
                let rand = Random.State.make [| 44 |] in
                let found =
                  Gen.find ~count:20
                    ~f:(fun n -> n > bound || n < -bound - 1)
                    Gen.int rand
                in
                is_some found);
          test "nativeint generator reaches values beyond 30-bit range"
            (fun () ->
              if Sys.word_size <= 32 then
                skip ~reason:"requires a wide nativeint platform" ()
              else
                let bound = Nativeint.of_int ((1 lsl 30) - 1) in
                let lower = Nativeint.sub (Nativeint.neg bound) 1n in
                let rand = Random.State.make [| 45 |] in
                let found =
                  Gen.find ~count:20
                    ~f:(fun n -> n > bound || n < lower)
                    Gen.nativeint rand
                in
                is_some found);
          test "int_range min_int..0 does not collapse to min_int" (fun () ->
              let rand = Random.State.make [| 46 |] in
              let found =
                Gen.find ~count:100
                  ~f:(fun n -> n <> min_int)
                  (Gen.int_range min_int 0) rand
              in
              is_some found);
          test "int64_range near max_int has more than one value" (fun () ->
              let low = Int64.sub Int64.max_int 10L in
              let rand = Random.State.make [| 47 |] in
              let found =
                Gen.find ~count:100
                  ~f:(fun n -> n <> low)
                  (Gen.int64_range low Int64.max_int)
                  rand
              in
              is_some found);
        ];
      group "config behavior"
        [
          test "default prop_count does not override a custom max_gen"
            (fun () ->
              let module P = Windtrap_prop.Prop in
              let module A = Windtrap_prop.Arbitrary in
              let config = { P.default_config with max_gen = 7 } in
              let arb = A.make ~gen:Gen.int ~print:string_of_int in
              Fun.protect
                ~finally:(fun () -> P.set_default_count None)
                (fun () ->
                  P.set_default_count (Some 500);
                  match
                    P.check ~config ~rand:(Random.State.make [| 48 |]) arb
                      (fun _ -> reject ())
                  with
                  | P.Gave_up { count; discarded; _ } ->
                      equal Testable.int 0 count;
                      equal Testable.int 7 discarded
                  | _ -> fail "expected Gave_up result"));
        ];
    ]

let diffing_tests =
  let open Testable in
  group "Component Diffing"
    [
      group "list"
        [
          test "equal lists pass" (fun () ->
              match check (list int) with
              | None -> fail "list testable should have check"
              | Some check_fn -> (
                  match check_fn [ 1; 2; 3 ] [ 1; 2; 3 ] with
                  | Pass -> ()
                  | Fail _ -> fail "equal lists should pass"));
          test "different lists fail with diff" (fun () ->
              match check (list int) with
              | None -> fail "list testable should have check"
              | Some check_fn -> (
                  match check_fn [ 1; 2; 3 ] [ 1; 99; 3 ] with
                  | Pass -> fail "different lists should fail"
                  | Fail { diff = Some _; _ } -> ()
                  | Fail { diff = None; _ } ->
                      fail "should have diff for single element change"));
          test "very different lists fall back to simple diff" (fun () ->
              match check (list int) with
              | None -> fail "list testable should have check"
              | Some check_fn -> (
                  match check_fn [ 1; 2; 3; 4; 5 ] [ 10; 20; 30; 40; 50 ] with
                  | Pass -> fail "completely different lists should fail"
                  | Fail { diff = None; _ } ->
                      () (* >80% edit ratio, no element-level diff *)
                  | Fail { diff = Some _; _ } ->
                      fail "should fall back to simple diff for high edit ratio"
                  ));
          test "empty vs non-empty list" (fun () ->
              match check (list int) with
              | None -> fail "list testable should have check"
              | Some check_fn -> (
                  match check_fn [] [ 1; 2; 3 ] with
                  | Pass -> fail "empty vs non-empty should fail"
                  | Fail _ -> ()));
        ];
      group "array"
        [
          test "equal arrays pass" (fun () ->
              match check (array int) with
              | None -> fail "array testable should have check"
              | Some check_fn -> (
                  match check_fn [| 1; 2; 3 |] [| 1; 2; 3 |] with
                  | Pass -> ()
                  | Fail _ -> fail "equal arrays should pass"));
          test "different arrays fail with diff" (fun () ->
              match check (array int) with
              | None -> fail "array testable should have check"
              | Some check_fn -> (
                  match check_fn [| 1; 2; 3 |] [| 1; 99; 3 |] with
                  | Pass -> fail "different arrays should fail"
                  | Fail { diff = Some _; _ } -> ()
                  | Fail { diff = None; _ } ->
                      fail "should have diff for single element change"));
        ];
      group "string"
        [
          test "equal strings pass" (fun () ->
              match check string with
              | None -> fail "string testable should have check"
              | Some check_fn -> (
                  match check_fn "hello" "hello" with
                  | Pass -> ()
                  | Fail _ -> fail "equal strings should pass"));
          test "different strings fail with diff" (fun () ->
              match check string with
              | None -> fail "string testable should have check"
              | Some check_fn -> (
                  match check_fn "hello" "hxllo" with
                  | Pass -> fail "different strings should fail"
                  | Fail { diff = Some _; _ } -> ()
                  | Fail { diff = None; _ } ->
                      fail "should have diff for single char change"));
          test "very different strings fall back to simple diff" (fun () ->
              match check string with
              | None -> fail "string testable should have check"
              | Some check_fn -> (
                  match check_fn "abcde" "vwxyz" with
                  | Pass -> fail "completely different strings should fail"
                  | Fail { diff = None; _ } -> ()
                  | Fail { diff = Some _; _ } ->
                      fail "should fall back to simple diff for high edit ratio"
                  ));
        ];
    ]

let cli_tests =
  let module Cli = Windtrap__Cli in
  let parse args = Cli.parse (Array.of_list ("test" :: args)) in
  group "CLI"
    [
      group "parsing"
        [
          test "empty argv" (fun () ->
              let cli = Cli.parse [||] in
              is_none cli.stream;
              is_none cli.format;
              is_none cli.filter);
          test "-v sets format to verbose" (fun () ->
              let cli = parse [ "-v" ] in
              some Testable.string "verbose" cli.format);
          test "--verbose sets format to verbose" (fun () ->
              let cli = parse [ "--verbose" ] in
              some Testable.string "verbose" cli.format);
          test "--seed parses integer" (fun () ->
              let cli = parse [ "--seed"; "42" ] in
              some Testable.int 42 cli.seed);
          test "--seed=N equals-style" (fun () ->
              let cli = parse [ "--seed=99" ] in
              some Testable.int 99 cli.seed);
          test "--timeout parses float" (fun () ->
              let cli = parse [ "--timeout"; "5.5" ] in
              let t =
                match cli.timeout with
                | Some t -> t
                | None -> fail "expected Some"
              in
              is_true (Float.abs (t -. 5.5) < 0.001));
          test "--timeout=N equals-style" (fun () ->
              let cli = parse [ "--timeout=2.0" ] in
              is_some cli.timeout);
          test "-s sets stream" (fun () ->
              let cli = parse [ "-s" ] in
              some Testable.bool true cli.stream);
          test "-q sets quick" (fun () ->
              let cli = parse [ "-q" ] in
              some Testable.bool true cli.quick);
          test "-x sets bail to 1" (fun () ->
              let cli = parse [ "-x" ] in
              some Testable.int 1 cli.bail);
          test "--bail sets bail count" (fun () ->
              let cli = parse [ "--bail"; "3" ] in
              some Testable.int 3 cli.bail);
          test "--bail=N equals-style" (fun () ->
              let cli = parse [ "--bail=5" ] in
              some Testable.int 5 cli.bail);
          test "-l sets list_only" (fun () ->
              let cli = parse [ "-l" ] in
              some Testable.bool true cli.list_only);
          test "-u sets update" (fun () ->
              let cli = parse [ "-u" ] in
              some Testable.bool true cli.update);
          test "-f sets filter" (fun () ->
              let cli = parse [ "-f"; "pattern" ] in
              some Testable.string "pattern" cli.filter);
          test "--filter=pattern equals-style" (fun () ->
              let cli = parse [ "--filter=mytest" ] in
              some Testable.string "mytest" cli.filter);
          test "--format sets format" (fun () ->
              let cli = parse [ "--format"; "compact" ] in
              some Testable.string "compact" cli.format);
          test "--junit sets path" (fun () ->
              let cli = parse [ "--junit"; "report.xml" ] in
              some Testable.string "report.xml" cli.junit);
          test "-o sets output_dir" (fun () ->
              let cli = parse [ "-o"; "/tmp/test" ] in
              some Testable.string "/tmp/test" cli.output_dir);
          test "positional becomes filter" (fun () ->
              let cli = parse [ "mytest" ] in
              some Testable.string "mytest" cli.filter);
          test "-- stops parsing" (fun () ->
              let cli = parse [ "--"; "--unknown" ] in
              is_none cli.filter);
          test "combined flags" (fun () ->
              let cli = parse [ "-s"; "-q"; "-x"; "-f"; "foo" ] in
              some Testable.bool true cli.stream;
              some Testable.bool true cli.quick;
              some Testable.int 1 cli.bail;
              some Testable.string "foo" cli.filter);
          test "--tag adds to tags list" (fun () ->
              let cli = parse [ "--tag"; "integration" ] in
              equal (Testable.list Testable.string) [ "integration" ] cli.tags);
          test "--tag is repeatable" (fun () ->
              let cli = parse [ "--tag"; "unit"; "--tag"; "fast" ] in
              equal (Testable.list Testable.string) [ "fast"; "unit" ] cli.tags);
          test "--tag=LABEL equals-style" (fun () ->
              let cli = parse [ "--tag=integration" ] in
              equal (Testable.list Testable.string) [ "integration" ] cli.tags);
          test "--exclude-tag adds to exclude list" (fun () ->
              let cli = parse [ "--exclude-tag"; "flaky" ] in
              equal (Testable.list Testable.string) [ "flaky" ] cli.exclude_tags);
          test "--exclude-tag=LABEL equals-style" (fun () ->
              let cli = parse [ "--exclude-tag=slow" ] in
              equal (Testable.list Testable.string) [ "slow" ] cli.exclude_tags);
          test "-e sets exclude" (fun () ->
              let cli = parse [ "-e"; "Snapshot" ] in
              some Testable.string "Snapshot" cli.exclude);
          test "--exclude sets exclude" (fun () ->
              let cli = parse [ "--exclude"; "Snapshot" ] in
              some Testable.string "Snapshot" cli.exclude);
          test "--exclude=PATTERN equals-style" (fun () ->
              let cli = parse [ "--exclude=Slow" ] in
              some Testable.string "Slow" cli.exclude);
          test "--prop-count parses integer" (fun () ->
              let cli = parse [ "--prop-count"; "500" ] in
              equal (Testable.option Testable.int) (Some 500) cli.prop_count);
          test "--prop-count=N equals-style" (fun () ->
              let cli = parse [ "--prop-count=200" ] in
              equal (Testable.option Testable.int) (Some 200) cli.prop_count);
          test "--failed sets failed" (fun () ->
              let cli = parse [ "--failed" ] in
              some Testable.bool true cli.failed);
        ];
      group "parse_format"
        [
          test "verbose" (fun () ->
              is_true (Cli.parse_format "verbose" = Windtrap__Progress.Verbose));
          test "compact" (fun () ->
              is_true (Cli.parse_format "compact" = Windtrap__Progress.Compact));
          test "tap" (fun () ->
              is_true (Cli.parse_format "tap" = Windtrap__Progress.Tap));
          test "junit" (fun () ->
              is_true (Cli.parse_format "junit" = Windtrap__Progress.Junit));
        ];
    ]

let expect_tests =
  group "Expect"
    [
      group "normalize"
        [
          test "strips trailing whitespace" (fun () ->
              let result = Windtrap__Expect.normalize "hello   \nworld  \n" in
              equal Testable.string "hello\nworld" result);
          test "removes leading blank lines" (fun () ->
              let result = Windtrap__Expect.normalize "\n\nhello" in
              equal Testable.string "hello" result);
          test "removes trailing blank lines" (fun () ->
              let result = Windtrap__Expect.normalize "hello\n\n\n" in
              equal Testable.string "hello" result);
          test "dedents common indentation" (fun () ->
              let result = Windtrap__Expect.normalize "  hello\n  world" in
              equal Testable.string "hello\nworld" result);
          test "dedents preserves relative indentation" (fun () ->
              let result = Windtrap__Expect.normalize "  hello\n    world" in
              equal Testable.string "hello\n  world" result);
          test "empty string" (fun () ->
              let result = Windtrap__Expect.normalize "" in
              equal Testable.string "" result);
          test "whitespace only" (fun () ->
              let result = Windtrap__Expect.normalize "   \n   \n" in
              equal Testable.string "" result);
        ];
    ]

let path_ops_tests =
  group "Path_ops"
    [
      group "sanitize_component"
        [
          test "alphanumeric passes through" (fun () ->
              equal Testable.string "hello123"
                (Windtrap__Path_ops.sanitize_component "hello123"));
          test "spaces become underscores" (fun () ->
              equal Testable.string "hello_world"
                (Windtrap__Path_ops.sanitize_component "hello world"));
          test "special chars become underscores" (fun () ->
              equal Testable.string "a_b_c"
                (Windtrap__Path_ops.sanitize_component "a/b\\c"));
          test "empty becomes unnamed" (fun () ->
              equal Testable.string "unnamed"
                (Windtrap__Path_ops.sanitize_component ""));
          test "dotdot becomes unnamed" (fun () ->
              equal Testable.string "unnamed"
                (Windtrap__Path_ops.sanitize_component ".."));
          test "dashes and underscores preserved" (fun () ->
              equal Testable.string "my-test_case"
                (Windtrap__Path_ops.sanitize_component "my-test_case"));
          test "dots preserved" (fun () ->
              equal Testable.string "test.ml"
                (Windtrap__Path_ops.sanitize_component "test.ml"));
          test "long names are truncated" (fun () ->
              let long = String.make 100 'a' in
              let result = Windtrap__Path_ops.sanitize_component long in
              is_true (String.length result <= 80));
        ];
      group "collapse_home"
        [
          test "replaces home prefix with tilde" (fun () ->
              match Sys.getenv_opt "HOME" with
              | Some home ->
                  let path = home ^ "/some/path" in
                  let result = Windtrap__Path_ops.collapse_home path in
                  equal Testable.string "~/some/path" result
              | None -> skip ~reason:"HOME not set" ());
          test "leaves non-home paths unchanged" (fun () ->
              let result = Windtrap__Path_ops.collapse_home "/usr/bin/test" in
              equal Testable.string "/usr/bin/test" result);
        ];
      group "mkdir_p"
        [
          test "creates nested directories" (fun () ->
              let tmpdir = Filename.temp_dir "windtrap_test" "" in
              let nested = Filename.concat tmpdir "a/b/c" in
              Windtrap__Path_ops.mkdir_p nested;
              is_true (Sys.file_exists nested && Sys.is_directory nested));
          test "no-op on existing directory" (fun () ->
              let tmpdir = Filename.temp_dir "windtrap_test" "" in
              Windtrap__Path_ops.mkdir_p tmpdir;
              is_true (Sys.file_exists tmpdir));
        ];
    ]

let text_tests =
  group "Text"
    [
      test "normalize_newlines converts CRLF" (fun () ->
          equal Testable.string "a\nb\n"
            (Windtrap__Text.normalize_newlines "a\r\nb\r\n"));
      test "normalize_newlines converts CR" (fun () ->
          equal Testable.string "a\nb\n"
            (Windtrap__Text.normalize_newlines "a\rb\r"));
      test "length_utf8 counts codepoints" (fun () ->
          equal Testable.int 5 (Windtrap__Text.length_utf8 "hello");
          equal Testable.int 0 (Windtrap__Text.length_utf8 ""));
      test "contains_substring finds match" (fun () ->
          is_true
            (Windtrap__Text.contains_substring ~pattern:"world" "hello world"));
      test "contains_substring no match" (fun () ->
          is_false
            (Windtrap__Text.contains_substring ~pattern:"xyz" "hello world"));
      test "contains_substring empty pattern matches" (fun () ->
          is_true (Windtrap__Text.contains_substring ~pattern:"" "hello"));
    ]

let distance_tests =
  group "Distance"
    [
      test "equal strings produce empty script" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "hello"
              "hello"
          in
          equal (Testable.list Testable.pass) [] script);
      test "single insertion" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "ac" "abc"
          in
          is_true (List.length script = 1);
          match script with
          | [ Windtrap__Distance.Insert _ ] -> ()
          | _ -> fail "expected single Insert");
      test "single deletion" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "abc" "ac"
          in
          is_true (List.length script = 1);
          match script with
          | [ Windtrap__Distance.Delete _ ] -> ()
          | _ -> fail "expected single Delete");
      test "substitution" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "abc" "axc"
          in
          is_true (List.length script = 1);
          match script with
          | [ Windtrap__Distance.Substitute _ ] -> ()
          | _ -> fail "expected single Substitute");
      test "empty inputs" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "" ""
          in
          equal (Testable.list Testable.pass) [] script);
      test "empty to non-empty" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "" "abc"
          in
          equal Testable.int 3 (List.length script));
      test "list container" (fun () ->
          let script =
            Windtrap__Distance.levenshtein List ~equal:Int.equal [ 1; 2; 3 ]
              [ 1; 2; 3 ]
          in
          equal (Testable.list Testable.pass) [] script);
      test "list with changes" (fun () ->
          let script =
            Windtrap__Distance.levenshtein List ~equal:Int.equal [ 1; 2; 3 ]
              [ 1; 99; 3 ]
          in
          is_true (List.length script = 1));
      test "array container" (fun () ->
          let script =
            Windtrap__Distance.levenshtein Array ~equal:Int.equal [| 1; 2 |]
              [| 1; 3 |]
          in
          is_true (List.length script = 1));
      test "edit_indices returns correct positions" (fun () ->
          let script =
            Windtrap__Distance.levenshtein String ~equal:Char.equal "abc" "axc"
          in
          let expected_idx, actual_idx =
            Windtrap__Distance.edit_indices script
          in
          equal (Testable.list Testable.int) [ 1 ] expected_idx;
          equal (Testable.list Testable.int) [ 1 ] actual_idx);
    ]

let runner_filter_tests =
  let make_filter ?(quick = false) ?filter_pattern ?exclude_pattern
      ?(required_tags = []) ?(dropped_tags = []) () =
    Windtrap__Runner.make_filter ~quick ~filter_pattern ~exclude_pattern
      ~required_tags ~dropped_tags
  in
  group "Runner"
    [
      group "make_filter"
        [
          test "no filters runs everything" (fun () ->
              let filter = make_filter () in
              is_true (filter ~path:"any test" Tag.empty = `Run));
          test "quick mode skips slow tests" (fun () ->
              let filter = make_filter ~quick:true () in
              is_true (filter ~path:"slow test" (Tag.speed Tag.Slow) = `Skip));
          test "quick mode runs quick tests" (fun () ->
              let filter = make_filter ~quick:true () in
              is_true (filter ~path:"fast test" Tag.empty = `Run));
          test "pattern filter matches" (fun () ->
              let filter = make_filter ~filter_pattern:"foo" () in
              is_true (filter ~path:"test foo bar" Tag.empty = `Run));
          test "pattern filter skips non-matching" (fun () ->
              let filter = make_filter ~filter_pattern:"foo" () in
              is_true (filter ~path:"test bar baz" Tag.empty = `Skip));
          test "disabled tests are skipped" (fun () ->
              let filter = make_filter () in
              is_true
                (filter ~path:"disabled test" (Tag.labels [ "disabled" ])
                = `Skip));
          test "combined quick and pattern" (fun () ->
              let filter = make_filter ~quick:true ~filter_pattern:"match" () in
              is_true (filter ~path:"match test" Tag.empty = `Run);
              is_true (filter ~path:"no" Tag.empty = `Skip);
              is_true (filter ~path:"match slow" (Tag.speed Tag.Slow) = `Skip));
          test "required tag runs matching" (fun () ->
              let filter = make_filter ~required_tags:[ "integration" ] () in
              is_true (filter ~path:"test" (Tag.labels [ "integration" ]) = `Run));
          test "required tag skips non-matching" (fun () ->
              let filter = make_filter ~required_tags:[ "integration" ] () in
              is_true (filter ~path:"test" Tag.empty = `Skip));
          test "excluded tag skips matching" (fun () ->
              let filter = make_filter ~dropped_tags:[ "flaky" ] () in
              is_true (filter ~path:"test" (Tag.labels [ "flaky" ]) = `Skip));
          test "excluded tag runs non-matching" (fun () ->
              let filter = make_filter ~dropped_tags:[ "flaky" ] () in
              is_true (filter ~path:"test" Tag.empty = `Run));
          test "exclude pattern skips matching" (fun () ->
              let filter = make_filter ~exclude_pattern:"Snapshot" () in
              is_true (filter ~path:"Suite › Snapshot › test" Tag.empty = `Skip));
          test "exclude pattern runs non-matching" (fun () ->
              let filter = make_filter ~exclude_pattern:"Snapshot" () in
              is_true (filter ~path:"Suite › Runner › test" Tag.empty = `Run));
          test "filter and exclude combined" (fun () ->
              let filter =
                make_filter ~filter_pattern:"Suite" ~exclude_pattern:"Slow" ()
              in
              is_true (filter ~path:"Suite › Fast" Tag.empty = `Run);
              is_true (filter ~path:"Suite › Slow" Tag.empty = `Skip);
              is_true (filter ~path:"Other › Fast" Tag.empty = `Skip));
        ];
    ]

let focused_tests =
  let module T = Windtrap__Test in
  group "Focused"
    [
      test "has_focused detects ftest" (fun () ->
          let tests =
            [ T.test "a" (fun () -> ()); T.ftest "b" (fun () -> ()) ]
          in
          is_true (T.has_focused tests));
      test "has_focused detects fgroup" (fun () ->
          let tests = [ T.fgroup "g" [ T.test "a" (fun () -> ()) ] ] in
          is_true (T.has_focused tests));
      test "has_focused returns false when none focused" (fun () ->
          let tests =
            [
              T.test "a" (fun () -> ());
              T.group "g" [ T.test "b" (fun () -> ()) ];
            ]
          in
          is_false (T.has_focused tests));
      test "has_focused finds nested ftest" (fun () ->
          let tests = [ T.group "g" [ T.ftest "a" (fun () -> ()) ] ] in
          is_true (T.has_focused tests));
    ]

let hooks_tests =
  let counter = ref 0 in
  let log = ref [] in
  group "Hooks"
    [
      group "before_each"
        ~before_each:(fun () -> incr counter)
        ~setup:(fun () -> counter := 0)
        [
          test "runs before first test" (fun () ->
              equal Testable.int 1 !counter);
          test "runs before second test" (fun () ->
              equal Testable.int 2 !counter);
        ];
      group "after_each"
        ~after_each:(fun () -> incr counter)
        ~setup:(fun () -> counter := 0)
        [
          test "counter is 0 before first test" (fun () ->
              equal Testable.int 0 !counter);
          test "after_each ran after first test" (fun () ->
              equal Testable.int 1 !counter);
        ];
      group "nested hooks ordering"
        ~before_each:(fun () -> log := "outer_before" :: !log)
        ~after_each:(fun () -> log := "outer_after" :: !log)
        ~setup:(fun () -> log := [])
        [
          group "inner"
            ~before_each:(fun () -> log := "inner_before" :: !log)
            ~after_each:(fun () -> log := "inner_after" :: !log)
            [
              test "hooks run in correct order" (fun () ->
                  equal
                    (Testable.list Testable.string)
                    [ "inner_before"; "outer_before" ]
                    !log);
            ];
        ];
    ]

let snapshot_config_tests =
  group "Snapshot"
    [
      group "Config"
        [
          test "default mode is Check" (fun () ->
              let config = Windtrap__Snapshot.Config.create () in
              is_true
                (Windtrap__Snapshot.Config.mode config
                = Windtrap__Snapshot.Check));
          test "with_mode changes mode" (fun () ->
              let config =
                Windtrap__Snapshot.Config.create ()
                |> Windtrap__Snapshot.Config.with_mode Windtrap__Snapshot.Update
              in
              is_true
                (Windtrap__Snapshot.Config.mode config
                = Windtrap__Snapshot.Update));
          test "default diff_context is 3" (fun () ->
              let config = Windtrap__Snapshot.Config.create () in
              equal Testable.int 3
                (Windtrap__Snapshot.Config.diff_context config));
          test "with_filter appends filter" (fun () ->
              let config =
                Windtrap__Snapshot.Config.create ()
                |> Windtrap__Snapshot.Config.with_filter String.uppercase_ascii
              in
              equal Testable.int 1
                (List.length (Windtrap__Snapshot.Config.filters config)));
        ];
    ]

let () =
  run "Windtrap"
    [
      testable_tests;
      check_tests;
      tag_tests;
      skip_tests;
      bracket_tests;
      timeout_tests;
      property_tests;
      property_regression_tests;
      diffing_tests;
      cli_tests;
      expect_tests;
      path_ops_tests;
      text_tests;
      distance_tests;
      runner_filter_tests;
      focused_tests;
      hooks_tests;
      snapshot_config_tests;
    ]
