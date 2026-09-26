ppx_windtrap's expansions and refusals, as pp.exe prints them (see
../coverage.t).

  $ export OCAML_COLOR=never
  $ expand() { ../pp.exe -apply ppx_windtrap "$@"; }

  $ expand --impl ./expect_attributes.ml
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./expect_attributes.ml"
      ~pos:("./expect_attributes.ml", 6, 0, 75) ~tags:["kept"] "dropped"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./expect_attributes.ml", 6, 0, 56)
           ~body_end:("./expect_attributes.ml", 6, 56, 56) ~nodes:[]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit) (fun () -> ()))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./expect_attributes.ml"
      ~pos:("./expect_attributes.ml", 9, 0, 125) ~tags:[] "carried"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./expect_attributes.ml", 9, 0, 125)
           ~body_end:("./expect_attributes.ml", 12, 45, 45)
           ~nodes:[("./expect_attributes.ml", 11, 2, 19)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   print_string "x";
                   (((Ppx_windtrap_runtime.Ppx_runtime.reach
                        ("./expect_attributes.ml", 11, 2, 19);
                      Windtrap.expect
                        (Expect_test_config.sanitize (Windtrap.output ()))
                        (("./expect_attributes.ml", 11, 2, 19), {| x |})))
                   [@carried ]);
                   ignore ((Expect_test_config.sanitize (Windtrap.output ()))
                     [@carried_output ])))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))

  $ expand --impl ./expect_basic.ml
  let greet name = Printf.printf "hello %s\n" name
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./expect_basic.ml"
      ~pos:("./expect_basic.ml", 7, 0, 268) ~tags:[] "greetings"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./expect_basic.ml", 7, 0, 268)
           ~body_end:("./expect_basic.ml", 18, 11, 11)
           ~nodes:[("./expect_basic.ml", 9, 2, 35);
                  ("./expect_basic.ml", 15, 2, 40);
                  ("./expect_basic.ml", 18, 2, 11)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   greet "world";
                   (Ppx_windtrap_runtime.Ppx_runtime.reach
                      ("./expect_basic.ml", 9, 2, 35);
                    Windtrap.expect
                      (Expect_test_config.sanitize (Windtrap.output ()))
                      (("./expect_basic.ml", 9, 2, 35),
                        {|
      hello world
    |}));
                   greet "again";
                   (let noise =
                      Expect_test_config.sanitize (Windtrap.output ()) in
                    Printf.printf "captured %d bytes\n" (String.length noise);
                    (Ppx_windtrap_runtime.Ppx_runtime.reach
                       ("./expect_basic.ml", 15, 2, 40);
                     Windtrap.expect_exact
                       (Expect_test_config.sanitize (Windtrap.output ()))
                       (("./expect_basic.ml", 15, 2, 40),
                         {|captured 12 bytes
  |}));
                    print_string "";
                    Ppx_windtrap_runtime.Ppx_runtime.reach
                      ("./expect_basic.ml", 18, 2, 11);
                    Windtrap.expect
                      (Expect_test_config.sanitize (Windtrap.output ()))
                      (("./expect_basic.ml", 18, 2, 11), ""))))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./expect_basic.ml"
      ~pos:("./expect_basic.ml", 20, 0, 65) ~tags:[] "line_20"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./expect_basic.ml", 20, 0, 65)
           ~body_end:("./expect_basic.ml", 22, 20, 20)
           ~nodes:[("./expect_basic.ml", 22, 2, 20)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   print_string "quoted";
                   Ppx_windtrap_runtime.Ppx_runtime.reach
                     ("./expect_basic.ml", 22, 2, 20);
                   Windtrap.expect
                     (Expect_test_config.sanitize (Windtrap.output ()))
                     (("./expect_basic.ml", 22, 2, 20), "quoted")))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./expect_basic.ml"
      ~pos:("./expect_basic.ml", 24, 0, 85) ~tags:["slow"] "tagged"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./expect_basic.ml", 24, 0, 85)
           ~body_end:("./expect_basic.ml", 26, 21, 21)
           ~nodes:[("./expect_basic.ml", 26, 2, 21)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   print_string "x";
                   Ppx_windtrap_runtime.Ppx_runtime.reach
                     ("./expect_basic.ml", 26, 2, 21);
                   Windtrap.expect
                     (Expect_test_config.sanitize (Windtrap.output ()))
                     (("./expect_basic.ml", 26, 2, 21), {x| x |x})))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))

  $ expand --impl ./test_basic.ml
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
      ~pos:("./test_basic.ml", 4, 0, 40) ~tags:[] "addition"
      (fun () -> assert ((1 + 1) = 2))
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
      ~pos:("./test_basic.ml", 5, 0, 15) ~tags:[] "line_5" (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
      ~pos:("./test_basic.ml", 6, 0, 39) ~tags:["slow"] "tagged" (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
      ~pos:("./test_basic.ml", 7, 0, 44) ~tags:["slow"; "io"] "multi"
      (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group ~file:"./test_basic.ml"
      ~tags:[] "Outer"
  module Outer =
    struct
      let helper = 41
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
          ~pos:("./test_basic.ml", 11, 2, 45) ~tags:[] "inner"
          (fun () -> assert ((helper + 1) = 42))
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.enter_group ~file:"./test_basic.ml"
          ~tags:[] "Nested"
      module Nested =
        struct
          let () =
            Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
              ~pos:("./test_basic.ml", 14, 4, 19) ~tags:[] "line_14"
              (fun () -> ())
        end
      let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
    end
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group ~file:"./test_basic.ml"
      ~tags:["group-tag"] "Tagged"
  module Tagged =
    struct
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./test_basic.ml"
          ~pos:("./test_basic.ml", 19, 2, 33) ~tags:[] "in tagged group"
          (fun () -> ())
    end[@@warning "-60"]
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()

A refusal is an error located at the node, and the driver exits 1:

  $ expand --impl ./reject_bad_payload.ml
  File "./reject_bad_payload.ml", line 3, characters 2-14:
  3 |   [%expect 42]
        ^^^^^^^^^^^^
  Error: Expected a string literal payload
  [1]

  $ expand --impl ./reject_dropped_body.ml
  File "./reject_dropped_body.ml", line 3, characters 33-43:
  3 | let%test "dropped" = ignore (1 [@expect.foo])
                                       ^^^^^^^^^^
  Error: [@@expect.foo] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_expect_outside.ml
  File "./reject_expect_outside.ml", line 1, characters 13-19:
  1 | let f () = [%expect {| nothing |}]
                   ^^^^^^
  Error: [%expect] must appear inside a let%expect_test body
  [1]

  $ expand --impl ./reject_expect_prefix.ml
  File "./reject_expect_prefix.ml", line 1, characters 10-20:
  1 | let x = [%expect.foo]
                ^^^^^^^^^^
  Error: [%expect.foo] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_expectation.ml
  File "./reject_expectation.ml", line 1, characters 24-35:
  1 | let check () = ignore [%expectation {| x |}]
                              ^^^^^^^^^^^
  Error: [%expectation] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_expectation_prefix.ml
  File "./reject_expectation_prefix.ml", line 1, characters 10-25:
  1 | let x = [%expectation.foo]
                ^^^^^^^^^^^^^^^
  Error: [%expectation.foo] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_if_reached.ml
  File "./reject_if_reached.ml", line 2, characters 18-35:
  2 |   if false then [%expect.if_reached {| never |}];
                        ^^^^^^^^^^^^^^^^^
  Error: [%expect.if_reached] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_leftover_attr.ml
  File "./reject_leftover_attr.ml", line 3, characters 13-25:
  3 | let x = (1 [@expect_exact])
                   ^^^^^^^^^^^^
  Error: [@@expect_exact] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_malformed_tags.ml
  File "./reject_malformed_tags.ml", line 1, characters 21-31:
  1 | let%expect_test ("n" [@tags 42]) = ()
                           ^^^^^^^^^^
  Error: Expected [@tags "..."] or [@tags ("...", ...)]
  [1]

  $ expand --impl ./reject_malformed_tags_tuple.ml
  File "./reject_malformed_tags_tuple.ml", line 1, characters 21-35:
  1 | let%expect_test ("n" [@tags "a", 1]) = ()
                           ^^^^^^^^^^^^^^
  Error: Expected [@tags "..."] or [@tags ("...", ...)]
  [1]

  $ expand --impl ./reject_module_attr.ml
  File "./reject_module_attr.ml", line 4, characters 3-22:
  4 | [@@expect.uncaught_exn {| (Failure boom) |}]
         ^^^^^^^^^^^^^^^^^^^
  Error: [@@expect.uncaught_exn] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_name_pattern.ml
  File "./reject_name_pattern.ml", line 1, characters 0-39:
  1 | let%expect_test name = print_string "x"
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Expected let%expect_test "name" = ... or let%expect_test _ = ...
  [1]

  $ expand --impl ./reject_output_payload.ml
  File "./reject_output_payload.ml", line 3, characters 11-24:
  3 |   ignore [%expect.output "x"]
                 ^^^^^^^^^^^^^
  Error: [%expect.output] takes no payload
  [1]

  $ expand --impl ./reject_pattern_attr.ml
  File "./reject_pattern_attr.ml", line 1, characters 27-46:
  1 | let%expect_test ("named" [@expect.uncaught_exn {| boom |}]) =
                                 ^^^^^^^^^^^^^^^^^^^
  Error: [@@expect.uncaught_exn] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_rec_binding.ml
  File "./reject_rec_binding.ml", line 1, characters 0-26:
  1 | let%expect_test rec f = ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Expected let%expect_test <name> = <expr>
  [1]

  $ expand --impl ./reject_test_anonymous_module.ml
  File "./reject_test_anonymous_module.ml", line 1, characters 0-26:
  1 | module%test _ = struct end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Expected let%test "name" = ..., let%test _ = ..., or module%test Name = ...
  [1]

  $ expand --impl ./reject_test_binding_attr.ml
  File "./reject_test_binding_attr.ml", line 1, characters 21-40:
  1 | let%test "n" = () [@@expect.uncaught_exn {| |}]
                           ^^^^^^^^^^^^^^^^^^^
  Error: [@@expect.uncaught_exn] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_test_item.ml
  File "./reject_test_item.ml", line 1, characters 0-21:
  1 | [%%test type t = int]
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Expected let%test "name" = ..., let%test _ = ..., or module%test Name = ...
  [1]

  $ expand --impl ./reject_test_name_pattern.ml
  File "./reject_test_name_pattern.ml", line 3, characters 0-18:
  3 | let%test name = ()
      ^^^^^^^^^^^^^^^^^^
  Error: Expected let%expect_test "name" = ... or let%expect_test _ = ...
  [1]

  $ expand --impl ./reject_test_pattern_attr.ml
  File "./reject_test_pattern_attr.ml", line 1, characters 16-26:
  1 | let%test ("n" [@expect.foo]) = ()
                      ^^^^^^^^^^
  Error: [@@expect.foo] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_two_bindings.ml
  File "./reject_two_bindings.ml", lines 1-2, characters 0-12:
  1 | let%expect_test "a" = ()
  2 | and "b" = ()
  Error: Expected let%expect_test <name> = <expr>
  [1]

  $ expand --impl ./reject_uncaught_exn.ml
  File "./reject_uncaught_exn.ml", line 4, characters 3-22:
  4 | [@@expect.uncaught_exn {| (Failure boom) |}]
         ^^^^^^^^^^^^^^^^^^^
  Error: [@@expect.uncaught_exn] is not supported by ppx_windtrap
  [1]

  $ expand --impl ./reject_unreachable.ml
  File "./reject_unreachable.ml", line 2, characters 18-36:
  2 |   if false then [%expect.unreachable];
                        ^^^^^^^^^^^^^^^^^^
  Error: [%expect.unreachable] is not supported by ppx_windtrap
  [1]

Under the inline_tests cookie "enabled", the expansion is the one without
the cookie:

  $ expand --impl ./test_basic.ml > default.out
  $ expand -cookie 'inline_tests="enabled"' --impl ./test_basic.ml | cmp - default.out

Under "disabled" or "ignored", each form expands to nothing, and the rest
of a dropped body is not checked:

  $ expand -cookie 'inline_tests="disabled"' --impl ./expect_basic.ml
  let greet name = Printf.printf "hello %s\n" name

  $ expand -cookie 'inline_tests="disabled"' --impl ./reject_dropped_body.ml
  let kept = 1

  $ expand -cookie 'inline_tests="ignored"' --impl ./test_basic.ml

A dropped form is still refused for a bad name, shape, tags or node, in
the same words:

  $ for f in reject_name_pattern reject_two_bindings reject_malformed_tags reject_bad_payload reject_unreachable; do
  >   expand --impl "./$f.ml" > enabled.out 2>&1
  >   expand -cookie 'inline_tests="disabled"' --impl "./$f.ml" > disabled.out 2>&1
  >   echo "$f: exit $?"; cmp disabled.out enabled.out
  > done
  reject_name_pattern: exit 1
  reject_two_bindings: exit 1
  reject_malformed_tags: exit 1
  reject_bad_payload: exit 1
  reject_unreachable: exit 1

Any other value is refused:

  $ expand -cookie 'inline_tests="bogus"' --impl ./test_basic.ml
  File "<command-line>", line 1, characters 0-7:
  Error: invalid 'inline_tests' cookie (bogus), expected one of: enabled, disabled or ignored
  [1]

The library-name cookie names the library a test registers under:

  $ expand -cookie 'library-name="scene"' --impl ./test_basic.ml
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
      ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 4, 0, 40) ~tags:[]
      "addition" (fun () -> assert ((1 + 1) = 2))
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
      ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 5, 0, 15) ~tags:[]
      "line_5" (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
      ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 6, 0, 39) ~tags:
      ["slow"] "tagged" (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
      ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 7, 0, 44)
      ~tags:["slow"; "io"] "multi" (fun () -> ())
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group ~library:"scene"
      ~file:"./test_basic.ml" ~tags:[] "Outer"
  module Outer =
    struct
      let helper = 41
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
          ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 11, 2, 45) ~tags:[]
          "inner" (fun () -> assert ((helper + 1) = 42))
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.enter_group ~library:"scene"
          ~file:"./test_basic.ml" ~tags:[] "Nested"
      module Nested =
        struct
          let () =
            Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
              ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 14, 4, 19)
              ~tags:[] "line_14" (fun () -> ())
        end
      let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
    end
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group ~library:"scene"
      ~file:"./test_basic.ml" ~tags:["group-tag"] "Tagged"
  module Tagged =
    struct
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test ~library:"scene"
          ~file:"./test_basic.ml" ~pos:("./test_basic.ml", 19, 2, 33) ~tags:[]
          "in tagged group" (fun () -> ())
    end[@@warning "-60"]
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()
