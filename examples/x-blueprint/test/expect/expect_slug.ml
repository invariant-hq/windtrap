let%expect_test "slugify, at a glance" =
  List.iter
    (fun s -> print_endline (Windtrap_example_blueprint.Slug.slugify s))
    [ "Hello, World!"; "  OCaml 5.x  "; "a--b" ];
  [%expect {|
    hello-world
    ocaml-5-x
    a-b
  |}]
