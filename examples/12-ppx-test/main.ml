(* Module-based test syntax with PPX.

   The ppx_windtrap preprocessor provides [let%test], [module%test], and
   [%%run_tests] for writing self-contained test executables with grouping.

   To run: dune exec ./examples/12-ppx-test/main.exe *)

open Windtrap

let add x y = x + y
let mul x y = x * y

(* Simple test — the body uses assertions and returns unit. *)
let%test "addition" = equal Testable.int 5 (add 2 3)
let%test "multiplication" = equal Testable.int 20 (mul 4 5)

(* Group related tests with module%test. *)
module%test Arithmetic = struct
  let%test "add commutative" = equal Testable.int (add 3 7) (add 7 3)
  let%test "mul commutative" = equal Testable.int (mul 3 7) (mul 7 3)
  let%test "add identity" = equal Testable.int 42 (add 0 42)
  let%test "mul identity" = equal Testable.int 42 (mul 1 42)
end

(* Nested groups for deeper hierarchies. *)
module%test StringOps = struct
  let%test "concat" =
    equal Testable.string "hello world" ("hello" ^ " " ^ "world")

  let%test "length" = equal Testable.int 3 (String.length "abc")

  module%test Uppercase = struct
    let%test "simple" =
      equal Testable.string "HELLO" (String.uppercase_ascii "hello")

    let%test "empty" = equal Testable.string "" (String.uppercase_ascii "")
  end

  module%test Trim = struct
    let%test "spaces" = equal Testable.string "hi" (String.trim "  hi  ")
    let%test "already trimmed" = equal Testable.string "hi" (String.trim "hi")
  end
end

(* Execute all registered tests. An optional name labels the suite. *)
[%%run_tests "PPX Test Syntax"]
