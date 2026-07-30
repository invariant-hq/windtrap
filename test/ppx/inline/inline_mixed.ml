(* let%test and module%test under the real backend: nested groups
   register in order, and let%test / let%expect_test coexist inside a
   module%test — the enter/leave wrapping keeps module contents (here
   [answer]) in scope for the tests that follow. *)

let%test "top-level unit test" = assert (2 + 2 = 4)

module%test Arithmetic = struct
  let answer = 42
  let%test "addition reaches the answer" = assert (41 + 1 = answer)

  module%test Nested = struct
    let%test "nested group runs" = assert (String.length "ok" = 2)
  end

  let%expect_test "expect inside a module%test" =
    Printf.printf "answer %d\n" answer;
    [%expect {| answer 42 |}]
end

let%test _ = assert (List.rev [ 1; 2 ] = [ 2; 1 ])
