(* Fixture: the expect-test surface. Node ids count in source order;
   payload extents include delimiters; the bare [%expect] declares no
   payload; [%expect.output] is a consuming read, not a node. *)

let greet name = Printf.printf "hello %s\n" name

let%expect_test "greetings" =
  greet "world";
  [%expect {|
    hello world
  |}];
  greet "again";
  let noise = [%expect.output] in
  Printf.printf "captured %d bytes\n" (String.length noise);
  [%expect_exact {|captured 12 bytes
|}];
  print_string "";
  [%expect]

let%expect_test _ =
  print_string "quoted";
  [%expect "quoted"]

let%expect_test ("tagged" [@tags "slow"]) =
  print_string "x";
  [%expect {x| x |x}]

(* Rules pinned here, by id in RULES.md and interface line: E1, pwt:20-23;
   E2, pwt:23-25; E5, pwt:25-26; E8, pwt:23; E9, pwt:35-38; E10, pwt:38;
   E12, pwt:39; E23, pwt:65-69. *)
