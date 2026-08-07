(* The payload-shape matrix under the real backend: every [%expect]
   spelling the PPX records must round-trip through the runtime's
   matcher without churn. Payloads are formatted exactly as a
   correction would write them, so a promote of any of these tests is
   a no-op — the transient promote-loop check relies on that. Two
   exceptions exercise matcher-accepted spellings that are not the
   writer's fixed point ({||} and bare [%expect]): they match and
   never correct on their own, but a correction elsewhere in this file
   standardizes them to {| |} on promote (ppx_expect's corrected-file
   style, pinned by the conformance corpus), so the byte-exact
   promote-loop check breaks a payload in inline_mixed.ml instead. *)

let%expect_test "multiline payload" =
  print_string "alpha\nbeta\ngamma\n";
  [%expect {|
    alpha
    beta
    gamma
    |}]

let%expect_test "relative indentation is preserved" =
  print_string "outer\n  inner\nouter\n";
  [%expect {|
    outer
      inner
    outer
    |}]

let%expect_test "empty literal payload" =
  print_string "";
  [%expect {| |}]

let%expect_test "empty braces literal" =
  print_string "";
  [%expect {||}]

let%expect_test "bare expect" =
  print_string "";
  [%expect]

let%expect_test "single-line payload" =
  print_string "one line";
  [%expect {| one line |}]

let%expect_test "quoted payload" =
  print_string "quoted";
  [%expect "quoted"]

let%expect_test "exact payload keeps whitespace" =
  Printf.printf "no trailing newline";
  [%expect_exact {|no trailing newline|}]

let%expect_test "output is consumed, not matched" =
  print_string "abcdef";
  let captured = [%expect.output] in
  Printf.printf "%d bytes\n" (String.length captured);
  [%expect {| 6 bytes |}]

let%expect_test "sanitize applies ambient config" =
  print_string "plain";
  [%expect {| plain |}]

(* Bodies whose TAIL swallows a [;]. A trailing correction sequences [;]
   onto the body, and after a [match], [try] or [function] that [;] binds
   to the last arm — the inserted node would land inside the arm, so the
   promoted file would mean something else and the correction would never
   converge, appending one more dead node per round.

   The hazard belongs to what the body ends with, not what it starts with,
   so all four shapes below need the parentheses: a body that merely ends
   in a match is the common case ([let ... in match], [stmt; match]) and
   was the one the first version of this guard missed. They are shown
   already promoted; the promote-loop check in this directory rewrites
   them from scratch. *)
let%expect_test "bare match body takes parentheses" =
  (match Some 1 with
  | Some n -> Printf.printf "got %d\n" n
  | None -> print_string "none\n");
  [%expect {| got 1 |}]

let%expect_test "a let ending in a match takes them too" =
  (let x = Some 2 in
   match x with
   | Some n -> Printf.printf "let %d\n" n
   | None -> print_string "none\n");
  [%expect {| let 2 |}]

let%expect_test "a sequence ending in a match takes them too" =
  (print_string "before\n";
   match Some 3 with
   | Some n -> Printf.printf "seq %d\n" n
   | None -> print_string "none\n");
  [%expect {|
    before
    seq 3
    |}]

let%expect_test "a body ending in a try takes them too" =
  (try raise Not_found with Not_found -> print_string "caught\n");
  [%expect {| caught |}]

(* And a body that cannot swallow the [;] is left alone — no parentheses
   appear here, which is what keeps the common shape readable. *)
let%expect_test "an ordinary body keeps its shape" =
  let s = "plain\n" in
  print_string s;
  [%expect {| plain |}]
