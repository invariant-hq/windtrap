open Windtrap

let report_counts_the_rows () =
  expect (Mytool.report ~rows:42)
  @@ __POS_OF__ {|
    processed 42 rows
    status: ok
    |}

let messages =
  group "messages"
    [
      test "the report counts the rows" report_counts_the_rows;
      test "the help lists the commands" (fun () ->
          expect_file (Mytool.help ()) "examples/05-baselines/help.expected");
      test "the greeting names the user" (fun () ->
          Mytool.greet "Ada";
          expect (output ()) @@ __POS_OF__ {| Hello, Ada! |});
    ]

let () = exit (run "mytool" [ messages ])
