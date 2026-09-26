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
      cases "the greeting names the user" ~name:fst
        [
          ("Ada", __POS_OF__ {| Hello, Ada! |});
          ("Grace", __POS_OF__ {| Hello, Grace! |});
        ]
        (fun (name, greeting) ->
          Mytool.greet name;
          expect (output ()) greeting);
    ]

let () = exit (run "mytool" [ messages ])
