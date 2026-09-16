(* Baselines: a reviewed expectation the source names. [expect] holds it
   as a literal at the call, compared with ppx_expect's whitespace
   flexibility; [expect_file] holds it in a file named relative to the
   project root (windtrap's, since this example lives in its tree).
   Checking is read-only — a green run always means "matched the
   reviewed expectation" — and the stanza's --corrected run lets
   `dune promote` accept a change. [output ()] hands a test its captured
   output, which [expect] compares like any other text. *)

open Windtrap

let () =
  exit
  @@ run "mytool"
       [
         test "report" (fun () ->
             expect (Mytool.report ~rows:42)
             @@ __POS_OF__
                  {|
               processed 42 rows
               status: ok
               |});
         test "cli help" (fun () ->
             expect_file (Mytool.help ()) "examples/05-baselines/help.expected");
         test "greeting goes through capture" (fun () ->
             print_string "Hello, World!\n";
             expect (output ()) @@ __POS_OF__ {| Hello, World! |});
       ]
