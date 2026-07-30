(* Compiled mirrors of the manual's *failing* walkthroughs — the transcripts
   shown in doc/manual/. This executable is built (so every snippet
   compiles) but never wired to runtest: run it by hand to regenerate a
   transcript, selecting one walkthrough with -f. *)

open Windtrap

(* getting-started.md / assertions.md: the failing equal with a diff. *)

module Sessions = struct
  let all () = [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4; 5 ]); ("carol", []) ]
end

let failing_equal =
  group "users"
    [
      test "sessions after login" (fun () ->
          let sessions = Sessions.all () in
          equal
            (list (pair string (list int)))
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
            sessions;
          equal int 3 (List.length sessions));
    ]

(* property-testing.md: the failing property and its replay line. *)

let encode fields = String.concat "," fields
let decode = function "" -> [] | s -> String.split_on_char ',' s

let failing_prop =
  prop "decode inverts encode"
    Gen.(list string)
    (fun fields ->
      equal (list string) fields (decode (encode fields));
      classify "empty" (fields = []))

(* snapshots-and-expect.md: the missing-baseline failure. *)

let help () =
  String.concat "\n"
    [
      "Usage: mytool [OPTIONS] COMMAND";
      "";
      "Commands:";
      "  build    Build the project";
      "  test     Run the tests";
    ]

let missing_snapshot =
  group "cli" [ test "cli help" (fun () -> snapshot "help" (help ())) ]

let suite = [ failing_equal; failing_prop; missing_snapshot ]
let () = run "mytool" suite
