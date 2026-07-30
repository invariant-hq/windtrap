(* A tour of the assertion vocabulary: the everyday verbs, flat testable
   instances, Testable.make for custom types, and table-driven [cases]. *)

open Windtrap

(* A tiny domain to assert against. *)

type point = { x : int; y : int }

let pp_point ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
let point = Testable.make ~pp:pp_point ~equal:( = )
let find_user = function "alice" -> Some 1 | _ -> None

let parse_port input =
  match int_of_string_opt input with
  | Some port when port > 0 && port < 65_536 -> Ok port
  | Some _ | None -> Error ("invalid port: " ^ input)

let () =
  run "assertions"
    [
      test "equal composes witnesses" (fun () ->
          equal
            (list (pair string (list int)))
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]);
      test "not_equal" (fun () -> not_equal int 1 2);
      test "is_true / is_false" (fun () ->
          is_true (1 < 2);
          is_false (2 < 1));
      test "require_some asserts and unwraps" (fun () ->
          let id = require_some (find_user "alice") in
          equal int 1 id);
      test "require_ok / require_error" (fun () ->
          let port = require_ok (parse_port "8080") in
          equal int 8080 port;
          let message = require_error (parse_port "0") in
          equal string "invalid port: 0" message);
      test "raises" (fun () -> raises Exit (fun () -> raise Exit));
      test "raises_match" (fun () ->
          raises_match
            (function Invalid_argument _ -> true | _ -> false)
            (fun () -> invalid_arg "boom"));
      test "fail marks unreachable branches" (fun () ->
          match find_user "alice" with
          | Some _ -> ()
          | None -> fail "alice must exist");
      test "skip under unmet preconditions" (fun () ->
          if Sys.win32 then skip ~reason:"unix only" ());
      test "custom testables print like their pp" (fun () ->
          equal point { x = 1; y = 2 } { x = 1; y = 2 });
      test "float takes an absolute tolerance" (fun () ->
          equal (float 1e-9) 0.3 (0.1 +. 0.2));
      test "slist compares as a multiset" (fun () ->
          equal (slist int compare) [ 3; 1; 2 ] [ 1; 2; 3 ]);
      test "contramap projects before comparing" (fun () ->
          let by_length = contramap String.length int in
          equal by_length "abc" "xyz");
      cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
        (fun input -> ignore (require_ok (parse_port input)));
    ]
