(* A tour of the assertion vocabulary (doc/manual/assertions.md): equality
   through flat witnesses and Testable.make, the ordering verbs, the
   require_* verbs that assert and unwrap, predicates and containment, the
   Exn predicates, the escape hatches, and table-driven [cases]. Every test
   here passes; the chapter shows what the same verbs print when they
   fail. *)

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

type addr = Tcp of int | Unix_socket of string

let tcp_port = function Tcp port -> Some port | Unix_socket _ -> None
let resolve = function "db" -> Tcp 5432 | sock -> Unix_socket sock
let render_log user = Printf.sprintf "user=%s token=REDACTED\n" user

let checkout ~items ~coupon =
  if coupon < 0 then invalid_arg "checkout: negative coupon"
  else List.fold_left ( + ) (-coupon) items

(* Event logs: compare on a key, in any order, ignoring the noisy field. *)
type event = { path : string; kind : string; timestamp : float }

let key e = (e.path, e.kind)
let event = Testable.contramap key (pair string string)
let events = slist event (fun a b -> compare (key a) (key b))

(* A module with the conventional trio; with_compare completes it, so the
   ordering verbs accept the witness. *)
module Version = struct
  type t = int * int

  let make major minor = (major, minor)
  let pp ppf (major, minor) = Format.fprintf ppf "%d.%d" major minor
  let equal = ( = )
  let compare = Stdlib.compare
  let of_string s = Scanf.sscanf s "%d.%d" (fun major minor -> (major, minor))
end

let version =
  Testable.make ~pp:Version.pp ~equal:Version.equal
  |> Testable.with_compare Version.compare

let () =
  exit
  @@ run "assertions"
       [
         group "equality"
           [
             test "equal composes witnesses" (fun () ->
                 equal
                   (list (pair string (list int)))
                   [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
                   [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]);
             test "not_equal" (fun () -> not_equal int 1 2);
             test "custom testables print like their pp" (fun () ->
                 equal point { x = 1; y = 2 } { x = 1; y = 2 });
             test "text diffs multi-line strings line by line" (fun () ->
                 equal text "{\n  \"version\": \"1.2.0\"\n}\n"
                   "{\n  \"version\": \"1.2.0\"\n}\n");
             test "float takes an absolute tolerance" (fun () ->
                 equal (float 1e-9) 0.3 (0.1 +. 0.2));
             test "float_exact can assert NaN" (fun () ->
                 equal float_exact Float.nan (0. /. 0.));
             test "slist compares as a multiset" (fun () ->
                 equal (slist int compare) [ 3; 1; 2 ] [ 1; 2; 3 ]);
             test "contramap projects before comparing" (fun () ->
                 let by_length = Testable.contramap String.length int in
                 equal by_length "abc" "xyz");
             test "slist over contramap ignores order and noisy fields"
               (fun () ->
                 equal events
                   [
                     { path = "a"; kind = "created"; timestamp = 0. };
                     { path = "b"; kind = "removed"; timestamp = 0. };
                   ]
                   [
                     { path = "b"; kind = "removed"; timestamp = 17.3 };
                     { path = "a"; kind = "created"; timestamp = 42.1 };
                   ]);
           ];
         group "orders"
           [
             test "ordering verbs keep the bound and the value" (fun () ->
                 let retries () = 2 in
                 less int ~than:3 (retries ());
                 at_least (float 1e-6) ~than:0.4 0.41);
             test "a range is two ordering assertions" (fun () ->
                 let v = 50. in
                 greater (float 1e-9) ~than:30. v;
                 less (float 1e-9) ~than:70. v);
             test "with_compare completes the trio" (fun () ->
                 at_least version ~than:(Version.make 1 2)
                   (Version.of_string "1.4"));
           ];
         group "unwrap"
           [
             test "require_some asserts and unwraps" (fun () ->
                 let id = require_some (find_user "alice") in
                 equal int 1 id);
             test "require_ok / require_error" (fun () ->
                 let port = require_ok (parse_port "8080") in
                 equal int 8080 port;
                 let message = require_error (parse_port "0") in
                 equal string "invalid port: 0" message);
             test "require_match asserts a constructor and unwraps" (fun () ->
                 let port = require_match tcp_port (resolve "db") in
                 equal int 5432 port);
             test "shape verbs take a printer, not a witness" (fun () ->
                 is_none (find_user "nobody");
                 is_some (find_user "alice");
                 is_ok (parse_port "8080");
                 is_error ~pp:Format.pp_print_int (parse_port "0"));
           ];
         group "predicates and containment"
           [
             test "is_true / is_false" (fun () ->
                 is_true (1 < 2);
                 is_false (2 < 1));
             test "satisfies names the predicate and prints the value"
               (fun () ->
                 (* on failure: the rejected value, rendered by the
                    witness, not a bare [false]. *)
                 satisfies ~msg:"positive" int
                   (fun n -> n > 0)
                   (checkout ~items:[ 3; 4 ] ~coupon:2));
             test "satisfies ~claim replaces the sentence" (fun () ->
                 satisfies ~claim:"a power of two" int
                   (fun n -> n land (n - 1) = 0)
                   16);
             test "contains / not_contains excerpt the haystack" (fun () ->
                 let log = render_log "alice" in
                 contains ~sub:"user=alice" log;
                 not_contains ~sub:"secret" log);
             test "in_order asserts a chain of substrings" (fun () ->
                 let log = "connect authenticate send disconnect" in
                 in_order ~subs:[ "connect"; "authenticate"; "disconnect" ] log);
             test "starts_with / ends_with demand a position" (fun () ->
                 let path = "sessions/ghost/session.json" in
                 starts_with ~affix:"sessions/" path;
                 ends_with ~affix:".json" path);
             test "mem is membership through a witness" (fun () ->
                 mem int 3 [ 2; 3; 5 ]);
           ];
         group "exceptions"
           [
             test "raises compares structurally" (fun () ->
                 raises Exit (fun () -> raise Exit));
             test "Exn predicates classify exceptions by message" (fun () ->
                 raises_match (Exn.invalid_arg ~substring:"negative coupon")
                   (fun () -> checkout ~items:[ 3 ] ~coupon:(-1)));
             test "raises_match takes any predicate" (fun () ->
                 raises_match
                   (function Invalid_argument _ -> true | _ -> false)
                   (fun () -> invalid_arg "boom"));
           ];
         group "escape hatches"
           [
             test "fail marks unreachable branches" (fun () ->
                 match find_user "alice" with
                 | Some _ -> ()
                 | None -> fail "alice must exist");
             test "skip under unmet preconditions" (fun () ->
                 if Sys.win32 then skip ~reason:"unix only" ());
           ];
         cases "ports parse" ~name:Fun.id [ "1"; "80"; "8080"; "65535" ]
           (fun input -> ignore (require_ok (parse_port input)));
       ]
