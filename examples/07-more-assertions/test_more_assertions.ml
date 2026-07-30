(* Beyond equality: [satisfies] and [contains] make predicate and
   containment claims that print their data, [require_match] unwraps past
   options, the [Exn] predicates classify exceptions, [subtest] names
   sub-cases that keep running past a failing sibling, and [xfail] keeps a
   known-bug reproduction in-tree without a red run. *)

open Windtrap

(* A tiny domain to assert against. *)

type addr = Tcp of int | Unix_socket of string

let resolve = function
  | "db" -> Tcp 5432
  | "http" -> Unix_socket "/run/http.sock" (* issue #42: should be Tcp 8080 *)
  | sock -> Unix_socket sock

let render_log user = Printf.sprintf "user=%s token=REDACTED\n" user

let checkout ~items ~coupon =
  if coupon < 0 then invalid_arg "checkout: negative coupon"
  else List.fold_left ( + ) (-coupon) items

let backends = [ ("list", 12); ("array", 12); ("bigarray", 12) ]
let tcp_port = function Tcp port -> Some port | Unix_socket _ -> None

let () =
  run "more-assertions"
    [
      test "satisfies names the predicate and prints the value" (fun () ->
          (* on failure: the rejected value, rendered by the testable —
             not a bare [false]. *)
          satisfies ~msg:"positive" int
            (fun n -> n > 0)
            (checkout ~items:[ 3; 4 ] ~coupon:2));
      test "contains / not_contains excerpt the haystack" (fun () ->
          let log = render_log "alice" in
          contains ~sub:"user=alice" log;
          not_contains ~sub:"secret" log);
      test "require_match asserts a constructor and unwraps" (fun () ->
          let port = require_match tcp_port (resolve "db") in
          equal int 5432 port);
      test "Exn predicates classify exceptions by message" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"negative coupon") (fun () ->
              checkout ~items:[ 3 ] ~coupon:(-1)));
      test "subtest runs every backend, even past a failure" (fun () ->
          (* a failing sub-case is recorded as "… › <name>" and its
             siblings still run; the test fails at the end with every
             recorded entry. *)
          List.iter
            (fun (name, count) -> subtest name (fun () -> equal int 12 count))
            backends);
      xfail ~reason:"issue #42: http resolves to a socket, not a port"
        (test "http resolves to its TCP port" (fun () ->
             equal int 8080 (require_match tcp_port (resolve "http"))));
    ]
