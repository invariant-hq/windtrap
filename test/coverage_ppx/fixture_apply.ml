(* Application out-edges (Law 14 as amended): a non-tail application is
   wrapped in [___windtrap_post_visit___], so its point fires only when
   the call returns - a raising call reports uncovered. The point's
   extent is the application; its attribution offset is the donor's (end
   of the callee, or the start of the successor expression when control
   flow makes one known). Tail calls are never wrapped. *)

let helper x = x + 1

(* The argument call is an out-edge; the outer call is in tail position. *)
let nested x = helper (helper x)

(* A single let binding attributes the out-edge to the body. *)
let bound () =
  let r = helper 1 in
  r + 1

(* [f @@ x] with an applied left side: the wrap keys on the callee. *)
let at_op x = helper @@ helper x

(* Effectful statements: each out-edge is attributed to its successor. *)
let sequenced () =
  print_string "a";
  print_string "b";
  print_newline ()
