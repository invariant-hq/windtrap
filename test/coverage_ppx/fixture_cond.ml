(* [&&]/[||] condition arms (Law 14 as amended). [a || b] desugars into
   nested ifs whose marks fire when an arm returns true; [a && b] marks
   [b]'s entry (it runs only when [a] was true). A right arm that is a
   non-trivial call in tail position keeps its tail call and gives up its
   point instead (the donor guard - the semantics suite pins the deep
   recursion). *)

let both x y = x && y
let either x y = x || y
let chain a b c = a || b || c
let rec search p = function [] -> false | x :: rest -> p x || search p rest

(* Right arms that are not applications: one function per shape the
   instrumenter's tail guard lists (let, match, if, try, sequence, open,
   letmodule, letexception, letop, constraint, coerce). Each inherits tail
   position in its own sub-expressions, so the recursive call inside is a
   tail call; the arm keeps its position and gives up its point, exactly as
   a bare application does. The expansion must show the arm verbatim as the
   [else] branch, with no [___windtrap_post_visit___] around the calls that
   sit in tail position inside it. Dropping a shape from the guard demotes
   that arm to an [if] condition — [else if <arm> then (visit k; true) else
   false] — which traverses it out of tail position and wraps the call.
   This golden is the only thing that bites: the semantics suite can only
   observe results, and OCaml 5 grows the main fibre's stack on demand, so
   a lost tail call does not reliably overflow there. *)
let rec or_let n =
  n = 0
  ||
  let next = n - 1 in
  or_let next

let rec or_match n = n = 0 || match n with k -> or_match (k - 1)
let rec or_if n = n = 0 || if n > 0 then or_if (n - 1) else false

(* [try] is the shape whose sub-expressions do not all inherit the
   position: the handler does, the body does not (its handler must stay on
   the stack), so the golden must show the handler's call bare and the
   body's post-wrapped. Both calls are here so the asymmetry is pinned. *)
let rec or_try n = n = 0 || try or_try (n - 1) with Not_found -> or_try (n - 2)

let rec or_seq n =
  n = 0
  ||
  (ignore n;
   or_seq (n - 1))

let rec or_open n =
  n = 0
  ||
  let open Stdlib in
  or_open (n - 1)

let rec or_letmodule n =
  n = 0
  ||
  let module M = Stdlib in
  or_letmodule (n - 1)

let rec or_letexception n =
  n = 0
  ||
  let exception E in
  or_letexception (n - 1)

let ( let* ) x f = f x

let rec or_letop n =
  n = 0
  ||
  let* m = n - 1 in
  or_letop m

let rec or_constraint n = n = 0 || (or_constraint (n - 1) : bool)
let rec or_coerce n = n = 0 || (or_coerce (n - 1) :> bool)
