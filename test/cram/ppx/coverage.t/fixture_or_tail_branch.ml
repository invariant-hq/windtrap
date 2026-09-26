(* Right arms of [||] in tail position that are not applications, part
   one: the branching forms ([match], [if], [try]). Each inherits tail
   position in its own sub-expressions, so the recursive call inside is a
   tail call; the arm keeps its position and gives up its point, exactly
   as a bare application does. The expansion must show the arm verbatim as
   the [else] branch, with no [___windtrap_post_visit___] around the calls
   that sit in tail position inside it. Dropping a shape from the guard
   demotes that arm to an [if] condition, [else if <arm> then (visit k;
   true) else false], which traverses it out of tail position and wraps
   the call. The semantics suite runs [match] and [if] deep under a
   bounded stack; this golden pins every shape. *)

let rec or_match n = n = 0 || match n with k -> or_match (k - 1)
let rec or_if n = n = 0 || if n > 0 then or_if (n - 1) else false

(* [try] is the shape whose sub-expressions do not all inherit the
   position: the handler does, the body does not (its handler must stay on
   the stack), so the golden must show the handler's call bare and the
   body's post-wrapped. Both calls are here so the asymmetry is pinned. *)
let rec or_try n = n = 0 || try or_try (n - 1) with Not_found -> or_try (n - 2)
