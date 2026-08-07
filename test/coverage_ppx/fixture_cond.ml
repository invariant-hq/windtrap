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

(* Right arms that are not applications. Each shape below inherits tail
   position in its own sub-expressions, so the recursive call inside is a
   tail call; the arm keeps its position and gives up its point, exactly as
   a bare application does. The expansion must show the arm as the [else]
   branch with no [___windtrap_post_visit___] around the call — demoting it
   to an [if] condition would wrap it and cost the tail call. *)
let rec or_let n =
  n = 0
  ||
  let next = n - 1 in
  or_let next

let rec or_match n = n = 0 || match n with k -> or_match (k - 1)
let rec or_if n = n = 0 || if n > 0 then or_if (n - 1) else false
