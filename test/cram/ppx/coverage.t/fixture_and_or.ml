(* [&&]/[||] condition arms. [a || b] desugars into
   nested ifs whose marks fire when an arm returns true; [a && b] marks
   [b]'s entry (it runs only when [a] was true). A right arm that is a
   non-trivial call in tail position keeps its tail call and gives up its
   point instead (the donor guard - the semantics suite pins the deep
   recursion). The right arms of [||] that are not applications are in
   fixture_or_tail_*.ml. *)

let both x y = x && y
let either x y = x || y
let chain a b c = a || b || c
let rec search p = function [] -> false | x :: rest -> p x || search p rest

(* A right arm that calls a function that never returns is never true: it
   stays the [else] branch with no point, in tail position and bound by a
   [let]. *)
let positive x = x > 0 || failwith "positive"

let bound x =
  let ok = x > 0 || raise Exit in
  ok

(* The same through [@@] and [|>]. *)
let through x =
  let applied = x > 0 || raise @@ Exit and piped = x < 10 || Exit |> raise in
  (applied, piped)
