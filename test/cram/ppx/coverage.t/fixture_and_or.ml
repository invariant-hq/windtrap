(* [&&]/[||] condition arms (Law 14 as amended). [a || b] desugars into
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

(* Rules pinned here, by id in RULES.md and interface line: C20, cov:47;
   C22, cov:48-50; C24, cov:48-50; C25, cov:59-62; C50, cov:109-110;
   C57, cov:106-107. *)
