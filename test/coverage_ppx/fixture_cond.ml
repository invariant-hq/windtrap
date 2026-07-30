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
