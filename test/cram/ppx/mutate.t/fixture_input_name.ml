(* M46, mut:223-224: under its own name this file is mutated. The rules
   input_name_* of ./dune give it the input names [//toplevel//],
   [(stdin)], [.ocamlinit] and [topfind] instead, and each returns it as
   parsed (input_name_ignored.expected). *)

let sign n = if n > 0 then 1 else 0
