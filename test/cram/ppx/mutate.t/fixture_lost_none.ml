(* A file that declares a constructor named [Some] or [None] loses [none],
   since its guard names [Stdlib.Option.None] beside the file's [Some]. An
   exception counts as a declaration. The other rewrites are untouched. *)

exception None

let wrap x = Some (x + 1)
