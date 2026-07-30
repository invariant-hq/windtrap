(* Windtrap-authored shim (not upstream source): the minimal slice of
   Core that hello_async.ml's body uses, so the byte-identical upstream
   fixture typechecks up to the point mechanism (b) must reject. *)

module List = struct
  let iter l ~f = Stdlib.List.iter f l
end

let print_string = Stdlib.print_string
