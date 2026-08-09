(* Every site dismissed: the preamble registers the catalogue but binds
   nothing anything references, so it must not emit an [open] - warning
   33 is fatal here. *)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16
