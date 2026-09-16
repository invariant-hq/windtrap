(* Class bodies (Law 14 as amended): concrete method bodies,
   initializers, and optional-argument defaults are entry points; a
   virtual method has no body to mark. Method calls are out-edges - a
   [send] with a known successor is attributed to it, one in tail
   position stays unwrapped. *)

class counter ?(step = 1) () =
  object
    val mutable n = 0
    method bump = n <- n + step
    method value = n
    initializer n <- 0
  end

let use () =
  let c = new counter () in
  c#bump;
  c#value
