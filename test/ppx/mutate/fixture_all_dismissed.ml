(* Every site of the file dismissed: the file still registers, and the
   catalogue holds each dismissal with its reason, but no guard refers to
   the module that binds the guard closure. That module is never opened,
   so no unused [open] can fail a library built with
   [-w +a -warn-error +a]. *)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16
