(* Every site of the file dismissed: it still registers, so [report] mode
   can list the dismissal with its reason, but nothing references the
   guard closure - so the preamble binds it and does not open it. An
   unused [open] is a fatal warning in a library built with
   [-w +a -warn-error +a]. *)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16
