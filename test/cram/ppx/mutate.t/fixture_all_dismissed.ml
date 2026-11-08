(* Every site of the file dismissed: a dismissed expression records no
   site, so the file records none and its expansion is the source as
   written, with no generated module and no registration. The catalogue
   then holds nothing of the file, and an identifier of it is
   uncatalogued. *)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else (16 [@mutate off "a capacity"])
