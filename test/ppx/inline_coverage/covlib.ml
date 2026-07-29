(* Instrumented fixture: [choose]'s [false] branch is deliberately never
   exercised by the inline test, so the coverage report has an uncovered
   region to name. *)

let choose flag = if flag then "yes" else "no"
