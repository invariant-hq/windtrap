(** A tiny textual histogram. *)

val render : (string * int) list -> string
(** [render rows] draws one line per [(label, count)]: the label padded to the
    widest, two spaces, then [count] ['#'] marks (negative counts clamp to
    zero). A [total N] line follows. No trailing newline. *)
