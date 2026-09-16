(* An in-memory stand-in for a per-test database connection. *)

type t = { mutable rows : string list; mutable open_ : bool }

let connect () = { rows = []; open_ = true }
let close db = db.open_ <- false
let insert db row = db.rows <- row :: db.rows
let count db = List.length db.rows
