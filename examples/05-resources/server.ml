(* An in-memory stand-in for a shared server process. *)

type t = { mutable running : bool }

let start () = { running = true }
let stop server = server.running <- false
let ping server = server.running
