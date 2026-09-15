(* An in-memory stand-in for a shared server process, with a session API of
   the callback-shaped kind: a session exists only inside [with_session]. *)

type t = { mutable running : bool; mutable open_sessions : int }
type session = { server : t; id : int }

let start () = { running = true; open_sessions = 0 }
let stop server = server.running <- false
let ping server = server.running

(* A long-running maintenance call: the example tags its test ["slow"]. *)
let reindex server = server.running

let with_session server k =
  server.open_sessions <- server.open_sessions + 1;
  let session = { server; id = server.open_sessions } in
  Fun.protect
    ~finally:(fun () -> server.open_sessions <- server.open_sessions - 1)
    (fun () -> k session)

let session_id session = session.id
