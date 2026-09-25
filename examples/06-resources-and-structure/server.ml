type t = { mutable running : bool; mutable sessions : int }
type session = { id : int }

let start () = { running = true; sessions = 0 }
let stop server = server.running <- false
let ping server = server.running
let reindex server = server.running
let backup server = server.running

let with_session server f =
  server.sessions <- server.sessions + 1;
  Fun.protect
    ~finally:(fun () -> server.sessions <- server.sessions - 1)
    (fun () -> f { id = server.sessions })

let session_id session = session.id
