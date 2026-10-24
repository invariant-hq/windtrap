(* A queue meant to be shared between domains: every operation holds the
   queue's lock. *)

type 'a t = {
  lock : Mutex.t;
  mutable front : 'a list;
  mutable back : 'a list; (* newest first *)
  mutable length : int;
}

let create () = { lock = Mutex.create (); front = []; back = []; length = 0 }
let locked q f = Mutex.protect q.lock f

let push q x =
  locked q (fun () ->
      q.back <- x :: q.back;
      q.length <- q.length + 1)

let pop_opt q =
  locked q (fun () ->
      match q.front with
      | x :: rest ->
          q.front <- rest;
          q.length <- q.length - 1;
          Some x
      | [] -> (
          match List.rev q.back with
          | [] -> None
          | x :: rest ->
              q.front <- rest;
              q.back <- [];
              q.length <- q.length - 1;
              Some x))

let length q = locked q (fun () -> q.length)
