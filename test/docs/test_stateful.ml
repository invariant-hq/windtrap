(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The compiled mirror of doc/manual/stateful-testing.md: every call site
   the chapter shows appears here in the chapter's order and shape, so a
   snippet that rots breaks the build. The systems under test are the
   chapter's, inlined. A plain windtrap suite: this executable is a runtest
   test and must exit 0, so the chapter's *failing* walkthroughs are not run
   here — the bounded queue is the fixed one, and the raising [~pre] is
   declared but never executed (doc/manual/snippets/transcript_fail.ml runs
   the broken versions and regenerates the transcripts). *)

open Windtrap

(* ───── the worked example ───── *)

module Bounded_queue = struct
  exception Full
  exception Empty

  type t = {
    data : int array;
    capacity : int;
    mutable head : int;
    mutable size : int;
  }

  let create capacity =
    { data = Array.make capacity 0; capacity; head = 0; size = 0 }

  let size q = q.size

  let push q x =
    if q.size = q.capacity then raise Full;
    q.data.((q.head + q.size) mod q.capacity) <- x;
    q.size <- q.size + 1

  let pop q =
    if q.size = 0 then raise Empty;
    let x = q.data.(q.head) in
    q.head <- (q.head + 1) mod q.capacity;
    q.size <- q.size - 1;
    x

  let peek q = if q.size = 0 then raise Empty else q.data.(q.head)
end

let capacity = 4

(* The model: the elements the queue should hold, oldest first. *)
type model = int list

let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal int (List.hd m) (Bounded_queue.pop q));
    call "peek"
      ~pre:(fun m -> m <> [])
      ~next:Fun.id
      (fun m q -> equal int (List.hd m) (Bounded_queue.peek q));
    call "push when full"
      ~pre:(fun m -> List.length m = capacity)
      ~next:Fun.id
      (fun _ q -> raises Bounded_queue.Full (fun () -> Bounded_queue.push q 0));
  ]

let queue_test =
  stateful "behaves like a list" ~model:[]
    ~scope:(fun run -> run (Bounded_queue.create capacity))
    ~pp_model:(Testable.pp (list int))
    ~invariant:(fun m q -> equal int (List.length m) (Bounded_queue.size q))
    commands

(* ───── preconditions filter, and select ───── *)

(* The chapter's [cover] recipe, verbatim and with the chapter's threshold:
   the whole point of it is that the figure is a test outcome, so the mirror
   runs it rather than only compiling it. The label is marked from the
   invariant, which runs on every case — a [cover] in the body of the
   command whose precondition is in question registers nothing on the runs
   where that command is never drawn. The measured rate is ~30%. Only the
   test name differs from the chapter's, which reuses the worked example's. *)
let cover_test =
  stateful "behaves like a list, and reaches capacity" ~model:[]
    ~scope:(fun run -> run (Bounded_queue.create capacity))
    ~pp_model:(Testable.pp (list int))
    ~invariant:(fun m q ->
      cover "reached capacity" (List.length m = capacity);
      equal int (List.length m) (Bounded_queue.size q))
    commands

(* ───── shrinking, and what a command may depend on ───── *)

module Pool = struct
  type t = (int, Buffer.t) Hashtbl.t

  let create () : t = Hashtbl.create 8
  let live pool = Hashtbl.length pool
  let open_ pool id = Hashtbl.replace pool id (Buffer.create 16)
  let write pool id data = Buffer.add_string (Hashtbl.find pool id) data
  let close pool id = Hashtbl.remove pool id
end

type pool_model = { live : int list; next_id : int }

(* An index into the live set, not an absolute handle: it stays
   meaningful however many handles have been opened and closed. *)
let slot = Gen.int_range 0 3

let pool_commands =
  [
    call "open"
      ~pre:(fun m -> List.length m.live < 4)
      ~next:(fun m ->
        { live = m.live @ [ m.next_id ]; next_id = m.next_id + 1 })
      (fun m pool -> Pool.open_ pool m.next_id);
    command "write"
      (Gen.pair slot (Gen.string_of (Gen.char_range 'a' 'z')))
      ~pre:(fun m (i, _) -> i < List.length m.live)
      ~next:Fun.const
      (fun m (i, data) pool -> Pool.write pool (List.nth m.live i) data);
    command "close" slot
      ~pre:(fun m i -> i < List.length m.live)
      ~next:(fun m i ->
        { m with live = List.filteri (fun j _ -> j <> i) m.live })
      (fun m i pool -> Pool.close pool (List.nth m.live i));
  ]

let pool_test =
  stateful "handles stay live" ~model:{ live = []; next_id = 0 }
    ~scope:(fun run -> run (Pool.create ()))
    ~invariant:(fun m pool -> equal int (List.length m.live) (Pool.live pool))
    pool_commands

(* ───── the system under test ───── *)

module Store = struct
  type t = string

  let open_ dir : t = dir
  let close _ = ()
  let path store key = Filename.concat store (string_of_int key)

  let put store key value =
    Out_channel.with_open_text (path store key) (fun oc ->
        Out_channel.output_string oc value)

  let get store key =
    In_channel.with_open_text (path store key) In_channel.input_all
end

module Store_model = struct
  type t = (int * string) list

  let empty : t = []
end

let rec rm_rf path =
  if Sys.is_directory path then begin
    Array.iter
      (fun name -> rm_rf (Filename.concat path name))
      (Sys.readdir path);
    Sys.rmdir path
  end
  else Sys.remove path

let store_commands =
  [
    command "put"
      (Gen.pair (Gen.int_range 0 5) (Gen.string_of (Gen.char_range 'a' 'z')))
      ~next:(fun m (k, v) -> (k, v) :: List.remove_assoc k m)
      (fun _ (k, v) store -> Store.put store k v);
    command "get" (Gen.int_range 0 5)
      ~pre:(fun m k -> List.mem_assoc k m)
      ~next:Fun.const
      (fun m k store -> equal string (List.assoc k m) (Store.get store k));
  ]

(* The chapter's ~scope shape, acquisition and release in one call.
   [~count] and [~steps] are the mirror's own: the chapter shows the
   lifecycle, and this suite pays a directory and a file per case for
   it. *)
let store_test =
  stateful "store survives any sequence" ~count:20 ~steps:8
    ~model:Store_model.empty
    ~scope:(fun run ->
      let dir = Filename.temp_file "store-" ".dir" in
      Sys.remove dir;
      Sys.mkdir dir 0o700;
      let store = Store.open_ dir in
      Fun.protect
        ~finally:(fun () ->
          Store.close store;
          rm_rf dir)
        (fun () -> run store))
    store_commands

(* ───── when the specification itself raises ───── *)

(* Declared, never run: executing it is the poisoned-program failure the
   chapter shows. [List.nth] raises when the slot is past the live set. *)
let _raising_pre : (pool_model, Pool.t) command =
  command "close" slot
    ~pre:(fun m i -> List.nth m.live i >= 0)
    ~next:(fun m i -> { m with live = List.filteri (fun j _ -> j <> i) m.live })
    (fun m i pool -> Pool.close pool (List.nth m.live i))

(* ───── notes ───── *)

(* "[stateful] with an empty command list fails the test with
   [Invalid_argument] at its first case, rather than passing vacuously" —
   held here by [xfail], which inverts the outcome without hiding it. *)
let empty_commands =
  xfail ~reason:"the chapter's empty-command-list note"
    (stateful "no commands" ~model:() ~scope:(fun run -> run ()) [])

let () =
  run "stateful-testing"
    [ queue_test; cover_test; pool_test; store_test; empty_commands ]
