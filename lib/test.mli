(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test tree definition and traversal.

    Tests form a tree of {!test} cases and {!group} nodes. Groups can carry
    setup/teardown hooks and tags that are inherited by their children. *)

(** {1 Types} *)

type pos = string * int * int * int
(** Source position: [(filename, line, start_char, end_char)]. Use with
    [__POS__]. *)

type t
(** An abstract test or group of tests. *)

(** {1 Test Creation} *)

val test :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  t
(** [test name fn] creates a test case. The return value of [fn] is ignored.

    @param timeout
      Maximum time in seconds for the test to complete. If exceeded, the test
      fails with a timeout error.
    @param retries
      Number of retry attempts on failure (default: 0). Use sparingly for flaky
      tests. *)

val group :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?setup:(unit -> unit) ->
  ?teardown:(unit -> unit) ->
  string ->
  t list ->
  t
(** [group name children] creates a test group. Tags are inherited by children.

    @param setup Runs once before any child test.
    @param teardown Runs once after all child tests, even on failure. *)

val ftest :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  t
(** [ftest name fn] creates a focused test case. When any focused test or group
    exists in the suite, only focused tests run. A warning is printed after the
    run to remind you to remove focus markers before committing. *)

val fgroup :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?setup:(unit -> unit) ->
  ?teardown:(unit -> unit) ->
  string ->
  t list ->
  t
(** [fgroup name children] creates a focused test group. All tests inside a
    focused group are treated as focused. *)

val has_focused : t list -> bool
(** [has_focused tests] returns [true] if any test or group in the tree is
    focused. *)

val slow :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  string ->
  (unit -> 'a) ->
  t
(** [slow name fn] creates a test tagged with {!Tag.Slow}, skipped when
    [~quick:true] is passed to the runner. The return value of [fn] is ignored.
*)

val bracket :
  ?pos:pos ->
  ?tags:Tag.t ->
  ?timeout:float ->
  ?retries:int ->
  setup:(unit -> 'a) ->
  teardown:('a -> unit) ->
  string ->
  ('a -> 'b) ->
  t
(** [bracket ~setup ~teardown name fn] creates a test with resource management.
    [setup] runs before the test to acquire a resource, which is passed to [fn].
    [teardown] runs after the test completes (even on failure) to release the
    resource.

    Partial application creates reusable test constructors:
    {[
      let with_db = bracket ~setup:connect_db ~teardown:close_db

      group "Database" [
        with_db "query works" (fun db -> ...);
        with_db "insert works" (fun db -> ...);
      ]
    ]} *)

(** {1 Test Traversal}

    Traverse the test tree without exposing the internal representation. *)

(** Events emitted during test tree traversal. *)
type visit =
  | Case of {
      name : string;
      fn : unit -> unit;
      pos : pos option;
      tags : Tag.t;
      timeout : float option;
      retries : int;
      focused : bool;
    }  (** A leaf test case. *)
  | Enter_group of {
      name : string;
      pos : pos option;
      tags : Tag.t;
      setup : (unit -> unit) option;
      focused : bool;
    }  (** Entering a group node, before visiting children. *)
  | Leave_group of { name : string; teardown : (unit -> unit) option }
      (** Leaving a group node, after all children have been visited. *)

val fold : ('a -> visit -> 'a) -> 'a -> t -> 'a
(** [fold f init t] traverses the test tree in depth-first order. For each test
    case, calls [f] with [Case]. For groups, calls [f] with [Enter_group] before
    children and [Leave_group] after. *)

val iter : (visit -> unit) -> t -> unit
(** [iter f t] traverses like {!fold} but discards the accumulator. *)
