(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What windtrap's own test suites share and the facade does not give them:
    scratch directories outside a run, and child processes started in a stated
    environment. Never installed. *)

(** {1:scratch Scratch directories} *)

module Scratch : sig
  val dir : string -> string
  (** [dir prefix] is a new directory in the system's temporary directory, its
      name starting with [prefix]. It and everything under it are removed when
      the process that called [dir] exits; a forked child removes nothing. The
      removal is one [at_exit] function registered as this library initialises,
      before any run, so an [exit] that a run intercepts removes nothing. *)

  val remove_tree : string -> unit
  (** [remove_tree path] removes [path] and everything under it. It never
      follows a symbolic link, and a missing [path] is no error. *)
end

(** {1:children Child processes} *)

module Child : sig
  val environment : (string * string) list -> string array
  (** [environment bindings] is the whole environment of a child: [PATH],
      [HOME], [TMPDIR], [TEMP], [TMP], [SYSTEMROOT], [LANG] and [LC_ALL] as this
      process has them, [WINDTRAP_COLOR=never], then [bindings]. A binding
      replaces a name given before it. Nothing else of this process's
      environment reaches the child, so a variable set in a developer's shell
      never changes what a test sees. *)

  type result = {
    status : Unix.process_status;
    out : string;  (** Everything the child wrote to standard output. *)
    err : string;  (** Everything the child wrote to standard error. *)
  }

  val run :
    ?cwd:string ->
    ?env:(string * string) list ->
    string ->
    string list ->
    result
  (** [run ?cwd ?env exe args] runs [exe] with [args] in [environment env]
      ([env] defaults to [[]]), from [cwd] (default: this process's working
      directory), and waits for it. Its standard input is empty; its two output
      streams are kept apart. [exe] is a path, not looked up in [PATH]. Raises
      [Unix.Unix_error] if the child cannot be started. *)

  val exit_code : result -> int
  (** [exit_code r] is the code [r]'s child exited with. Raises
      [Invalid_argument] if it was killed or stopped by a signal. *)
end
