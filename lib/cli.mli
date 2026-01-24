(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** CLI argument parsing for Windtrap test runners.

    Hand-rolled parser with no external dependencies. Supports short and long
    flags, [--key=value] syntax, and a single positional argument interpreted as
    a filter pattern. *)

(** {1 Types} *)

type t = {
  stream : bool option;  (** [-s], [--stream]: stream output to console. *)
  format : string option;
      (** [--format]: output format ([verbose], [compact], [tap], [junit]). *)
  fail_fast : bool option;  (** [-x], [--fail-fast]: stop on first failure. *)
  quick : bool option;  (** [-q], [--quick]: skip slow tests. *)
  filter : string option;
      (** [-f], [--filter], or first positional argument: test name filter. *)
  list_only : bool option;  (** [-l], [--list]: list tests without running. *)
  output_dir : string option;  (** [-o], [--output]: directory for test logs. *)
  update : bool option;  (** [-u], [--update]: update snapshots. *)
  junit : string option;  (** [--junit]: write JUnit XML to this path. *)
  seed : int option;  (** [--seed]: random seed for property tests. *)
  timeout : float option;  (** [--timeout]: default timeout in seconds. *)
}
(** Parsed CLI flags. [None] means the flag was not provided. *)

(** {1 Parsing} *)

val empty : t
(** A config with all fields set to [None]. *)

val parse : string array -> t
(** [parse argv] parses command-line arguments. Prints help and exits on
    [--help]. Exits with an error on unknown flags or duplicate positional
    arguments. *)

val print_help : string -> unit
(** [print_help prog_name] prints usage information to stdout. *)

val parse_format : string -> Progress.mode
(** [parse_format s] converts a format name to a {!Progress.mode}. Recognized
    values: ["verbose"], ["compact"], ["tap"], ["junit"]. Exits with an error
    for unknown values. *)

(** {1 Configuration Resolution} *)

val resolve_config :
  ?quick:bool ->
  ?fail_fast:bool ->
  ?output_dir:string ->
  ?stream:bool ->
  ?update:bool ->
  ?snapshot_dir:string ->
  ?filter:string ->
  ?format:Progress.mode ->
  ?junit:string ->
  ?seed:int ->
  ?timeout:float ->
  t ->
  Runner.config
(** [resolve_config ... cli] builds a {!Runner.config} by merging sources with
    the following priority (highest first):

    + Programmatic optional arguments passed to this function.
    + CLI flags from [cli].
    + [WINDTRAP_*] environment variables (see {!Env}).
    + Built-in defaults. *)
