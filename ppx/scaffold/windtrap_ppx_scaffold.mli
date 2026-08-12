(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   Portions adapted from Bisect_ppx (MIT license).
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Scaffolding shared by the two whole-file instrumenters.

    The coverage and mutation instrumenters are distinct engines by design —
    coverage threads tail-position and successor context because out-edge
    wrapping can change meaning, mutation is a plain map — but everything around
    the traversal is one mechanism: the exclusion-attribute grammar, the [lazy]
    trivial-value guard, the per-file generated module, and the entry filter.
    This module is that mechanism, spelled once. Each backend instantiates the
    grammar with its own namespace ([coverage], [mutate]) and keeps its own
    region-toggle diagnostics, whose messages name the tool rather than the
    attribute. *)

(** {1:grammar The exclusion-attribute grammar}

    Both backends exclude code by the Bisect_ppx attribute spelling: [[@ns off]]
    on an expression, [[@@ns off]] on a value or module binding,
    [[@@@ns off]]/[[@@@ns on]] around a region, [[@@@ns exclude_file]] for a
    file. The mutation backend additionally admits a reason string on [off]
    ([[@mutate off "why"]]), which is what {!val:grammar}'s [reasons] switches
    on. *)

type grammar
(** The exclusion grammar of one attribute namespace. Created once per backend
    with {!val:grammar}. *)

val grammar : namespace:string -> reasons:bool -> grammar
(** [grammar ~namespace ~reasons] recognizes [[@<namespace> …]] attributes. When
    [reasons], the [off] payload may carry a reason string; otherwise only the
    bare [off], [on] and [exclude_file] payloads are well-formed. [namespace]
    also names the attribute in every diagnostic. *)

type directive =
  [ `None  (** Not an attribute of this grammar's namespace. *)
  | `Off of string  (** [`Off reason]; [reason] is [""] when none was given. *)
  | `On
  | `Exclude_file ]
(** The type for recognized attributes. *)

val recognize : grammar -> Ppxlib.attribute -> directive
(** [recognize g attr] is [attr]'s directive. Raises a located ppxlib error for
    a malformed payload on an attribute of [g]'s namespace; a foreign attribute
    is [`None], never an error. *)

val off_reason : grammar -> Ppxlib.attributes -> string option
(** [off_reason g attrs] is [Some reason] when [attrs] carries [[@ns off]].
    Folds rather than short-circuits so every attribute is error-checked; raises
    a located error as {!recognize} does, and for a misplaced [on] or
    [exclude_file]. *)

val has_off_attribute : grammar -> Ppxlib.attributes -> bool
(** [has_off_attribute g attrs] is [off_reason g attrs <> None]. *)

(** {1:filter Entry filtering} *)

val excluded_file : grammar -> file:string -> Ppxlib.structure -> bool
(** [excluded_file g ~file ast] is [true] when instrumentation must leave the
    file untouched: toplevel and ocamlinit inputs ([//toplevel//], [(stdin)],
    [.ocamlinit], [topfind]), or a top-level [[@@@ns exclude_file]] in [ast].
    The attribute scan raises as {!recognize} does — but never for an
    always-ignored [file], which is not scanned at all. *)

(** {1:guards Semantics preservation} *)

val is_trivial_syntactic_value : Ppxlib.expression -> bool
(** [is_trivial_syntactic_value e] is [true] when [lazy e] compiles as already
    forced — an identifier, constant, nullary constructor or function, through
    any [(e : t)]/[(e :> t)] wrapping. Inserting anything under such a [lazy]
    would turn the value into a thunk and change the compilation of the [lazy]
    itself, so both instrumenters leave these bodies alone. *)

(** {1:preamble The generated preamble}

    Each instrumented file is prepended one generated module that registers the
    file with its runtime at load time; the marks or guards in the file call
    into it. The module is named after the file so that each compilation unit
    calls its own — an unscoped binding could be shadowed by a later [open], and
    two files could collide when one includes another. *)

val ghost_loc : file:string -> Ppxlib.Location.t
(** [ghost_loc ~file] is a ghost location in [file], for generated code no point
    or site may attribute. *)

val mangled_module_name : prefix:string -> file:string -> string
(** [mangled_module_name ~prefix ~file] is [prefix] followed by [file] with
    every character outside [A]–[Z], [a]–[z], [0]–[9] and [_] replaced by [___].
    [prefix] must be a valid module-name prefix ([Windtrap_cov___],
    [Windtrap_mut___]). *)

val preamble :
  loc:Ppxlib.Location.t ->
  module_name:string ->
  opened:bool ->
  Ppxlib.structure ->
  Ppxlib.structure
(** [preamble ~loc ~module_name ~opened bindings] is
    [module <module_name> = struct <bindings> end] framed by
    [[@@@ocaml.text "/*"]] stop comments that hide the generated code from odoc.
    When [opened], [open <module_name>] follows the module — coverage's marks
    name the visit functions unqualified. The mutation module carries a record
    type and is referenced qualified instead, so its guards never put field
    labels in the user's scope. *)
