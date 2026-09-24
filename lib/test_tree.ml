(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tags *)

module Tag = struct
  module String_set = Set.Make (String)

  type t = String_set.t

  let empty = String_set.empty
  let of_list = String_set.of_list
  let union = String_set.union
  let mem = String_set.mem

  (* Well-known tags: "slow" is an ordinary tag pre-applied by the [slow]
     constructor and dropped like any other. *)
  let slow = "slow"
  let prop = "prop"

  type predicate = { required : String_set.t; dropped : String_set.t }

  let any = { required = String_set.empty; dropped = String_set.empty }

  let require name p =
    {
      required = String_set.add name p.required;
      dropped = String_set.remove name p.dropped;
    }

  let drop name p =
    {
      required = String_set.remove name p.required;
      dropped = String_set.add name p.dropped;
    }

  let accepts p tags =
    String_set.subset p.required tags
    && String_set.is_empty (String_set.inter p.dropped tags)
end

type body =
  | Body of (unit -> unit)
  | Scoped : { scope : ('r -> unit) -> unit; body : 'r -> unit } -> body

type xfail = { reason : string option }

(* What a node declares of its own. Ancestors' values are applied at
   [flatten] time, innermost wins; [None] is "not declared here", which is
   what lets an enclosing group's default reach the node. *)
type annotations = {
  tags : Tag.t;
  focused : bool;
  timeout : float option;
  retries : int option;
  xfail : xfail option;
}

type t =
  | Test of {
      name : string;
      body : body;
      loc : Loc.t option;
      annotations : annotations;
    }
  | Group of {
      name : string;
      children : t list;
      loc : Loc.t option;
      annotations : annotations;
    }

(* Declaration *)

let check_timeout = function
  | None -> ()
  | Some seconds ->
      if (not (Float.is_finite seconds)) || seconds <= 0. then
        invalid_arg "windtrap: timeout must be finite and positive"

let check_retries = function
  | None -> ()
  | Some retries ->
      if retries < 0 then invalid_arg "windtrap: retries must be non-negative"

let declared ?(tags = []) ?timeout ?retries () =
  check_timeout timeout;
  check_retries retries;
  { tags = Tag.of_list tags; focused = false; timeout; retries; xfail = None }

(* Declaration location: [?__POS__] wins, and the backtrace fallback is
   best-effort — it can attribute to the wrong frame when the constructor
   call was reached through tail calls (documented in the interface).
   Every constructor binds it with a [let], which keeps the capture out of
   tail position. Inside these definitions [__POS__] is the parameter, not
   the builtin: it is forwarded, never recaptured. *)

let make_test ?__POS__ ?tags ?timeout ?retries name body =
  let annotations = declared ?tags ?timeout ?retries () in
  let loc = Loc.resolve ?__POS__ () in
  Test { name; body; loc; annotations }

let test ?__POS__ ?tags ?timeout ?retries name fn =
  make_test ?__POS__ ?tags ?timeout ?retries name (Body fn)

let slow ?__POS__ ?(tags = []) ?timeout ?retries name fn =
  make_test ?__POS__ ~tags:(Tag.slow :: tags) ?timeout ?retries name (Body fn)

let group ?__POS__ ?tags ?timeout ?retries name children =
  let annotations = declared ?tags ?timeout ?retries () in
  let loc = Loc.resolve ?__POS__ () in
  Group { name; children; loc; annotations }

(* The rows are plain children: the table's tags, limit and retries sit on
   the group and reach each row as its defaults. [name] is required: a child's
   path is its identity, and a positional default would give other seeds and
   another store entry to every later child whenever a row is inserted. *)
let cases ?__POS__ ?tags ?timeout ?retries ~name base inputs fn =
  let annotations = declared ?tags ?timeout ?retries () in
  let loc = Loc.resolve ?__POS__ () in
  let child input =
    Test
      {
        name = name input;
        body = Body (fun () -> fn input);
        loc;
        annotations = declared ();
      }
  in
  Group { name = base; children = List.map child inputs; loc; annotations }

(* [scope] is positional and comes first so that [scoped Eio_main.run] is a
   constructor with every optional argument still available: an optional is
   erased by applying a positional argument that follows it, and here none
   does. *)
let scoped scope ?__POS__ ?tags ?timeout ?retries name fn =
  make_test ?__POS__ ?tags ?timeout ?retries name (Scoped { scope; body = fn })

(* A bracket is a scoped test whose scope is spelled by hand rather than
   with [Fun.protect]: a raising teardown must reach the runner as itself
   — an assertion, a skip, the re-armed timeout — not wrapped in
   [Finally_raised], and a fatal exception must not run user code on its
   way out. The body's exception keeps its backtrace across the teardown. *)
let bracket ?__POS__ ?tags ?timeout ?retries ~setup ~teardown name fn =
  let scope k =
    let resource = setup () in
    match k resource with
    | () -> teardown resource
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        if not (Failure.is_fatal exn) then teardown resource;
        Printexc.raise_with_backtrace exn backtrace
  in
  scoped scope ?__POS__ ?tags ?timeout ?retries name fn

(* Annotations *)

let annotate f = function
  | Test t -> Test { t with annotations = f t.annotations }
  | Group g -> Group { g with annotations = f g.annotations }

let focus t = annotate (fun a -> { a with focused = true }) t

(* Innermost wins: the value nearest the test — its own, else its closest
   annotated ancestor's — is the one that applies. *)
let nearest own inherited = match own with Some _ -> own | None -> inherited

let xfail ?reason t =
  annotate (fun a -> { a with xfail = nearest a.xfail (Some { reason }) }) t

(* Focus *)

let focus_sites tests =
  let rec node acc = function
    | Test { loc; annotations; _ } ->
        if annotations.focused then loc :: acc else acc
    | Group { loc; annotations; children; _ } ->
        let acc = if annotations.focused then loc :: acc else acc in
        List.fold_left node acc children
  in
  List.rev (List.fold_left node [] tests)

(* Flattening *)

type case = {
  path : string list;
  body : body;
  loc : Loc.t option;
  tags : Tag.t;
  focused : bool;
  timeout : float option;
  retries : int;
  xfail : xfail option;
}

(* A node's effective annotations: its own over its ancestors', tags
   unioned, focus inherited, the rest innermost-wins. *)
let effective ~(inherited : annotations) (own : annotations) : annotations =
  {
    tags = Tag.union inherited.tags own.tags;
    focused = inherited.focused || own.focused;
    timeout = nearest own.timeout inherited.timeout;
    retries = nearest own.retries inherited.retries;
    xfail = nearest own.xfail inherited.xfail;
  }

let flatten tests =
  let rec node ~rev_groups ~inherited acc = function
    | Test t ->
        let a = effective ~inherited t.annotations in
        {
          path = List.rev (t.name :: rev_groups);
          body = t.body;
          loc = t.loc;
          tags = a.tags;
          focused = a.focused;
          timeout = a.timeout;
          retries = Option.value a.retries ~default:0;
          xfail = a.xfail;
        }
        :: acc
    | Group g ->
        let rev_groups = g.name :: rev_groups in
        let inherited = effective ~inherited g.annotations in
        List.fold_left (node ~rev_groups ~inherited) acc g.children
  in
  List.rev
    (List.fold_left (node ~rev_groups:[] ~inherited:(declared ())) [] tests)

let path_to_string path = String.concat " › " path
