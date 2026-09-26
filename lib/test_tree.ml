(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Tag = struct
  module String_set = Set.Make (String)

  type t = String_set.t

  let empty = String_set.empty
  let of_list = String_set.of_list
  let union = String_set.union
  let mem = String_set.mem
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

type xfail = { reason : string option }

type body =
  | Body of (unit -> unit)
  | Scoped : { scope : ('r -> unit) -> unit; body : 'r -> unit } -> body

(* What a node declares of its own, which [flatten] resolves against its
   ancestors'. [None] is "not declared here", so an enclosing group's value
   reaches the node. *)
type annotations = {
  tags : Tag.t;
  focused : bool;
  timeout : float option;
  retries : int option;
  xfail : xfail option;
}

type t = {
  name : string;
  loc : Loc.t option;
  annotations : annotations;
  kind : kind;
}

and kind = Test of body | Group of t list

(* Declaring tests *)

let declared ?(tags = []) ?timeout ?retries () =
  (match timeout with
  | Some s when (not (Float.is_finite s)) || s <= 0. ->
      invalid_arg "windtrap: timeout must be finite and positive"
  | Some _ | None -> ());
  (match retries with
  | Some n when n < 0 -> invalid_arg "windtrap: retries must be non-negative"
  | Some _ | None -> ());
  { tags = Tag.of_list tags; focused = false; timeout; retries; xfail = None }

(* Inside a constructor [__POS__] is the parameter, not the builtin, so a
   position is passed on and never captured anew. *)
let node ?__POS__ ?tags ?timeout ?retries name kind =
  let annotations = declared ?tags ?timeout ?retries () in
  let loc = Loc.resolve ?__POS__ () in
  { name; loc; annotations; kind }

let test ?__POS__ ?tags ?timeout ?retries name fn =
  node ?__POS__ ?tags ?timeout ?retries name (Test (Body fn))

let slow ?__POS__ ?(tags = []) ?timeout ?retries name fn =
  node ?__POS__ ~tags:(Tag.slow :: tags) ?timeout ?retries name (Test (Body fn))

let group ?__POS__ ?tags ?timeout ?retries name children =
  node ?__POS__ ?tags ?timeout ?retries name (Group children)

(* A row is a plain test under the table's group, whose annotations reach it
   as any group's do. [name] has no positional default: inserting a row would
   rename every later one, and so change its seeds and its last-failed entry. *)
let cases ?__POS__ ?tags ?timeout ?retries ~name base inputs fn =
  let annotations = declared ?tags ?timeout ?retries () in
  let loc = Loc.resolve ?__POS__ () in
  let row input =
    let kind = Test (Body (fun () -> fn input)) in
    { name = name input; loc; annotations = declared (); kind }
  in
  { name = base; loc; annotations; kind = Group (List.map row inputs) }

let scoped scope ?__POS__ ?tags ?timeout ?retries name fn =
  node ?__POS__ ?tags ?timeout ?retries name
    (Test (Scoped { scope; body = fn }))

(* Not [Fun.protect]: a raising teardown must reach the runner as itself, not
   as [Finally_raised], and a fatal exception, which [Failure.catch] never
   returns, must run no user code on its way out. *)
let bracket ?__POS__ ?tags ?timeout ?retries ~setup ~teardown name fn =
  let scope k =
    let resource = setup () in
    match Failure.catch (fun () -> k resource) with
    | Ok () -> teardown resource
    | Error c ->
        teardown resource;
        Failure.reraise c
  in
  scoped scope ?__POS__ ?tags ?timeout ?retries name fn

(* Annotations *)

let focus t = { t with annotations = { t.annotations with focused = true } }
let nearest own inherited = match own with Some _ -> own | None -> inherited

let xfail ?reason t =
  let xfail = nearest t.annotations.xfail (Some { reason }) in
  { t with annotations = { t.annotations with xfail } }

let focus_sites tests =
  let rec add acc t =
    let acc = if t.annotations.focused then t.loc :: acc else acc in
    match t.kind with
    | Test _ -> acc
    | Group children -> List.fold_left add acc children
  in
  List.rev (List.fold_left add [] tests)

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

(* Tags are unioned, focus is inherited, and every other value is the one
   declared nearest the node. *)
let effective ~(inherited : annotations) (own : annotations) : annotations =
  {
    tags = Tag.union inherited.tags own.tags;
    focused = inherited.focused || own.focused;
    timeout = nearest own.timeout inherited.timeout;
    retries = nearest own.retries inherited.retries;
    xfail = nearest own.xfail inherited.xfail;
  }

let flatten tests =
  let rec add ~rev_groups ~inherited acc t =
    let a = effective ~inherited t.annotations in
    match t.kind with
    | Test body ->
        {
          path = List.rev (t.name :: rev_groups);
          body;
          loc = t.loc;
          tags = a.tags;
          focused = a.focused;
          timeout = a.timeout;
          retries = Option.value a.retries ~default:0;
          xfail = a.xfail;
        }
        :: acc
    | Group children ->
        let rev_groups = t.name :: rev_groups in
        List.fold_left (add ~rev_groups ~inherited:a) acc children
  in
  List.rev
    (List.fold_left (add ~rev_groups:[] ~inherited:(declared ())) [] tests)

let path_to_string path = String.concat " › " path
