(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: this module judges mutants (see ppx/runtime/dune). *)
[@@@mutate exclude_file]

(* The registry *)

(* A registration's library, [None] for code of no library, and its
   partition, the basename of its file. *)
type origin = string option * string

(* An open [module%test] group. *)
type frame = {
  name : string;
  tags : string list;
  origin : origin;
  names : (string, int ref) Hashtbl.t; (* how many children took each name *)
  mutable children : Windtrap.test list; (* newest first *)
}

(* Module initializers fill the registry before [init] reads the protocol;
   [collect] drains the top level. *)
let groups : frame list ref = ref [] (* the open groups, innermost first *)
let top_level : (origin * Windtrap.test) list ref = ref [] (* newest first *)

(* How many top-level registrations took each name, per library and module. *)
let top_names : (string option * string * string, int ref) Hashtbl.t =
  Hashtbl.create 16

(* Every origin that registered, drained or not, newest first. *)
let origins : origin list ref = ref []

(* The undriven guard *)

let claimed = ref false

let partitions_of drives =
  List.sort_uniq String.compare
    (List.filter_map
       (fun (library, partition) ->
         if drives library then Some partition else None)
       !origins)

(* [Windtrap.run] installs its exit guard mid-run, after every registration,
   so this handler runs after it. Each [at_exit] handler runs at most once,
   so the [Stdlib.exit] here still runs the others, a coverage dump
   included. A forked child inherits the handler and must stay silent. *)
let undriven_guard =
  lazy
    (let owner = Unix.getpid () in
     at_exit (fun () ->
         if (not !claimed) && Unix.getpid () = owner then begin
           Printf.eprintf
             "windtrap: registered inline tests were never driven: this \
              executable links ppx_windtrap-preprocessed test code of no \
              library (%s) and nothing ran it.\n\
              windtrap: move the tests into a library stanza with \
              (inline_tests), whose inline runner dune builds and drives, or \
              drive the runner protocol yourself \
              (Ppx_windtrap_runtime.Ppx_runtime.init/exit). Exiting 2: nothing \
              ran.\n\
              %!"
             (String.concat ", " (partitions_of Option.is_none));
           Stdlib.exit 2
         end))

(* Registration *)

let module_name partition =
  String.capitalize_ascii (List.hd (String.split_on_char '.' partition))

(* The runner refuses two tests of one path. *)
let rec unique names key name =
  match Hashtbl.find_opt names (key name) with
  | None ->
      Hashtbl.add names (key name) (ref 1);
      name
  | Some n ->
      incr n;
      unique names key (Printf.sprintf "%s (%d)" name !n)

let note_origin ?library file =
  if Option.is_none library then Lazy.force undriven_guard;
  let origin = (library, Filename.basename file) in
  origins := origin :: !origins;
  origin

let scoped_name (library, partition) name =
  match !groups with
  | [] ->
      unique top_names (fun name -> (library, module_name partition, name)) name
  | frame :: _ -> unique frame.names Fun.id name

let register origin tree =
  match !groups with
  | [] -> top_level := (origin, tree) :: !top_level
  | frame :: _ -> frame.children <- tree :: frame.children

let add_test ?library ~file ~pos ~tags name fn =
  let origin = note_origin ?library file in
  let name = scoped_name origin name in
  register origin (Windtrap.test ~__POS__:pos ~tags name fn)

let enter_group ?library ~file ~tags name =
  let origin = note_origin ?library file in
  let name = scoped_name origin name in
  groups :=
    { name; tags; origin; names = Hashtbl.create 8; children = [] } :: !groups

let leave_group () =
  match !groups with
  | [] -> invalid_arg "Ppx_runtime.leave_group: no group is open"
  | frame :: rest ->
      groups := rest;
      register frame.origin
        (Windtrap.group ~tags:frame.tags frame.name (List.rev frame.children))

(* Expect tests *)

module Baseline = Windtrap.Private.Baseline
module Loc = Windtrap.Private.Loc
module Run = Windtrap.Private.Run

(* The delimiter keeps a failure raised in the body's tail position from
   being located in this function. *)
let expect_test ~pos ~body_end body output =
  Loc.delimit body;
  Run.check_baseline ~loc:(Loc.of_pos body_end)
    (Baseline.Trailing { pos })
    (output ())

(* The runner protocol *)

(* [runner] is the library of [inline-test-runner <lib>], and [None]
   outside the runner mode. *)
type protocol = {
  prog : string;
  runner : string option;
  partition : string option;
  list_partitions : bool;
}

let unread =
  { prog = ""; runner = None; partition = None; list_partitions = false }

let protocol = ref unread

let init argv =
  claimed := true;
  let rec parse p = function
    | "inline-test-runner" :: library :: rest ->
        parse { p with runner = Some library } rest
    | "-partition" :: file :: rest ->
        parse { p with partition = Some file } rest
    | "-list-partitions" :: rest -> parse { p with list_partitions = true } rest
    | _ :: rest -> parse p rest
    | [] -> p
  in
  let prog = if Array.length argv > 0 then argv.(0) else "" in
  protocol := parse { unread with prog } (Array.to_list argv)

let drives library =
  Option.is_none library || Option.equal String.equal library !protocol.runner

(* [init] runs after the test modules have loaded, so the partition is
   applied here and not at registration. *)
let collect () =
  claimed := true;
  match !groups with
  | _ :: _ ->
      invalid_arg "Ppx_runtime.collect: a module%test group was never closed"
  | [] ->
      let registered = List.rev !top_level in
      top_level := [];
      Hashtbl.reset top_names;
      let in_partition partition =
        match !protocol.partition with
        | None -> true
        | Some wanted -> String.equal partition wanted
      in
      let by_module = Hashtbl.create 16 and first_seen = ref [] in
      List.iter
        (fun ((library, partition), tree) ->
          if drives library && in_partition partition then
            let name = module_name partition in
            match Hashtbl.find_opt by_module name with
            | Some trees -> trees := tree :: !trees
            | None ->
                first_seen := name :: !first_seen;
                Hashtbl.add by_module name (ref [ tree ]))
        registered;
      List.rev_map
        (fun name ->
          Windtrap.group name (List.rev !(Hashtbl.find by_module name)))
        !first_seen

let exit () =
  let p = !protocol in
  match (p.runner, p.list_partitions) with
  | None, _ -> Stdlib.exit 0
  | Some _, true ->
      List.iter print_endline (partitions_of drives);
      Stdlib.exit 0
  | Some library, false ->
      (* Dune runs a library's partitions concurrently, and a suite's name
         keys its capture logs, its JUnit file and its last failed tests. *)
      let suite =
        match p.partition with
        | Some file -> library ^ "/" ^ file
        | None -> library
      in
      (* Under [--corrected] a recorded correction leaves the exit code
         alone: the [diff?] dune runs next is the verdict. *)
      Stdlib.exit
        (Windtrap.run ~argv:[| p.prog; "--corrected" |] suite (collect ()))
