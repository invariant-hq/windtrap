(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — it collects the suite and calls [run] — so a
   mutant here is armed inside the process that is supposed to detect
   it: a dropped registration or a wrong exit code is not a survivor but
   a corrupted verdict. This library's dune carries no mutation stanza,
   so no build can instrument it; the attribute stays as the statement
   of intent and the guard against a stanza appearing. Coverage still
   measures the file. *)
[@@@mutate exclude_file]

(* The ordinary-OCaml half of ppx_windtrap: the module-load registry the
   generated code fills, the inline-test-runner protocol dune speaks to
   the generated main, and the undriven-registration guard. A client of
   the public API only: every test it registers is a [Windtrap.test], and
   running them is one [Windtrap.run] under [--corrected]. *)

module String_set = Set.Make (String)

(* Registration *)

(* A [module%test] group being filled: its own sibling-name counts, and
   its children in reverse registration order. *)
type frame = {
  name : string;
  tags : string list;
  file : string;
  names : (string, int ref) Hashtbl.t;
  mutable children : Windtrap.test list;
}

let group_stack : frame list ref = ref []

(* Top-level trees with their source file, in reverse registration order,
   and the top-level sibling-name counts per (module, name). *)
let top_level : (string * Windtrap.test) list ref = ref []
let top_names : (string * string, int ref) Hashtbl.t = Hashtbl.create 16
let partitions_seen = ref String_set.empty

(* The undriven-registration guard

   The silent success this closes: [let%expect_test] code preprocessed
   with ppx_windtrap inside a plain (executable) or (test) stanza
   registers its tests at module load, and with no (inline_tests) stanza
   nothing ever drives the registry — the binary exits 0 having run
   nothing, and its expectations are never checked against anything.
   The first registration therefore installs a [Stdlib.at_exit] handler;
   every legitimate driving path claims the registry ([init] and
   [collect]); a process that terminates normally with registrations
   never claimed prints the diagnostic and exits 2 — the nothing-ran
   code, which can be read as neither a pass nor a test failure.

   Ordering against the core runner's exit guard: registration is a
   module-load act and [Windtrap.run] installs its guard mid-run, so this
   handler always sits deeper in the [at_exit] chain and runs after it.
   An in-run exit the core guard cancels never reaches this handler; it
   fires only at the exit that finally proceeds, when no run is active.
   Firing calls [Stdlib.exit] from inside an [at_exit] handler, which is
   safe: each registered handler runs at most once, so the nested
   [do_at_exit] skips this one and still runs the rest — a coverage
   runtime's at_exit dump included, which is why this is [Stdlib.exit]
   and not [Unix._exit].

   The pid check is the core guard's own defense: a test that forks
   inherits the handler, and a child exiting through [Stdlib.exit] must
   not repeat the parent's diagnostic. *)

let guard_claimed = ref false
let guard_installed = ref false
let claim_registry () = guard_claimed := true

let undriven_diagnostic files =
  let code =
    match files with
    | [] -> "ppx_windtrap-preprocessed test code"
    | files ->
        Printf.sprintf "ppx_windtrap-preprocessed test code (%s)"
          (String.concat ", " files)
  in
  "windtrap: registered inline tests were never driven: this executable links "
  ^ code
  ^ " but nothing ran it.\n\
     windtrap: add (inline_tests) to the library stanza so dune builds and \
     drives the inline runner, or drive the runner protocol yourself \
     (Ppx_windtrap_runtime.Ppx_runtime.init/exit). Exiting 2: nothing ran.\n"

let install_undriven_guard () =
  if not !guard_installed then begin
    guard_installed := true;
    let owner = Unix.getpid () in
    Stdlib.at_exit (fun () ->
        if (not !guard_claimed) && Unix.getpid () = owner then begin
          output_string Stdlib.stderr
            (undriven_diagnostic (String_set.elements !partitions_seen));
          flush Stdlib.stderr;
          Stdlib.exit 2
        end)
  end

let note_partition file =
  (* Every registration entry point passes through here, so the first
     registration is what arms the undriven guard — a process that
     merely links this runtime, registering nothing, installs no
     handler. *)
  install_undriven_guard ();
  partitions_seen := String_set.add (Filename.basename file) !partitions_seen

let module_name_of_file file =
  let base = Filename.basename file in
  let stem =
    match String.index_opt base '.' with
    | Some i -> String.sub base 0 i
    | None -> base
  in
  String.capitalize_ascii stem

(* Duplicate names in one scope: a functor containing [let%expect_test]
   instantiated twice registers the same name and location twice.
   ppx_expect runs both; windtrap's runner requires unique full paths, so
   later duplicates get a " (2)", " (3)", … suffix — deterministic in
   registration order — and both run. Top-level names are counted per
   (module, name); a group's siblings are counted in the frame's own
   table, so the scopes cannot collide. *)
let uniquify tbl key_of name =
  let rec fresh name =
    match Hashtbl.find_opt tbl (key_of name) with
    | None ->
        Hashtbl.add tbl (key_of name) (ref 1);
        name
    | Some n ->
        incr n;
        fresh (Printf.sprintf "%s (%d)" name !n)
  in
  fresh name

let scoped_name ~file name =
  match !group_stack with
  | [] -> uniquify top_names (fun n -> (module_name_of_file file, n)) name
  | frame :: _ -> uniquify frame.names (fun n -> n) name

let register ~file tree =
  match !group_stack with
  | [] -> top_level := (file, tree) :: !top_level
  | frame :: _ -> frame.children <- tree :: frame.children

let add_test ~file ~pos ~tags name fn =
  note_partition file;
  let name = scoped_name ~file name in
  register ~file (Windtrap.test ~__POS__:pos ~tags name fn)

let enter_group ~file ~tags name =
  note_partition file;
  let name = scoped_name ~file name in
  group_stack :=
    { name; tags; file; names = Hashtbl.create 8; children = [] }
    :: !group_stack

let leave_group () =
  match !group_stack with
  | [] -> invalid_arg "Ppx_runtime.leave_group: no group is open"
  | frame :: rest ->
      group_stack := rest;
      register ~file:frame.file
        (Windtrap.group ~tags:frame.tags frame.name (List.rev frame.children))

(* The runner protocol *)

let prog = ref ""
let runner_mode = ref false
let library = ref None
let partition = ref None
let list_only = ref false

let init argv =
  (* The runner protocol's entry claims the registry in every mode — a
     partition run, -list-partitions, and the generated runner invoked
     by hand (which then does nothing, by [exit]'s documented contract:
     a deliberate invocation is not a silent one). *)
  claim_registry ();
  prog := if Array.length argv > 0 then argv.(0) else "";
  runner_mode := false;
  library := None;
  partition := None;
  list_only := false;
  let rec parse = function
    | [] -> ()
    | "inline-test-runner" :: lib :: rest ->
        runner_mode := true;
        library := Some lib;
        parse rest
    | "-partition" :: name :: rest ->
        partition := Some name;
        parse rest
    | "-list-partitions" :: rest ->
        list_only := true;
        parse rest
    | _ :: rest -> parse rest
  in
  parse (Array.to_list argv)

(* Partition filtering happens at collection, not registration: init —
   which sets the partition — runs after the test modules have loaded. *)
let collect () =
  (* Draining claims the registry for the undriven guard: whoever takes
     the trees owns the execution of what they took — the rule that
     covers a hand-rolled main driving [Windtrap.run] itself. *)
  claim_registry ();
  if !group_stack <> [] then
    invalid_arg "Ppx_runtime.collect: a module%test group was never closed";
  let entries = List.rev !top_level in
  top_level := [];
  Hashtbl.reset top_names;
  let entries =
    match !partition with
    | None -> entries
    | Some wanted ->
        List.filter
          (fun (file, _) -> String.equal (Filename.basename file) wanted)
          entries
  in
  let order = ref [] in
  let by_module : (string, Windtrap.test list ref) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun (file, tree) ->
      let name = module_name_of_file file in
      match Hashtbl.find_opt by_module name with
      | Some trees -> trees := tree :: !trees
      | None ->
          order := name :: !order;
          Hashtbl.add by_module name (ref [ tree ]))
    entries;
  List.rev_map
    (fun name -> Windtrap.group name (List.rev !(Hashtbl.find by_module name)))
    !order

let partitions () = String_set.elements !partitions_seen

let exit () =
  if not !runner_mode then Stdlib.exit 0;
  if !list_only then begin
    List.iter print_endline (partitions ());
    Stdlib.exit 0
  end;
  let suite = Option.value ~default:"inline tests" !library in
  (* One runner: the inline suite is an ordinary [run] under
     [--corrected], which is dune's promotion protocol — a recorded
     correction leaves the exit code alone so the [diff?] that follows
     is the verdict — with the [WINDTRAP_*] mirrors as the rest of the
     command line, as for every run dune drives. *)
  Stdlib.exit (Windtrap.run ~argv:[| !prog; "--corrected" |] suite (collect ()))
