(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — the scheduler, the ambient run state, the reporting
   spine — so a mutant here is armed inside the process that is supposed
   to detect it. The failure mode is not a false survivor but a hang or a
   corrupted verdict: a mutated bail counter or timeout does not fail the
   reaching tests, it stops them from finishing. This library's dune
   carries no mutation stanza, so no build can instrument it; the
   attribute stays as the statement of intent and the guard against a
   stanza appearing. Coverage still measures the file. *)
[@@@mutate exclude_file]

(* The ordinary-OCaml half of ppx_windtrap. Protocol and sandbox mechanics
   (argv sniffing, partitions, .corrected files written to the module-load
   cwd) adapted from windtrap v1's lib/ppx_runtime.ml; payload matching,
   correction formatting, and the corrected-file writer follow ppx_expect
   at the pinned conformance commit (test/conformance/NOTICE; upstream
   runtime/{test_spec,test_node,output}.ml and the shapes its corpus
   goldens pin) — see the .mli for the contract. *)

(* The client diet. This block is the complete list of core modules the
   expect runtime consumes, kept deliberately minimal — the greppable
   census of the one out-of-core client's reach (Law 12). Drive-side:
   Cli (settings resolution over empty), Driver (the spine and
   execute_and_report, write_junit), Runner (outcome readers, startup
   exit codes), Run (result rows, the run's snapshot registry, the
   ambient slot the body-side one-liners below read), Env (GitHub
   gating), Registry (the Law 16d hook registration), Mutate_loop (the
   mutation-aware run entry both thin drivers call). Body-side: Capture,
   through [captured_output] below. Shared vocabulary: Failure,
   Test_tree, Loc, Text.

   Accepting corrections into the source tree (WINDTRAP_UPDATE) adds the
   three modules that already carry snapshot acceptance, so this runtime
   restates none of it: Snapshot for the resolved update mode — read off
   the registry the runner built, which is where its CI refusal and the
   [force] override have already been applied — Path_ops for the project
   root and the proof that a reconstructed path lies under it, and
   Atomic_file for the publication. See [accept_into_source_tree].

   All of it arrives through [Windtrap.Private]: this library sits
   outside the core, and Private is the core's one export surface for
   co-versioned clients. Widening this list is a design act; record the
   reason here. *)
module Atomic_file = Windtrap.Private.Atomic_file
module Capture = Windtrap.Private.Capture
module Cli = Windtrap.Private.Cli
module Driver = Windtrap.Private.Driver
module Env = Windtrap.Private.Env
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Mutate_loop = Windtrap.Private.Mutate_loop
module Path_ops = Windtrap.Private.Path_ops
module Registry = Windtrap.Private.Registry
module Run = Windtrap.Private.Run
module Runner = Windtrap.Private.Runner
module Snapshot = Windtrap.Private.Snapshot
module Test_tree = Windtrap.Private.Test_tree
module Text = Windtrap.Private.Text

(* Ambient-reading one-liners: read the one documented slot, dispatch on
   explicit state. Semantics live in Run and Capture. *)
let add_failure failure = Run.add_failure (Run.current_frame ()) failure
let current_path () = Run.path (Run.current_frame ())
let failure_count () = List.length (Run.failures (Run.current_frame ()))
let captured_output ?pos () = Capture.output ?pos (Run.capture (Run.current ()))

(* Initialization *)

(* Captured eagerly at module load, before any test runs: tests may chdir,
   and .corrected files must land where dune's diff action looks — next to
   the copied source in the sandbox, the cwd the runner started in. It is
   deliberately not part of [state] below: the module-load cwd is a fact
   about the process, not run state, and [reset] must leave it standing. *)
let initial_dir = Sys.getcwd ()

(* Locations and nodes *)

type loc = { line : int; start_bol : int; start_pos : int; end_pos : int }
type delimiter = Quote | Tag of string

(* [literal_loc], not [loc]: field names are unique across this module's
   record types so generated record literals resolve every field by
   qualified path alone — a collision would fall back to type-directed
   disambiguation, fatal warning 42 under -w +a -warn-error +a. *)
type payload = { contents : string; delimiter : delimiter; literal_loc : loc }
type node_kind = Expect | Expect_exact
type node = { id : int; kind : node_kind; loc : loc; payload : payload option }

let column loc = loc.start_pos - loc.start_bol
let loc_t ~file loc = { Loc.file; line = loc.line; column = column loc }

let pos_of ~file loc : Loc.pos =
  (file, loc.line, column loc, loc.end_pos - loc.start_bol)

(* Run state *)

(* The shapes the state is made of, gathered ahead of it so that it can be
   one value; the code that builds and consumes each lives in its own
   section below. *)

module String_set = Set.Make (String)

type group_frame = {
  group_name : string;
  group_tags : string list;
  group_file : string;
  group_names : (string, int ref) Hashtbl.t;
      (* sibling names used under this group, for duplicate renaming *)
  mutable children : Test_tree.t list;
}

type correction =
  | Node_fix of { span : int * int; text : string }
      (* the extent this correction replaces — the payload literal, or
         the whole node when the payload is the node — and its
         replacement *)
  | Insert of { body_loc : loc; body_wrap : int option; contents : string }
(* a trailing node inserted at the trailing point, plus the ";" at
   [body_loc]'s end *)

type correction_key =
  | Node_key of int * int (* node span *)
  | Insert_key of int (* trailing point *)

type mismatch = { formatted : string; shown : string }
type reach_result = Pass | Fail of mismatch

(* One reach of a node: the sanitized output it consumed and the reconciled
   result. [raw] is kept because the multiple-outputs CR block lists every
   reach's raw output (ppx_expect's shape). *)
type reach = { raw : string; result : reach_result }

type expect_ctx = {
  ctx_file : string;
  ctx_sanitize : string -> string;
  ctx_nodes : node array;
  ctx_results : reach list array; (* per node id, reverse reach order *)
  ctx_body_loc : loc;
  ctx_body_wrap : int option;
  ctx_trailing_loc : loc;
}

(* All the state this module keeps between calls, in one value.

   Module-global it has to be: the generated module initializers register
   as the test library loads, before any run record exists (see the .mli).
   One value it has to be for [reset]'s sake — [reset] is the single
   assignment [state := initial_state ()], and because [initial_state] is
   a record literal a field added later cannot compile without an initial
   value there, so no field can quietly escape the reset. Nothing outside
   this record holds a piece of it: every use reaches the tables through
   [!state], so replacing the record replaces them all. *)
type state = {
  (* Protocol: parsed by [init] out of the runner's argv, read by [exit]. *)
  mutable initialized : bool; (* [init]'s once-guard *)
  mutable am_test_runner : bool;
  mutable current_lib : string option;
  mutable partition : string option;
  mutable list_partitions_only : bool;
  (* Registration: filled by the generated module initializers, drained by
     [collect]. [top_names] counts a file's top-level sibling names, for
     duplicate renaming; a group's siblings are counted in its own frame. *)
  mutable group_stack : group_frame list;
  mutable top_level : (string * Test_tree.t) list;
  mutable partitions_seen : String_set.t;
  top_names : (string * string, int ref) Hashtbl.t;
  (* Recorded while tests run. *)
  corrections : (string, (correction_key, correction) Hashtbl.t) Hashtbl.t;
  node_pool : (string * int * int, reach list ref) Hashtbl.t;
      (* merged reach histories, keyed (file, node span) *)
  covered : (string list, bool) Hashtbl.t;
      (* paths of failed tests whose failures are all corrected *)
  mutable current_expect : expect_ctx option; (* the executing body, if any *)
  mutable read_only : bool;
      (* Law 16(d): while a mutant is armed, checking records no
         correction and reports no coverage of a failure by one. *)
}

let initial_state () : state =
  {
    initialized = false;
    am_test_runner = false;
    current_lib = None;
    partition = None;
    list_partitions_only = false;
    group_stack = [];
    top_level = [];
    partitions_seen = String_set.empty;
    top_names = Hashtbl.create 16;
    corrections = Hashtbl.create 16;
    node_pool = Hashtbl.create 16;
    covered = Hashtbl.create 16;
    current_expect = None;
    read_only = false;
  }

let state = ref (initial_state ())

(* The undriven-registration guard

   The silent success this closes: [let%expect_test] code preprocessed
   with ppx_windtrap inside a plain (executable) or (test) stanza
   registers its tests at module load, and with no (inline_tests) stanza
   nothing ever drives the registry — the binary exits 0 having run
   nothing, and its expectations are never checked against anything.
   The first registration therefore installs a [Stdlib.at_exit] handler;
   every legitimate driving path claims the registry ([claim_registry]'s
   callers: [init], [collect], [enter_armed], [reset] — the rule is
   argued at each call site and stated in the .mli); a process that
   terminates normally with registrations never claimed prints the
   diagnostic and exits 2 — Law 11's nothing-ran code, which can be read
   as neither a pass nor a test failure.

   Process facts, not run state, like [initial_dir]: [reset] replaces
   the state record and leaves these standing — a claimed process stays
   claimed for its life, and the handler is installed at most once.

   Ordering against the core runner's exit guard: registration is a
   module-load act and [Runner.execute] installs its guard mid-run, so
   this handler always sits deeper in the [at_exit] chain and runs
   after it. An in-run exit the core guard cancels (by raising
   [Failure.Exit_attempt] out of [do_at_exit]) never reaches this
   handler — its once-flag is untouched, and it fires only at the exit
   that finally proceeds, when no run is active. Firing calls
   [Stdlib.exit] from inside an [at_exit] handler, which is safe: each
   registered handler runs at most once, so the nested [do_at_exit]
   skips this one and still runs the rest — a coverage runtime's
   at_exit dump included, which is why this is [Stdlib.exit] and not
   [Unix._exit].

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
            (undriven_diagnostic (String_set.elements !state.partitions_seen));
          flush Stdlib.stderr;
          Stdlib.exit 2
        end)
  end

(* Armed processes (Law 16d)

   Two things a process that is about to run with a mutant armed owes this
   module, and they are always owed together:

   Read-only checking. The two correction recorders and the exit
   protocol's coverage bit are the whole of the correction path's write
   side, so silencing them is what makes an armed [%expect] mismatch a
   plain failure: nothing is recorded, so [flush_corrections_report]
   finds nothing to write, nothing to accept into the source tree
   whatever WINDTRAP_UPDATE says, and [inline_exit_code] sees no failure
   covered by a correction. Checking, matching and failure reporting are
   untouched — [accepting] reads the read-only bit for the same reason.
   Nothing ever turns it back off — a process that arms stays read-only
   for its life.

   Clearing the cross-run tables. A forked mutation child inherits the
   parent dry run's merged reach histories; left in place they would
   resolve the child's first mismatch against the PARENT's outputs and
   report ppx_expect's "test ran multiple times" CR block instead of the
   mismatch that killed the mutant. Registration and the protocol
   arguments are deliberately kept: the child runs the tests the parent
   registered.

   Snapshots need no counterpart: [Snapshot.resolve_mode] maps
   [Env.No_update] to [Snapshot.Check] and writing is reachable only
   under [Snapshot.Update], so an armed run's No_update already makes
   snapshot checking read-only by construction. *)

let enter_armed () =
  (* A process with a mutant armed belongs to the mutation loop: its
     transcript closes with a verdict line and its exit code is Law
     16(e)'s, and the undriven guard must write into neither. The forked
     children leave through [Unix._exit] and never run [at_exit] anyway;
     this claim covers the interactive WINDTRAP_MUTATE_ARM parent. *)
  claim_registry ();
  !state.read_only <- true;
  Hashtbl.reset !state.corrections;
  Hashtbl.reset !state.node_pool;
  Hashtbl.reset !state.covered;
  !state.current_expect <- None

(* Registered, not passed: the mutation loop sits below this module and
   fires the registry's hooks in every process that arms — each forked
   child before its first test, once in the parent under
   WINDTRAP_MUTATE_ARM (Law 16d). Registering at module load means any
   process that can run this runtime's tests has the debt on record
   before any run can arm; [reset] leaves it standing, like
   [initial_dir], because owing the mutation loop read-only checking is a
   fact about the process, not run state. *)
let () = Registry.on_armed enter_armed

(* Protocol arguments *)

let init argv =
  (* The runner protocol's entry claims the registry in every mode — a
     partition run, -list-partitions, and the generated runner invoked
     by hand (which then does nothing, by [exit]'s documented contract:
     a deliberate invocation is not a silent one). *)
  claim_registry ();
  if !state.initialized then ()
  else begin
    !state.initialized <- true;
    let rec parse = function
      | [] -> ()
      | "inline-test-runner" :: lib :: rest ->
          !state.am_test_runner <- true;
          !state.current_lib <- Some lib;
          parse rest
      | "-partition" :: name :: rest ->
          !state.partition <- Some name;
          parse rest
      | "-list-partitions" :: rest ->
          !state.list_partitions_only <- true;
          parse rest
      | _ :: rest -> parse rest
    in
    parse (Array.to_list argv)
  end

(* Registration *)

let note_partition file =
  (* Every registration entry point passes through here, so the first
     registration is what arms the undriven guard — a process that
     merely links this runtime, registering nothing, installs no
     handler. *)
  install_undriven_guard ();
  !state.partitions_seen <-
    String_set.add (Filename.basename file) !state.partitions_seen

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
   (module, name) in [state.top_names]; a group's siblings are counted in
   the frame's own table, so the scopes cannot collide. *)
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
  match !state.group_stack with
  | [] ->
      uniquify !state.top_names (fun n -> (module_name_of_file file, n)) name
  | frame :: _ -> uniquify frame.group_names (fun n -> n) name

let register ~file tree =
  match !state.group_stack with
  | [] -> !state.top_level <- (file, tree) :: !state.top_level
  | frame :: _ -> frame.children <- tree :: frame.children

let add_test ~file ~loc ~tags name fn =
  note_partition file;
  let name = scoped_name ~file name in
  register ~file (Test_tree.test ~pos:(pos_of ~file loc) ~tags name fn)

let enter_group ~file ~tags name =
  note_partition file;
  let name = scoped_name ~file name in
  !state.group_stack <-
    {
      group_name = name;
      group_tags = tags;
      group_file = file;
      group_names = Hashtbl.create 8;
      children = [];
    }
    :: !state.group_stack

let leave_group () =
  match !state.group_stack with
  | [] -> invalid_arg "Ppx_runtime.leave_group: no group is open"
  | frame :: rest ->
      !state.group_stack <- rest;
      register ~file:frame.group_file
        (Test_tree.group ~tags:frame.group_tags frame.group_name
           (List.rev frame.children))

(* Partition filtering happens at collection, not registration: init — which
   sets the partition — runs after the test modules have loaded. *)
let collect () =
  (* Draining claims the registry for the undriven guard: whoever takes
     the trees owns the execution of what they took — the rule that
     covers hand-rolled harnesses driving [Runner] directly. *)
  claim_registry ();
  if !state.group_stack <> [] then
    invalid_arg "Ppx_runtime.collect: a module%test group was never closed";
  let entries = List.rev !state.top_level in
  !state.top_level <- [];
  Hashtbl.reset !state.top_names;
  let entries =
    match !state.partition with
    | None -> entries
    | Some wanted ->
        List.filter
          (fun (file, _) -> String.equal (Filename.basename file) wanted)
          entries
  in
  let order = ref [] in
  let by_module : (string, Test_tree.t list ref) Hashtbl.t =
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
    (fun name -> Test_tree.group name (List.rev !(Hashtbl.find by_module name)))
    !order

let partitions () = String_set.elements !state.partitions_seen

(* Normalization (ppx_expect's pretty-payload pipeline) *)

(* Whitespace is Base.Char.is_whitespace — the set ppx_expect strips with. *)
let is_ws = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let rstrip s =
  let stop = ref (String.length s) in
  while !stop > 0 && is_ws s.[!stop - 1] do
    decr stop
  done;
  if !stop = String.length s then s else String.sub s 0 !stop

let strip s =
  let start = ref 0 and stop = ref (String.length s) in
  while !start < !stop && is_ws s.[!start] do
    incr start
  done;
  while !stop > !start && is_ws s.[!stop - 1] do
    decr stop
  done;
  String.sub s !start (!stop - !start)

let leading_spaces s =
  let n = ref 0 in
  while !n < String.length s && s.[!n] = ' ' do
    incr n
  done;
  !n

(* ppx_expect splits on '\n' treating "\r\n" as one separator; every
   consumer right-strips each line, which removes the '\r' of a "\r\n"
   pair, so a plain split suffices. A lone '\r' inside a line stays a
   literal byte, exactly as upstream. *)
let split_lines s = String.split_on_char '\n' s

let drop_blank_edges lines =
  let rec drop = function "" :: rest -> drop rest | lines -> lines in
  List.rev (drop (List.rev (drop lines)))

(* [(relative indent, stripped contents)] per line of pretty output.
   Indentation counts leading spaces only; contents are stripped of all
   whitespace, tabs included — ppx_expect's legacy rule, kept for
   byte-compatible matching and corrections. *)
let pretty_lines raw =
  let lines = drop_blank_edges (List.map rstrip (split_lines raw)) in
  let indented =
    List.map (fun line -> (leading_spaces line, strip line)) lines
  in
  let min_indent =
    List.fold_left
      (fun acc (indent, contents) ->
        if contents = "" then acc else min acc indent)
      max_int indented
  in
  List.map
    (fun (indent, contents) ->
      ((if contents = "" then 0 else max 0 (indent - min_indent)), contents))
    indented

let spaces n = String.make n ' '

(* The canonical (dedented) form of pretty output. Two [%expect] payloads
   match iff their normalizations are equal, which is exactly ppx_expect's
   rule of comparing both sides through its payload formatter: the
   formatter is [normalize] plus a uniform re-indent, so equality
   coincides. *)
let normalize s =
  String.concat "\n"
    (List.map
       (fun (indent, contents) ->
         if contents = "" then "" else spaces indent ^ contents)
       (pretty_lines s))

(* Correction formatting (ppx_expect's re-indentation) *)

(* Format raw output as the contents of a pretty payload whose node starts at
   column [node_column]: multi-line contents are indented [node_column + 2],
   matching ppx_expect so first promotes after adoption produce no churn. *)
let format_pretty ~delimiter ~node_column raw =
  match pretty_lines raw with
  | [] -> ( match delimiter with Tag _ -> " " | Quote -> "")
  | [ (_, line) ] -> (
      match delimiter with Tag _ -> " " ^ line ^ " " | Quote -> line)
  | lines ->
      let contents_indent = node_column + 2 in
      let first, indentation, last =
        match delimiter with
        | Quote -> (" ", 1, " ")
        | Tag _ -> ("", contents_indent, spaces contents_indent)
      in
      let render (indent, contents) =
        if contents = "" then "" else spaces (indentation + indent) ^ contents
      in
      String.concat "\n" ((first :: List.map render lines) @ [ last ])

let node_delimiter node =
  match node.payload with Some { delimiter; _ } -> delimiter | None -> Tag ""

let formatted_contents node raw =
  match node.kind with
  | Expect_exact -> raw
  | Expect ->
      format_pretty ~delimiter:(node_delimiter node)
        ~node_column:(column node.loc) raw

(* Delimiter conflict fixing: grow the tag until neither delimiter occurs in
   the contents. *)
let fix_tag ~contents tag =
  let rec fix tag =
    if
      Text.contains_substring ~pattern:("{" ^ tag ^ "|") contents
      || Text.contains_substring ~pattern:("|" ^ tag ^ "}") contents
    then fix (tag ^ "xxx")
    else tag
  in
  fix tag

let tag_payload ~tag contents =
  let tag = fix_tag ~contents tag in
  "{" ^ tag ^ "|" ^ contents ^ "|" ^ tag ^ "}"

let extension_name = function
  | Expect -> "expect"
  | Expect_exact -> "expect_exact"

(* Node source rendering *)

(* One corrected node as a patch on the source.

   ppx_expect's runtime patches the payload literal in place and leaves
   the node's head exactly where the author wrote it. The standardized
   whole-node shapes in the monorepo's corrected-file goldens are its
   build's own style pass (bin/apply-style, absent from the pinned
   checkout and from every windtrap user's build), and emulating them
   made one stale payload re-render every other node of the file. So the
   patch is the payload's extent — except where the payload IS the node:
   a bare [%expect] has no literal to patch, and the {%expect|…|}
   shorthand's literal spans the node. *)

let escaped_segments contents =
  List.map String.escaped (String.split_on_char '\n' contents)

let quote_one_line segs = "\"" ^ String.concat "\\n" segs ^ "\""

let node_patch node contents =
  let name = extension_name node.kind in
  match node.payload with
  | None ->
      let payload = tag_payload ~tag:"" contents in
      let text =
        if String.contains contents '\n' then
          "[%" ^ name ^ "\n" ^ spaces (column node.loc + 2) ^ payload ^ "]"
        else "[%" ^ name ^ " " ^ payload ^ "]"
      in
      ((node.loc.start_pos, node.loc.end_pos), text)
  | Some { literal_loc; delimiter; _ } ->
      let shorthand =
        literal_loc.start_pos <= node.loc.start_pos
        && literal_loc.end_pos >= node.loc.end_pos
      in
      let text =
        match (shorthand, delimiter) with
        | true, Tag tag ->
            (* Retagging must never drop the [%expect] spelling. *)
            let tag = fix_tag ~contents tag in
            if tag = "" then "{%" ^ name ^ "|" ^ contents ^ "|}"
            else "{%" ^ name ^ " " ^ tag ^ "|" ^ contents ^ "|" ^ tag ^ "}"
        | _, Tag tag -> tag_payload ~tag contents
        | _, Quote -> quote_one_line (escaped_segments contents)
      in
      ((literal_loc.start_pos, literal_loc.end_pos), text)

(* The corrections table *)

(* Everything recorded while tests run is keyed by [correction_key] so
   that a node reached by several registrations of the same test (functor
   instantiation) has one slot that later resolutions replace, never
   duplicate. *)

let file_corrections file =
  match Hashtbl.find_opt !state.corrections file with
  | Some tbl -> tbl
  | None ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.add !state.corrections file tbl;
      tbl

let record_node_fix ~file node ~contents =
  if not !state.read_only then begin
    let span, text = node_patch node contents in
    Hashtbl.replace (file_corrections file)
      (Node_key (node.loc.start_pos, node.loc.end_pos))
      (Node_fix { span; text })
  end

let record_insert ~file ~body_loc ~body_wrap ~trailing_loc ~contents =
  if not !state.read_only then
    Hashtbl.replace (file_corrections file) (Insert_key trailing_loc.start_pos)
      (Insert { body_loc; body_wrap; contents })

(* Applying corrections to source *)

type patch = { start : int; stop : int; text : string }

let render_insert ~body_loc contents =
  let node_col = column body_loc + 2 in
  let payload = tag_payload ~tag:"" contents in
  if String.contains contents '\n' then
    "\n" ^ spaces node_col ^ "[%expect\n"
    ^ spaces (node_col + 2)
    ^ payload ^ "]"
  else "\n" ^ spaces node_col ^ "[%expect " ^ payload ^ "]"

let corrected_source ~file ~source =
  match Hashtbl.find_opt !state.corrections file with
  | None -> None
  | Some tbl when Hashtbl.length tbl = 0 -> None
  | Some tbl ->
      let length = String.length source in
      let node_patches = ref [] and insert_patches = ref [] in
      Hashtbl.iter
        (fun key correction ->
          match (key, correction) with
          | Node_key _, Node_fix { span = start, stop; text } ->
              node_patches := { start; stop; text } :: !node_patches
          | Insert_key point, Insert { body_loc; body_wrap; contents } ->
              (* A bare [match]/[try]/[function] body takes parentheses in
                 the same patch: otherwise the [;] below binds to its last
                 arm and the inserted node lands inside that arm. *)
              let open_paren =
                match body_wrap with
                | Some start -> [ { start; stop = start; text = "(" } ]
                | None -> []
              in
              let close_paren =
                match body_wrap with
                | Some _ ->
                    [
                      {
                        start = body_loc.end_pos;
                        stop = body_loc.end_pos;
                        text = ")";
                      };
                    ]
                | None -> []
              in
              insert_patches :=
                (open_paren @ close_paren
                @ [
                    {
                      start = body_loc.end_pos;
                      stop = body_loc.end_pos;
                      text = ";";
                    };
                    {
                      start = point;
                      stop = point;
                      text = render_insert ~body_loc contents;
                    };
                  ])
                :: !insert_patches
          (* The mixed pairs cannot be built. *)
          | Node_key _, Insert _ | Insert_key _, Node_fix _ -> ())
        tbl;
      let patches = !node_patches @ List.concat !insert_patches in
      if patches = [] then None
      else begin
        let patches =
          List.stable_sort (fun a b -> compare a.start b.start) patches
        in
        let buf = Buffer.create (length + 256) in
        let cursor =
          List.fold_left
            (fun cursor { start; stop; text } ->
              let start = max 0 (min length start) in
              let stop = max start (min length stop) in
              if start < cursor then cursor
                (* overlap: keep the earlier patch *)
              else begin
                Buffer.add_substring buf source cursor (start - cursor);
                Buffer.add_string buf text;
                stop
              end)
            0 patches
        in
        Buffer.add_substring buf source cursor (length - cursor);
        Some (Buffer.contents buf)
      end

(* Under dune the PPX-recorded path is workspace-relative while the runner's
   cwd is the library's (sandboxed) build directory, which holds the copied
   source under its basename — v1's resolution rule. *)
let absolute_path file =
  if Filename.is_relative file then
    Filename.concat initial_dir (Filename.basename file)
  else file

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

type flush_report = {
  written : string list;
  accepted : string list;
  refused : string list;
}

(* Accepting a correction into the source tree (WINDTRAP_UPDATE).

   The [.corrected] file is a sandbox artifact, and dune registers it for
   promotion only if every partition of the library exits 0 — so one
   file's stale payload is held hostage by another file's crash, and no
   partition can see enough to say so. This is the other channel, the one
   snapshot baselines have always used from inside the same sandboxed
   action: [Path_ops.project_root] walks above any _build tree to the
   real root, [Path_ops.reconstruct] proves the reconstructed path lies
   under it, [Atomic_file.write] publishes. Nothing here is new
   machinery; the asymmetry was that expect payloads were the one
   accepted output still going through dune.

   The drift guard is the part to get right. The corrected content is a
   patch by byte offsets into the SANDBOX copy of the source, and those
   offsets describe the source-tree file only while the two are
   byte-identical. A source edited while the tests ran, a stale sandbox,
   a WINDTRAP_PROJECT_ROOT aimed elsewhere: any of them makes the offsets
   describe something else, and splicing fresh output at them corrupts a
   file the user did not ask to have touched. So compare the bytes first
   and refuse loudly on any difference — the caller reports the refusal
   and exits nonzero, leaving both the source and the [.corrected] alone.
   Never clobber. *)
let accept_into_source_tree ~file ~source ~corrected =
  let root = Path_ops.project_root () in
  match Path_ops.reconstruct ~root file with
  | Error candidate ->
      Error
        (Printf.sprintf "%s is not under the project root %s" candidate root)
  | Ok target -> (
      match read_file target with
      | exception Sys_error reason -> Error reason
      | current when not (String.equal current source) ->
          (* [target] is not named again: the caller's line already leads
             with the recorded path, and under dune the two spell the
             same file. *)
          Error
            "the source file differs from the copy the correction was computed \
             against"
      | _ -> (
          match Atomic_file.write ~path:target corrected with
          | () -> Ok (Path_ops.display target)
          | exception Sys_error reason -> Error reason))

(* One write attempt per file with recorded corrections. A file whose
   source cannot be read, whose target cannot be written, or whose
   acceptance is refused is a flush failure: reported on stderr right
   here — a line naming the recorded source path and the reason — never
   skipped silently, and returned so the exit path can refuse to call the
   partition passed (a correction that reached neither dune's diff nor
   the source tree cannot surface at all).

   [accept] is the run's resolved update mode: with it, each written
   correction is additionally accepted into the source tree, which is
   what makes it independent of the rest of the library. It is only ever
   [true] for corrections this process recorded from unmutated code —
   [enter_armed] empties the table an armed process could have filled
   (Law 16d), so there is nothing here to accept from mutated output. *)
let flush_corrections_report ~accept =
  Sys.chdir initial_dir;
  let files =
    Hashtbl.fold (fun file _ acc -> file :: acc) !state.corrections []
  in
  let written = ref [] and accepted = ref [] and refused = ref [] in
  (* Both refusals name the recorded source path and the reason, and both
     put the file in [refused] — the exit path treats "the correction did
     not fully land" as one fact. They are worded apart because they are
     different states on disk: nothing was written, versus the
     [.corrected] is there and the source tree was left alone. *)
  let refuse ~what file reason =
    Printf.eprintf "Error: correction for %s %s: %s\n%!" file what reason;
    refused := file :: !refused
  in
  List.iter
    (fun file ->
      let write () =
        let source = read_file (absolute_path file) in
        match corrected_source ~file ~source with
        | None -> ()
        | Some corrected -> (
            let target = Filename.basename file ^ ".corrected" in
            let oc = open_out_bin target in
            Fun.protect
              ~finally:(fun () -> close_out_noerr oc)
              (fun () -> output_string oc corrected);
            written := target :: !written;
            if accept then
              match accept_into_source_tree ~file ~source ~corrected with
              | Ok path -> accepted := path :: !accepted
              | Error reason ->
                  refuse ~what:"not accepted into the source tree" file reason)
      in
      match write () with
      | () -> ()
      | exception Sys_error reason -> refuse ~what:"not written" file reason)
    (List.sort compare files);
  (* Cleared in place, not by a fresh [state]: this is the flush's own
     partial clear — the run's other state (reach pools, covered paths,
     protocol) outlives it. *)
  Hashtbl.reset !state.corrections;
  {
    written = List.rev !written;
    accepted = List.rev !accepted;
    refused = List.rev !refused;
  }

(* Coverage of failures by corrections (the exit protocol) *)

(* [state.covered] holds the paths of failed tests whose every failure is
   an expect mismatch with a recorded correction. *)
let inline_exit_code (outcome : Runner.outcome) =
  match outcome.Runner.exit_code with
  | 0 -> 0
  | 2 -> 0 (* an empty selection is an empty partition, not a filter typo *)
  | _ ->
      let failed =
        List.filter
          (fun result ->
            match result.Run.outcome with
            | Failure.Fail _ -> true
            | Failure.Pass | Failure.Skip _ -> false)
          (Run.results outcome.Runner.run)
      in
      (* Only a test row can be covered — the runner's verdict rows (a
         failed fixture release, the strict stale-baselines check) are
         [Fail] rows no correction can cover, so they hold the exit at 1
         through the same predicate as any uncovered failure. The subject
         check, not the path lookup, is what says so: a covered test whose
         name spells a verdict label must not excuse the verdict. *)
      let is_covered (result : Run.result) =
        result.Run.subject = Run.Test
        && Option.value ~default:false
             (Hashtbl.find_opt !state.covered result.Run.path)
      in
      if failed <> [] && List.for_all is_covered failed then 0 else 1

(* The stderr trace of written corrections. Dune runs one sandboxed
   action per library — every partition's runner concurrently, then the
   per-file diff steps — and any nonzero runner exit fails the action,
   skips every diff, and deletes the sandbox with all computed
   .corrected files in it. No partition process can see a sibling's
   corrections (the runs race in a shared sandbox), so the only trace
   that survives a sibling's crash is a line each writing process
   prints for itself: hence the unconditional "wrote" line.

   The caveat under it is unconditional for the same reason. It used to
   fire only when this process was itself exiting nonzero, which reads
   as "my failure withheld my correction" — but the withholding is the
   whole LIBRARY's, and a partition that exits 0 having written a
   correction cannot know whether a sibling just vetoed it. Gating the
   explanation on a signal that lives in another process left it silent
   in exactly the multi-file case it was written for. It costs a line of
   noise on runs where promotion will in fact work; the alternative was
   silence in the case that needs the sentence.

   Under dune one partition is one file, so [written] has at most one
   element; the plural-safe spelling keeps by-hand multi-file runs
   honest. [accepted] names the source files this run wrote directly
   under WINDTRAP_UPDATE: those went nowhere near dune's channel, so
   the caveat does not apply to them and is replaced by the line that
   says where they landed. *)
let correction_notice ~accepted ~refused ~declined written =
  match written with
  | [] -> None
  | files ->
      let notice = Buffer.create 256 in
      Printf.bprintf notice "windtrap: wrote %s\n" (String.concat ", " files);
      if accepted <> [] then
        Printf.bprintf notice "windtrap: accepted into the source tree: %s\n"
          (String.concat ", " accepted);
      (* One explanation, matched to what actually happened: a refusal must
         not advise the acceptance that just failed, a declined acceptance
         must say why fixing the failures comes first, and only a run that
         never asked gets the WINDTRAP_UPDATE suggestion. *)
      if refused <> [] then
        Printf.bprintf notice
          "windtrap: %d correction%s not accepted into the source tree — \
           resolve the reasons above and rerun.\n"
          (List.length refused)
          (if List.length refused = 1 then " was" else "s were")
      else if declined then
        Printf.bprintf notice
          "windtrap: corrections were not accepted into the source tree: a \
           failure above is not an expect mismatch, and acceptance never \
           blesses output produced beside one. Fix the failures and rerun.\n"
      else if accepted = [] then
        Printf.bprintf notice
          "windtrap: dune registers a correction for promotion only when every \
           inline-test process of the library exits cleanly, so a failure in \
           any of its files withholds this one too. Fix the failures, rerun, \
           then 'dune promote' — or rerun with WINDTRAP_UPDATE=1 to accept \
           corrections into the source tree directly.\n";
      Some (Buffer.contents notice)

(* Expect-test execution *)

(* [state.node_pool] merges reaches across every registration of the same
   node — a functor containing an expect test instantiated twice registers
   its nodes twice at the same span; ppx_expect accumulates both instances'
   results in one per-location slot and corrects from the merged history.
   Keys are (file, span); values are chronological. Bodies buffer reaches
   locally and publish here only at resolution, so a skipped body's reaches
   are never merged. Pools store newest-first; [reaches] arrives
   chronological. *)
let pool_publish key reaches =
  match Hashtbl.find_opt !state.node_pool key with
  | Some existing -> existing := List.rev_append reaches !existing
  | None -> Hashtbl.add !state.node_pool key (ref (List.rev reaches))

let pool_get key =
  match Hashtbl.find_opt !state.node_pool key with
  | Some reaches -> List.rev !reaches
  | None -> []

let node_key ctx node = (ctx.ctx_file, node.loc.start_pos, node.loc.end_pos)

let trailing_key ctx =
  (ctx.ctx_file, ctx.ctx_trailing_loc.start_pos, -1 (* not a node span *))

let require_ctx op =
  match !state.current_expect with
  | Some ctx -> ctx
  | None ->
      invalid_arg
        (op
       ^ " only works inside a [let%expect_test] body executed by the test \
          runner")

let consume_output ctx ~pos = ctx.ctx_sanitize (captured_output ~pos ())

let expect_output () =
  let ctx = require_ctx "[%expect.output]" in
  let pos = pos_of ~file:ctx.ctx_file ctx.ctx_body_loc in
  consume_output ctx ~pos

let expect ~id =
  let ctx = require_ctx "[%expect]" in
  if id < 0 || id >= Array.length ctx.ctx_nodes then
    invalid_arg "Ppx_runtime.expect: undeclared node id";
  let node = ctx.ctx_nodes.(id) in
  let raw = consume_output ctx ~pos:(pos_of ~file:ctx.ctx_file node.loc) in
  let expected =
    match node.payload with Some { contents; _ } -> contents | None -> ""
  in
  let result =
    match node.kind with
    | Expect ->
        if String.equal (normalize raw) (normalize expected) then Pass
        else
          Fail
            { formatted = formatted_contents node raw; shown = normalize raw }
    | Expect_exact ->
        if String.equal raw expected then Pass
        else Fail { formatted = raw; shown = raw }
  in
  ctx.ctx_results.(id) <- { raw; result } :: ctx.ctx_results.(id)

(* ppx_expect's marker for a node reached several times with differing
   outputs: a CR comment followed by every distinct run, spliced as the
   corrected payload. Reproduced byte-for-byte so corrections stay
   byte-compatible with ppx_expect's. *)
let cr_for_multiple_outputs ?(output_name = "test output") ~outputs () =
  let cr =
    Printf.sprintf
      "(* CR expect_test: Test ran multiple times with different %ss *)"
      output_name
  in
  let total = List.length outputs in
  let header index =
    let header = Printf.sprintf "=== Output %d / %d ===" (index + 1) total in
    let pad = String.length cr - String.length header in
    if pad <= 0 then header
    else String.make (pad / 2) '=' ^ header ^ String.make (pad - (pad / 2)) '='
  in
  let sections =
    List.concat (List.mapi (fun i output -> [ header i; output ]) outputs)
  in
  String.concat "\n" (cr :: sections)

let shown_expected node =
  match node.payload with
  | Some { contents; _ } -> (
      match node.kind with
      | Expect -> normalize contents
      | Expect_exact -> contents)
  | None -> ""

(* Whether this run accepts a mismatch instead of failing on it.

   WINDTRAP_UPDATE means "take the output I just produced as the new
   expectation". For a snapshot baseline that is a write and a passing
   check; an expect payload is the same act on a different file, so a
   mismatch that comes with a recorded correction is not a failure here
   either — reporting fifty failures for fifty payloads the same run is
   busy accepting is noise, not information. Only mismatches: an
   unreached node, an assertion, a raise are not output anybody produced
   and stay failures, which is why this guards the three sites that
   record a correction and nothing else.

   The mode is read off the registry the runner built for this run, so
   the CI refusal and the [force] override are applied once, by Snapshot,
   for baselines and payloads alike. A read-only (armed) process records
   no correction, so it has nothing to accept and must keep failing —
   the two facts are kept together here rather than left to the caller. *)
let accepting () =
  (not !state.read_only)
  && Snapshot.mode (Run.snapshots (Run.current ())) = Snapshot.Update

let fail_node ctx node ~shown =
  if not (accepting ()) then
    add_failure
      (Failure.equality
         ~loc:(loc_t ~file:ctx.ctx_file node.loc)
         ~expected:(shown_expected node) ~actual:shown ())

(* Deduplicate a reach history by reconciled result — a pass, or the
   formatted correction — as ppx_expect does: two raw outputs that format
   identically are one result. *)
let distinct_fails reaches =
  List.rev
    (List.fold_left
       (fun acc r ->
         match r.result with
         | Pass -> acc
         | Fail m ->
             if
               List.exists
                 (fun seen -> String.equal seen.formatted m.formatted)
                 acc
             then acc
             else m :: acc)
       [] reaches)

(* Resolve one node against the merged reach history (this body's reaches
   already published): a single failing result records its correction;
   several distinct results (including pass + fail) record the CR block
   listing every reach's raw output, in reach order. *)
let resolve_reached ctx node =
  match pool_get (node_key ctx node) with
  | [] -> ()
  | reaches -> (
      let any_pass =
        List.exists
          (fun r -> match r.result with Pass -> true | Fail _ -> false)
          reaches
      in
      match (distinct_fails reaches, any_pass) with
      | [], _ -> ()
      | [ { formatted; shown } ], false ->
          record_node_fix ~file:ctx.ctx_file node ~contents:formatted;
          fail_node ctx node ~shown
      | _ ->
          let cr =
            cr_for_multiple_outputs
              ~outputs:(List.map (fun r -> r.raw) reaches)
              ()
          in
          record_node_fix ~file:ctx.ctx_file node
            ~contents:(formatted_contents node cr);
          fail_node ctx node ~shown:cr)

(* Publish this body's reaches into the merged pools. Reached nodes only:
   a node this body never reached contributes nothing. *)
let publish_reaches ctx =
  Array.iter
    (fun node ->
      match ctx.ctx_results.(node.id) with
      | [] -> ()
      | local -> pool_publish (node_key ctx node) (List.rev local))
    ctx.ctx_nodes

(* End-of-body resolution. [check_reachability] is off when an exception
   aborted the body: the exception explains the unreached nodes (no
   correction is recorded for the exception itself), so only reached
   mismatches are recorded. Reachability is judged on this body's own
   reaches; corrections come from the merged history. Returns
   [true] iff every recorded problem has a recorded correction — the exit
   protocol's "covered" bit. *)
let resolve_nodes ctx ~check_reachability =
  publish_reaches ctx;
  let all_covered = ref true in
  Array.iter
    (fun node ->
      match ctx.ctx_results.(node.id) with
      | [] ->
          if check_reachability then begin
            all_covered := false;
            add_failure
              (Failure.message
                 ~loc:(loc_t ~file:ctx.ctx_file node.loc)
                 (Printf.sprintf "[%%%s] node was never reached"
                    (extension_name node.kind)))
          end
      | _ -> resolve_reached ctx node)
    ctx.ctx_nodes;
  !all_covered

let had_problems ctx =
  Array.exists
    (fun node ->
      match ctx.ctx_results.(node.id) with
      | [] -> true
      | reaches ->
          List.exists
            (fun r -> match r.result with Pass -> false | Fail _ -> true)
            reaches)
    ctx.ctx_nodes

let record_covered value =
  (* Under read-only checking there is no correction for dune to promote,
     so no failure may be reported as covered by one. *)
  if !state.read_only then ()
  else
    let path = current_path () in
    if value = `No_problem then Hashtbl.remove !state.covered path
    else Hashtbl.replace !state.covered path (value = `Covered)

(* Trailing output, resolved like a node against the merged history: an
   instance with no trailing output is a passing reach, so a functor whose
   instances disagree produces ppx_expect's "different trailing outputs"
   CR block instead of the last instance silently winning. Returns [true]
   iff a trailing correction was recorded (unmatched output exists on some
   reach). *)
let resolve_trailing ctx ~raw =
  let insert_column = column ctx.ctx_body_loc + 2 in
  let formatted =
    format_pretty ~delimiter:(Tag "") ~node_column:insert_column raw
  in
  let result =
    if String.equal (strip raw) "" then Pass
    else Fail { formatted; shown = normalize raw }
  in
  let key = trailing_key ctx in
  pool_publish key [ { raw; result } ];
  let reaches = pool_get key in
  let any_pass =
    List.exists
      (fun r -> match r.result with Pass -> true | Fail _ -> false)
      reaches
  in
  let record contents =
    record_insert ~file:ctx.ctx_file ~body_loc:ctx.ctx_body_loc
      ~body_wrap:ctx.ctx_body_wrap ~trailing_loc:ctx.ctx_trailing_loc ~contents
  in
  match (distinct_fails reaches, any_pass) with
  | [], _ -> false
  | [ { formatted; shown } ], false ->
      record formatted;
      (match result with
      | Fail _ when not (accepting ()) ->
          add_failure
            (Failure.equality
               ~loc:(loc_t ~file:ctx.ctx_file ctx.ctx_trailing_loc)
               ~msg:"trailing output not matched by [%expect]" ~expected:""
               ~actual:shown ())
      | Fail _ | Pass -> ());
      true
  | _ ->
      let cr =
        cr_for_multiple_outputs ~output_name:"trailing output"
          ~outputs:(List.map (fun r -> r.raw) reaches)
          ()
      in
      record (format_pretty ~delimiter:(Tag "") ~node_column:insert_column cr);
      if not (accepting ()) then
        add_failure
          (Failure.equality
             ~loc:(loc_t ~file:ctx.ctx_file ctx.ctx_trailing_loc)
             ~msg:"trailing output not matched by [%expect]" ~expected:""
             ~actual:cr ());
      true

let run_expect_body ~file ~run ~sanitize ~nodes ~body_loc ~body_wrap
    ~trailing_loc body () =
  let nodes_array = Array.of_list nodes in
  let ctx =
    {
      ctx_file = file;
      ctx_sanitize = sanitize;
      ctx_nodes = nodes_array;
      ctx_results = Array.make (Array.length nodes_array) [];
      ctx_body_loc = body_loc;
      ctx_body_wrap = body_wrap;
      ctx_trailing_loc = trailing_loc;
    }
  in
  let saved = !state.current_expect in
  !state.current_expect <- Some ctx;
  (* Every expect failure is recorded below, after the body returns
     ([resolve_trailing]/[resolve_nodes]). So anything the frame gains
     during the body is something else — and [subtest] records a
     failure and carries on, so a body can return having already failed.
     The protocol's covered bit is what tells dune the run's failures are
     all promotable corrections; counting such a body as covered exits 0
     and invites [dune promote] to bless output the assertion says is
     wrong (Law 11: "masked assertion failures"). *)
  let failures_before = failure_count () in
  Fun.protect
    ~finally:(fun () -> !state.current_expect <- saved)
    (fun () ->
      match run body with
      | () ->
          (* Read before resolution records any expect failure of its own. *)
          let body_failed = failure_count () > failures_before in
          (* Trailing output not matched by any node becomes an inserted
             node; then per-node reachability. *)
          let trailing_problem =
            let raw =
              consume_output ctx ~pos:(pos_of ~file ctx.ctx_trailing_loc)
            in
            resolve_trailing ctx ~raw
          in
          let nodes_covered = resolve_nodes ctx ~check_reachability:true in
          if body_failed then record_covered `Not_covered
          else if (not trailing_problem) && not (had_problems ctx) then
            record_covered `No_problem
          else if nodes_covered then record_covered `Covered
          else record_covered `Not_covered
      | exception Failure.Skip_test reason ->
          (* A skipped expect test is an ordinary skip. Nothing
             is checked or recorded — not the nodes reached before the skip
             (their reaches die with this ctx), not trailing output, not
             reachability — so no correction ever exists for its nodes, no
             .corrected content is written, and the exit protocol never sees
             it (the runner classifies Skip outcomes out of the failed set). *)
          raise (Failure.Skip_test reason)
      | exception exn when Failure.is_fatal exn -> raise exn
      | exception exn ->
          (* An assertion, timeout, or any other uncaught exception is never
             a correction: resolve the reached nodes — their
             corrections are still recorded — and let the runner classify
             the exception. Nothing is spliced at the trailing point: a node
             inserted after a raising statement can never be reached on a
             future run (the correction could not converge under promote),
             and after a nonreturning tail statement it does not even
             compile (warning 21). Expected exceptions are ordinary code:
             catch and print, then [%expect]. *)
          let backtrace = Printexc.get_raw_backtrace () in
          ignore (resolve_nodes ctx ~check_reachability:false);
          record_covered `Not_covered;
          Printexc.raise_with_backtrace exn backtrace)

let add_expect_test ~file ~loc ~tags ~run ~sanitize ~nodes ~body_loc ~body_wrap
    ~trailing_loc name body =
  note_partition file;
  let name = scoped_name ~file name in
  register ~file
    (Test_tree.test ~pos:(pos_of ~file loc) ~tags name
       (run_expect_body ~file ~run ~sanitize ~nodes ~body_loc ~body_wrap
          ~trailing_loc body))

(* The inline runner driver *)

(* The thin inline driver: [Driver.execute_and_report] writes the whole
   transcript, shared byte-for-byte with the library runner (one behavior,
   both runners — ppx/F-4). The inline protocol has no CLI, so the
   WINDTRAP_* mirrors are the CLI: WINDTRAP_QUIET/WINDTRAP_VERBOSE pick the
   verbosity level and WINDTRAP_COVERAGE the inline coverage line (both
   resolved in [exit], beside the config, by the one [Cli.settings]
   call). What is
   legitimately this runner's own stays visible here: the [`Mirrors] hint
   context, the seedless and selectionless header (a mirror empties every
   partition it narrows, and [inline_exit_code] passes those runs — they
   are not the mistyped filter the sentence diagnoses), the .corrected
   files, and the returned exit code that [exit] combines with the
   correction protocol. *)
let run_inline_suite ~suite ~config ~coverage ~render ~output tests =
  let spine =
    {
      Driver.invocation = `Mirrors;
      seed = None;
      selection = None;
      github = Env.in_github_actions ();
      output;
      coverage;
      render;
      config;
      suite;
    }
  in
  match
    (* The mutation seam: one call at run entry, in place of the
       driver's. A mutation run's exit code is its own and never reports
       a test outcome, so [Reported] skips the correction protocol
       entirely: dune's promotion protocol is not what a mutation run is
       for, and Law 16(d) has already stopped every correction it could
       have recorded. *)
    Mutate_loop.execute_and_report spine tests
  with
  | Mutate_loop.Reported code -> code
  | Mutate_loop.Ran result -> (
      match result with
      | Error error ->
          (* The message is already on stderr; this runner returns the code
         for [exit] to combine with the correction protocol. *)
          Runner.startup_exit_code error
      | Ok outcome ->
          (* An inline partition is a suite like any other, and WINDTRAP_JUNIT
         is the only spelling that reaches it — the protocol has no CLI. It
         writes its own file under the directory form, which is what makes
         a report per partition possible at all. *)
          Option.iter
            (Driver.write_junit ~invocation:`Mirrors ~suite
               ~duration:outcome.Runner.duration
               ~results:(Run.results outcome.Runner.run))
            config.Run.junit;
          (* The update mode is read off the registry the runner built for
         this run, not re-resolved here: [Runner.startup] has already put
         WINDTRAP_UPDATE through [Snapshot.resolve_mode], so the CI
         refusal and the [force] override that governs snapshot baselines
         governs expect payloads by the same decision, made once. A run
         refused in CI never reaches this branch at all.

         Acceptance is further gated on this process's own verdict:
         [clean] means every failure this partition counted is covered by
         a correction — no assertion failed beside a stale payload,
         nothing crashed — which is the per-file half of the veto the
         masked-assertion rule (Law 11) exists for. Under dune one
         partition is one file, so the gate removes exactly the
         cross-file hostage-taking and nothing else: output produced
         beside a non-expect failure is never accepted, under
         WINDTRAP_UPDATE too. *)
          let clean = inline_exit_code outcome = 0 in
          let update =
            Snapshot.mode (Run.snapshots outcome.Runner.run) = Snapshot.Update
          in
          let { written; accepted; refused } =
            flush_corrections_report ~accept:(clean && update)
          in
          (* The correction-coverage exit-0 downgrade presumes the correction
         reached disk — dune's diff action can only surface corrections
         that exist, and neither does a source tree the drift guard
         refused to touch. A failed expect test whose correction was not
         written must exit nonzero (the write failure was reported
         above), or dune would record the partition as passed. *)
          let code = if refused = [] then if clean then 0 else 1 else 1 in
          (match
             correction_notice ~accepted ~refused
               ~declined:(update && not clean) written
           with
          | None -> ()
          | Some notice ->
              output_string Stdlib.stderr notice;
              flush Stdlib.stderr);
          code)

let exit () =
  if not !state.am_test_runner then Stdlib.exit 0;
  if !state.list_partitions_only then begin
    List.iter print_endline (partitions ());
    Stdlib.exit 0
  end;
  let tests = collect () in
  if tests = [] then Stdlib.exit 0;
  let suite = Option.value ~default:"inline tests" !state.current_lib in
  (* The WINDTRAP_* mirrors, resolved in the one call the library runner
     makes (minus the flags the inline protocol lacks): an invalid value in
     any of them is a refusal, never a silently defaulted run. *)
  match Cli.settings Cli.empty with
  | Error error ->
      prerr_endline (Cli.error_message error);
      Stdlib.exit 2
  | Ok { Cli.config; render; coverage; output_level } ->
      Stdlib.exit
        (run_inline_suite ~suite ~config ~coverage ~render
           ~output:output_level tests)

(* Test seams *)

(* Total by construction: [initial_state] is a record literal, so every
   field of [state] — present and future — is given a fresh initial value
   here. [initial_dir] is not state and is deliberately untouched, and
   neither is the undriven guard's claim: a process that pokes this seam
   owns the registry by construction, so [reset] claims it — otherwise
   the guard would diagnose registrations [reset] itself just threw
   away. *)
let reset () =
  claim_registry ();
  state := initial_state ()

(* The half of this module that only its own test suite calls. Generated
   code needs the registration, execution and protocol entry points and
   nothing else; these let test/unit check normalization, collection,
   correction formatting, the flush and the exit protocol as functions
   rather than as process transcripts. *)
module Private = struct
  type nonrec flush_report = flush_report = {
    written : string list;
    accepted : string list;
    refused : string list;
  }

  let normalize = normalize
  let collect = collect
  let partitions = partitions
  let corrected_source = corrected_source
  let flush_corrections_report = flush_corrections_report
  let inline_exit_code = inline_exit_code
  let correction_notice = correction_notice
  let reset = reset
end
