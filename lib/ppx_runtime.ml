(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The ordinary-OCaml half of ppx_windtrap. Protocol and sandbox mechanics
   (argv sniffing, partitions, .corrected files written to the module-load
   cwd) adapted from windtrap v1's lib/ppx_runtime.ml; payload matching,
   correction formatting, and the corrected-file writer follow ppx_expect
   at the pinned conformance commit (test/conformance/NOTICE; upstream
   runtime/{test_spec,test_node,output}.ml and the shapes its corpus
   goldens pin) — see the .mli for the contract. *)

(* Initialization *)

(* Captured eagerly at module load, before any test runs: tests may chdir,
   and .corrected files must land where dune's diff action looks — next to
   the copied source in the sandbox, the cwd the runner started in. *)
let initial_dir = Sys.getcwd ()

(* Protocol state *)

let initialized = ref false
let am_test_runner = ref false
let current_lib = ref None
let partition = ref None
let list_partitions_only = ref false

let init argv =
  if !initialized then ()
  else begin
    initialized := true;
    let rec parse = function
      | [] -> ()
      | "inline-test-runner" :: lib :: rest ->
          am_test_runner := true;
          current_lib := Some lib;
          parse rest
      | "-partition" :: name :: rest ->
          partition := Some name;
          parse rest
      | "-list-partitions" :: rest ->
          list_partitions_only := true;
          parse rest
      | ("-source-tree-root" | "-diff-cmd") :: _value :: rest ->
          (* Protocol arguments windtrap does not use: the value is consumed
             (it may itself look like a flag — dune passes [-diff-cmd -])
             and ignored. *)
          parse rest
      | _ :: rest -> parse rest
    in
    parse (Array.to_list argv)
  end

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

(* Registration *)

module String_set = Set.Make (String)

type group_frame = {
  group_name : string;
  group_tags : string list;
  group_file : string;
  group_names : (string, int ref) Hashtbl.t;
      (* sibling names used under this group, for duplicate renaming *)
  mutable children : Test_tree.t list;
}

let group_stack : group_frame list ref = ref []
let top_level : (string * Test_tree.t) list ref = ref []
let partitions_seen = ref String_set.empty

let note_partition file =
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
   registration order — and both run. *)
let top_names : (string * string, int ref) Hashtbl.t = Hashtbl.create 16

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
  | frame :: _ -> uniquify frame.group_names (fun n -> n) name

let register ~file tree =
  match !group_stack with
  | [] -> top_level := (file, tree) :: !top_level
  | frame :: _ -> frame.children <- tree :: frame.children

let add_test ~file ~loc ~tags name fn =
  note_partition file;
  let name = scoped_name ~file name in
  register ~file (Test_tree.test ~pos:(pos_of ~file loc) ~tags name fn)

let enter_group ~file ~tags name =
  note_partition file;
  let name = scoped_name ~file name in
  group_stack :=
    {
      group_name = name;
      group_tags = tags;
      group_file = file;
      group_names = Hashtbl.create 8;
      children = [];
    }
    :: !group_stack

let leave_group () =
  match !group_stack with
  | [] -> invalid_arg "Ppx_runtime.leave_group: no group is open"
  | frame :: rest ->
      group_stack := rest;
      register ~file:frame.group_file
        (Test_tree.group ~tags:frame.group_tags frame.group_name
           (List.rev frame.children))

(* Partition filtering happens at collection, not registration: init — which
   sets the partition — runs after the test modules have loaded. *)
let collect () =
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

let partitions () = String_set.elements !partitions_seen

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
  match indented with
  | [] -> []
  | _ ->
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

(* Node source rendering (the corrected-file shapes) *)

(* The shapes below reproduce the corrected files of the conformance
   corpus byte-for-byte: ppx_expect's runtime writes payload-only patches
   and Jane Street's style pass then standardizes every expect node of a
   corrected file — single-line payloads collapse onto the node's line,
   multi-line payloads put the extension head on its own line with the
   payload at node column + 2, and quoted payloads are re-escaped onto one
   line (or continuation-wrapped at the 90-column margin). windtrap folds
   both steps into one renderer. *)

let margin = 90

let escaped_segments contents =
  List.map String.escaped (String.split_on_char '\n' contents)

let quote_one_line segs = "\"" ^ String.concat "\\n" segs ^ "\""

(* One logical line of a wrapped quoted payload: [prefix] starts the
   source line ([<indent>"] on the first, [<indent>\] after), [closing]
   ends the segment ([\n\] between lines, ["]] on the last). A segment
   that overflows the margin is wrapped at space boundaries: the
   separating space stays before the continuation backslash and the
   remainder resumes at [col + 3] — ocamlformat's escaped-string layout,
   pinned by the corpus goldens. *)
let render_wrapped_segment buf ~col ~prefix ~closing seg =
  let rec fit prefix words =
    let all = String.concat " " words in
    let complete = String.length prefix + String.length all in
    if complete + String.length closing <= margin || List.length words <= 1 then begin
      Buffer.add_string buf prefix;
      Buffer.add_string buf all;
      Buffer.add_string buf closing
    end
    else begin
      (* Longest head fitting [prefix + head + space + backslash] in the
         margin, at least one word so the loop always progresses. *)
      let rec take acc words =
        match (acc, words) with
        | _, [] -> (Option.value ~default:"" acc, [])
        | acc, word :: rest ->
            let candidate =
              match acc with None -> word | Some a -> a ^ " " ^ word
            in
            if
              Option.is_some acc
              && String.length prefix + String.length candidate + 2 > margin
            then (Option.get acc, words)
            else take (Some candidate) rest
      in
      let head, rest = take None words in
      Buffer.add_string buf prefix;
      Buffer.add_string buf head;
      Buffer.add_string buf " \\\n";
      fit (spaces (col + 3)) rest
    end
  in
  fit prefix (String.split_on_char ' ' seg)

let render_quote_node ~name ~col contents =
  let segs = escaped_segments contents in
  let one = "[%" ^ name ^ " " ^ quote_one_line segs ^ "]" in
  if col + String.length one <= margin then one
  else begin
    let ind = spaces (col + 2) in
    let buf = Buffer.create 256 in
    Buffer.add_string buf ("[%" ^ name);
    let last = List.length segs - 1 in
    List.iteri
      (fun i seg ->
        Buffer.add_char buf '\n';
        let prefix = if i = 0 then ind ^ "\"" else ind ^ "\\" in
        let closing = if i = last then "\"]" else "\\n\\" in
        render_wrapped_segment buf ~col ~prefix ~closing seg)
      segs;
    Buffer.contents buf
  end

(* A styled node: everything the writer needs to re-render one expect
   node of a corrected file. Registered when the node's test resolves
   (skips never register). *)
type snode = {
  s_kind : node_kind;
  s_span : int * int;
  s_col : int;
  s_delim : delimiter;
  s_shorthand : bool;
  s_contents : string; (* original payload contents; "" when bare *)
}

let render_node sn contents =
  let name = extension_name sn.s_kind in
  match (sn.s_shorthand, sn.s_delim) with
  | true, Tag tag ->
      (* {%expect|…|} / {%expect tag|…|tag}: the correction keeps the
         extension id — retagging must never drop the [%expect] spelling. *)
      let tag = fix_tag ~contents tag in
      if tag = "" then "{%" ^ name ^ "|" ^ contents ^ "|}"
      else "{%" ^ name ^ " " ^ tag ^ "|" ^ contents ^ "|" ^ tag ^ "}"
  | true, Quote | false, Quote -> render_quote_node ~name ~col:sn.s_col contents
  | false, Tag tag ->
      let payload = tag_payload ~tag contents in
      if String.contains contents '\n' then
        "[%" ^ name ^ "\n" ^ spaces (sn.s_col + 2) ^ payload ^ "]"
      else "[%" ^ name ^ " " ^ payload ^ "]"

let snode_of ~file:_ node =
  let shorthand =
    match node.payload with
    | Some p ->
        p.literal_loc.start_pos <= node.loc.start_pos
        && p.literal_loc.end_pos >= node.loc.end_pos
    | None -> false
  in
  {
    s_kind = node.kind;
    s_span = (node.loc.start_pos, node.loc.end_pos);
    s_col = column node.loc;
    s_delim = node_delimiter node;
    s_shorthand = shorthand;
    s_contents =
      (match node.payload with Some { contents; _ } -> contents | None -> "");
  }

(* The corrections table *)

(* Everything recorded while tests run, keyed so that a node reached by
   several registrations of the same test (functor instantiation) has one
   slot that later resolutions replace, never duplicate. *)

type correction =
  | Node_fix of string (* corrected contents for the node at this span *)
  | Insert of { body_loc : loc; contents : string }
(* a trailing node inserted at the trailing point, plus the ";" at
   [body_loc]'s end *)

type correction_key =
  | Node_key of int * int (* node span *)
  | Insert_key of int (* trailing point *)

let corrections : (string, (correction_key, correction) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 16

let file_corrections file =
  match Hashtbl.find_opt corrections file with
  | Some tbl -> tbl
  | None ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.add corrections file tbl;
      tbl

let record_node_fix ~file node ~contents =
  Hashtbl.replace (file_corrections file)
    (Node_key (node.loc.start_pos, node.loc.end_pos))
    (Node_fix contents)

let record_insert ~file ~body_loc ~trailing_loc ~contents =
  Hashtbl.replace (file_corrections file) (Insert_key trailing_loc.start_pos)
    (Insert { body_loc; contents })

(* Styled nodes per file: every node of a resolved expect test, span-keyed.
   Only consulted for files that also have recorded corrections. *)
let styled : (string, (int * int, snode) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 16

let register_styled ~file node =
  let tbl =
    match Hashtbl.find_opt styled file with
    | Some tbl -> tbl
    | None ->
        let tbl = Hashtbl.create 16 in
        Hashtbl.add styled file tbl;
        tbl
  in
  let sn = snode_of ~file node in
  Hashtbl.replace tbl sn.s_span sn

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
  match Hashtbl.find_opt corrections file with
  | None -> None
  | Some tbl when Hashtbl.length tbl = 0 -> None
  | Some tbl ->
      let length = String.length source in
      let original (start, stop) =
        let start = max 0 (min length start) in
        let stop = max start (min length stop) in
        String.sub source start (stop - start)
      in
      let node_patches = ref [] and insert_patches = ref [] in
      (* Corrected nodes and, in the same corrected file, every other
         resolved node whose standardized rendering differs from its
         source — the style pass ppx_expect's corpus goldens include. *)
      (match Hashtbl.find_opt styled file with
      | None -> ()
      | Some nodes ->
          Hashtbl.iter
            (fun span sn ->
              let contents =
                match Hashtbl.find_opt tbl (Node_key (fst span, snd span)) with
                | Some (Node_fix contents) -> contents
                | _ -> (
                    match sn.s_kind with
                    | Expect ->
                        format_pretty ~delimiter:sn.s_delim
                          ~node_column:sn.s_col sn.s_contents
                    | Expect_exact -> sn.s_contents)
              in
              let text = render_node sn contents in
              if not (String.equal text (original span)) then
                node_patches :=
                  { start = fst span; stop = snd span; text } :: !node_patches)
            nodes);
      (* A corrected node whose test never resolved cannot happen (only
         resolutions record corrections), but a recorded Node_fix without
         a styled entry would be silently dropped above; keep the writer
         total by patching such spans directly. *)
      Hashtbl.iter
        (fun key correction ->
          match (key, correction) with
          | Node_key (start, stop), Node_fix contents ->
              if
                match Hashtbl.find_opt styled file with
                | Some nodes -> not (Hashtbl.mem nodes (start, stop))
                | None -> true
              then
                node_patches :=
                  { start; stop; text = tag_payload ~tag:"" contents }
                  :: !node_patches
          | Insert_key point, Insert { body_loc; contents } ->
              insert_patches :=
                ( {
                    start = body_loc.end_pos;
                    stop = body_loc.end_pos;
                    text = ";";
                  },
                  {
                    start = point;
                    stop = point;
                    text = render_insert ~body_loc contents;
                  } )
                :: !insert_patches
          | Node_key _, Insert _ | Insert_key _, Node_fix _ -> ())
        tbl;
      let patches =
        !node_patches
        @ List.concat_map (fun (semi, ins) -> [ semi; ins ]) !insert_patches
      in
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

(* One write attempt per file with recorded corrections. A file whose
   source cannot be read or whose target cannot be written is a flush
   failure: reported on stderr right here — a line naming the recorded
   source path and the OS reason — never skipped silently, and returned
   so the exit path can refuse to call the partition passed (a
   correction dune never sees cannot surface through its promotion
   diff). Returns the written target names and the failed sources. *)
let flush_corrections_report () =
  Sys.chdir initial_dir;
  let files = Hashtbl.fold (fun file _ acc -> file :: acc) corrections [] in
  let written = ref [] and failed = ref [] in
  List.iter
    (fun file ->
      let write () =
        match
          corrected_source ~file ~source:(read_file (absolute_path file))
        with
        | None -> ()
        | Some corrected ->
            let target = Filename.basename file ^ ".corrected" in
            let oc = open_out_bin target in
            Fun.protect
              ~finally:(fun () -> close_out_noerr oc)
              (fun () -> output_string oc corrected);
            written := target :: !written
      in
      match write () with
      | () -> ()
      | exception Sys_error reason ->
          Printf.eprintf "Error: correction for %s not written: %s\n%!" file
            reason;
          failed := file :: !failed)
    (List.sort compare files);
  Hashtbl.reset corrections;
  Hashtbl.reset styled;
  (List.rev !written, List.rev !failed)

let flush_corrections () = fst (flush_corrections_report ())

(* Coverage of failures by corrections (the exit protocol) *)

(* Paths of failed tests whose every failure is an expect mismatch with a
   recorded correction. Read by [inline_exit_code]. *)
let covered : (string list, bool) Hashtbl.t = Hashtbl.create 16

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
      let is_covered result =
        Option.value ~default:false (Hashtbl.find_opt covered result.Run.path)
      in
      if
        failed <> []
        && List.for_all is_covered failed
        && outcome.Runner.release_failures = []
      then 0
      else 1

(* The stderr trace of written corrections. Dune runs one sandboxed
   action per library — every partition's runner concurrently, then the
   per-file diff steps — and any nonzero runner exit fails the action,
   skips every diff, and deletes the sandbox with all computed
   .corrected files in it. No partition process can see a sibling's
   corrections (the runs race in a shared sandbox), so the only trace
   that survives a sibling's crash is a line each writing process
   prints for itself: hence the unconditional "wrote" line. A process
   that wrote corrections and itself exits nonzero adds the loud
   explanation, because its own corrections are the ones its own exit
   code just withheld. Under dune one partition is one file, so
   [written] has at most one element; the plural-safe spelling keeps
   by-hand multi-file runs honest. *)
let correction_notice ~exit_code written =
  match written with
  | [] -> None
  | files ->
      let notice = Buffer.create 128 in
      Printf.bprintf notice "windtrap: wrote %s\n" (String.concat ", " files);
      let n = List.length files in
      if exit_code <> 0 then
        Printf.bprintf notice
          "windtrap: %d correction%s written but NOT registered for promotion: \
           a failure above is not an expect mismatch, and dune drops every \
           correction in the library when any inline-test process fails. Fix \
           that failure, rerun, then 'dune promote'.\n"
          n
          (if n = 1 then "" else "s");
      Some (Buffer.contents notice)

(* Expect-test execution *)

type mismatch = { formatted : string; shown : string }
type reach_result = Pass | Fail of mismatch

(* One reach of a node: the sanitized output it consumed and the reconciled
   result. [raw] is kept because the multiple-outputs CR block lists every
   reach's raw output (ppx_expect's shape). *)
type reach = { raw : string; result : reach_result }

(* Merged reaches across every registration of the same node — a functor
   containing an expect test instantiated twice registers its nodes twice
   at the same span; ppx_expect accumulates both instances' results in one
   per-location slot and corrects from the merged history. Keys are
   (file, span); values are chronological. Bodies buffer reaches locally
   and publish here only at resolution, so a skipped body's reaches are
   never merged. *)
let node_pool : (string * int * int, reach list ref) Hashtbl.t =
  Hashtbl.create 16

(* Pools store newest-first; [reaches] arrives chronological. *)
let pool_publish key reaches =
  match Hashtbl.find_opt node_pool key with
  | Some existing -> existing := List.rev_append reaches !existing
  | None -> Hashtbl.add node_pool key (ref (List.rev reaches))

let pool_get key =
  match Hashtbl.find_opt node_pool key with
  | Some reaches -> List.rev !reaches
  | None -> []

type expect_ctx = {
  ctx_file : string;
  ctx_sanitize : string -> string;
  ctx_nodes : node array;
  ctx_results : reach list array; (* per node id, reverse reach order *)
  ctx_body_loc : loc;
  ctx_trailing_loc : loc;
}

let node_key ctx node = (ctx.ctx_file, node.loc.start_pos, node.loc.end_pos)

let trailing_key ctx =
  (ctx.ctx_file, ctx.ctx_trailing_loc.start_pos, -1 (* not a node span *))

let current_expect : expect_ctx option ref = ref None

let require_ctx op =
  match !current_expect with
  | Some ctx -> ctx
  | None ->
      invalid_arg
        (op
       ^ " only works inside a [let%expect_test] body executed by the test \
          runner")

let consume_output ctx ~pos =
  ctx.ctx_sanitize (Capture.output ~pos (Run.capture (Run.current ())))

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

let fail_node ctx node ~shown =
  Run.add_failure (Run.current_frame ())
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

(* Publish this body's reaches into the merged pools and register the
   nodes with the styled-writer registry. Reached nodes only: a node this
   body never reached contributes nothing (and an unreached node's stale
   formatting is left alone by the writer). *)
let publish_reaches ctx =
  Array.iter
    (fun node ->
      match ctx.ctx_results.(node.id) with
      | [] -> ()
      | local ->
          pool_publish (node_key ctx node) (List.rev local);
          register_styled ~file:ctx.ctx_file node)
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
            Run.add_failure (Run.current_frame ())
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
  let path = Run.path (Run.current_frame ()) in
  if value = `No_problem then Hashtbl.remove covered path
  else Hashtbl.replace covered path (value = `Covered)

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
      ~trailing_loc:ctx.ctx_trailing_loc ~contents
  in
  match (distinct_fails reaches, any_pass) with
  | [], _ -> false
  | [ { formatted; shown } ], false ->
      record formatted;
      (match result with
      | Fail _ ->
          Run.add_failure (Run.current_frame ())
            (Failure.equality
               ~loc:(loc_t ~file:ctx.ctx_file ctx.ctx_trailing_loc)
               ~msg:"trailing output not matched by [%expect]" ~expected:""
               ~actual:shown ())
      | Pass -> ());
      true
  | _ ->
      let cr =
        cr_for_multiple_outputs ~output_name:"trailing output"
          ~outputs:(List.map (fun r -> r.raw) reaches)
          ()
      in
      record (format_pretty ~delimiter:(Tag "") ~node_column:insert_column cr);
      Run.add_failure (Run.current_frame ())
        (Failure.equality
           ~loc:(loc_t ~file:ctx.ctx_file ctx.ctx_trailing_loc)
           ~msg:"trailing output not matched by [%expect]" ~expected:""
           ~actual:cr ());
      true

let run_expect_body ~file ~run ~sanitize ~nodes ~body_loc ~trailing_loc body ()
    =
  let nodes_array = Array.of_list nodes in
  Array.iteri
    (fun index node ->
      if node.id <> index then
        invalid_arg "Ppx_runtime: expect node ids must be 0, 1, … in order")
    nodes_array;
  let ctx =
    {
      ctx_file = file;
      ctx_sanitize = sanitize;
      ctx_nodes = nodes_array;
      ctx_results = Array.make (Array.length nodes_array) [];
      ctx_body_loc = body_loc;
      ctx_trailing_loc = trailing_loc;
    }
  in
  let saved = !current_expect in
  current_expect := Some ctx;
  Fun.protect
    ~finally:(fun () -> current_expect := saved)
    (fun () ->
      match run body with
      | () ->
          (* Trailing output not matched by any node becomes an inserted
             node; then per-node reachability. *)
          let trailing_problem =
            let raw =
              consume_output ctx ~pos:(pos_of ~file ctx.ctx_trailing_loc)
            in
            resolve_trailing ctx ~raw
          in
          let nodes_covered = resolve_nodes ctx ~check_reachability:true in
          if (not trailing_problem) && not (had_problems ctx) then
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

let add_expect_test ~file ~loc ~tags ~run ~sanitize ~nodes ~body_loc
    ~trailing_loc name body =
  note_partition file;
  let name = scoped_name ~file name in
  register ~file
    (Test_tree.test ~pos:(pos_of ~file loc) ~tags name
       (run_expect_body ~file ~run ~sanitize ~nodes ~body_loc ~trailing_loc body))

(* The inline runner driver *)

(* The thin inline driver: composed from Driver's shared producers — one
   behavior, both runners (ppx/F-4). The inline protocol has no CLI, so the
   WINDTRAP_* mirrors are the CLI: WINDTRAP_QUIET/WINDTRAP_VERBOSE pick the
   verbosity level and WINDTRAP_COVERAGE the coverage mode (resolved in
   [exit], beside the config). What is legitimately this runner's own stays
   visible here: the [`Mirrors] hint context, the seedless header, and the
   returned exit code that [exit] combines with the correction protocol. *)
let run_inline_suite ~suite ~config ~coverage_mode tests =
  let output = Cli.output_level Cli.empty in
  let github = Env.in_github_actions () in
  let renderer = Driver.renderer ~config ~mode:output ~invocation:`Mirrors () in
  let on_event = Driver.observe renderer ~seed:None in
  Driver.github_start ~github suite;
  match Runner.execute ~on_event ~config ~suite tests with
  | Error error ->
      Driver.github_end ~github;
      prerr_endline (Runner.startup_message error);
      Runner.startup_exit_code error
  | Ok outcome ->
      let results = Run.results outcome.Runner.run in
      let coverage_data = Driver.snapshot_coverage outcome.Runner.run in
      Render.finish renderer
        ?coverage:(Driver.coverage_summary ~coverage_mode outcome.Runner.run)
        ~results ~duration:outcome.Runner.duration ();
      Driver.coverage_report renderer ~coverage_mode outcome.Runner.run
        coverage_data;
      Driver.report_snapshots ~out:Format.std_formatter ~output
        ~invocation:`Mirrors outcome;
      Driver.github_end ~github;
      Driver.github_annotations ~github ~invocation:`Mirrors results;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      let written, unwritable = flush_corrections_report () in
      (* The correction-coverage exit-0 downgrade presumes the correction
         reached disk — dune's diff action can only surface corrections
         that exist. A failed expect test whose correction was not
         written must exit nonzero (the write failure was reported
         above), or dune would record the partition as passed. *)
      let code = if unwritable = [] then inline_exit_code outcome else 1 in
      (match correction_notice ~exit_code:code written with
      | None -> ()
      | Some notice ->
          output_string Stdlib.stderr notice;
          flush Stdlib.stderr);
      code

let exit () =
  if not !am_test_runner then Stdlib.exit 0;
  if !list_partitions_only then begin
    List.iter print_endline (partitions ());
    Stdlib.exit 0
  end;
  let tests = collect () in
  if tests = [] then Stdlib.exit 0;
  let suite = Option.value ~default:"inline tests" !current_lib in
  match Cli.resolve Cli.empty with
  | Error error ->
      prerr_endline (Cli.error_message error);
      Stdlib.exit 2
  | Ok config -> (
      (* The WINDTRAP_COVERAGE mirror, resolved like the library runner
         resolves it (minus the flag the inline protocol lacks): an invalid
         value is a refusal, never silently ignored. *)
      match Cli.coverage_mode Cli.empty with
      | Error error ->
          prerr_endline (Cli.error_message error);
          Stdlib.exit 2
      | Ok coverage_mode ->
          Stdlib.exit (run_inline_suite ~suite ~config ~coverage_mode tests))

(* Test seams *)

let reset () =
  initialized := false;
  am_test_runner := false;
  current_lib := None;
  partition := None;
  list_partitions_only := false;
  group_stack := [];
  top_level := [];
  Hashtbl.reset top_names;
  partitions_seen := String_set.empty;
  Hashtbl.reset corrections;
  Hashtbl.reset styled;
  Hashtbl.reset node_pool;
  Hashtbl.reset covered;
  current_expect := None
