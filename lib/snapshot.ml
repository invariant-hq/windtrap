(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type pos = Failure.pos
type here = Failure.here
type mode = Check | Update

(* ───── String Helpers ───── *)

let trim_trailing_slashes s =
  let rec last_non_slash i =
    if i < 0 then -1
    else match s.[i] with '/' | '\\' -> last_non_slash (i - 1) | _ -> i
  in
  let i = last_non_slash (String.length s - 1) in
  if i < 0 then s else String.sub s 0 (i + 1)

let normalize_sep s = String.map (fun c -> if c = '\\' then '/' else c) s

let strip_dune_build_prefix path =
  let p = normalize_sep path in
  let comps = String.split_on_char '/' p in
  let rec drop acc = function
    | [] -> List.rev acc
    | "_build" :: _ :: rest -> List.rev acc @ rest
    | c :: rest -> drop (c :: acc) rest
  in
  String.concat "/" (drop [] comps)

let make_relative_to ~root abs_path =
  let root = normalize_sep (trim_trailing_slashes root) in
  let p = normalize_sep abs_path in
  let prefix = root ^ "/" in
  if String.starts_with ~prefix p then
    String.sub p (String.length prefix) (String.length p - String.length prefix)
  else p

let to_os_path p =
  if Filename.dir_sep = "/" then p
  else
    let sep = Filename.dir_sep.[0] in
    String.map (fun c -> if c = '/' then sep else c) p

let chop_extension_safe s =
  try Filename.chop_extension s with Invalid_argument _ -> s

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let write_file path contents =
  Path_ops.mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let ensure_trailing_newline s =
  if s = "" then "\n"
  else if s.[String.length s - 1] = '\n' then s
  else s ^ "\n"

(* ───── Config ───── *)

module Config = struct
  type t = {
    root_dir : string option;
    mode : mode;
    diff_context : int;
    max_bytes : int;
    report_updates : bool;
    (* Filters are stored in reverse application order so that [with_filter]
       is O(1) prepend. [filters] reverses before returning. *)
    filters_rev : (string -> string) list;
  }

  let default () =
    let mode =
      if Option.value (Env.update ()) ~default:false then Update else Check
    in
    let diff_context = Option.value ~default:3 (Env.snapshot_diff_context ()) in
    let max_bytes = Option.value ~default:8192 (Env.snapshot_max_bytes ()) in
    let report_updates = Option.value ~default:false (Env.snapshot_report ()) in
    {
      root_dir = Env.snapshot_dir ();
      mode;
      diff_context;
      max_bytes;
      report_updates;
      filters_rev = [];
    }

  let create ?root_dir ?mode ?diff_context ?max_bytes ?report_updates
      ?(filters = []) () =
    let base = default () in
    {
      root_dir = Option.fold ~none:base.root_dir ~some:Option.some root_dir;
      mode = Option.value ~default:base.mode mode;
      diff_context = Option.value ~default:base.diff_context diff_context;
      max_bytes = Option.value ~default:base.max_bytes max_bytes;
      report_updates = Option.value ~default:base.report_updates report_updates;
      filters_rev = List.rev filters;
    }

  let with_filter f t = { t with filters_rev = f :: t.filters_rev }
  let with_mode m t = { t with mode = m }
  let mode t = t.mode
  let diff_context t = t.diff_context
  let max_bytes t = t.max_bytes
  let report_updates t = t.report_updates
  let filters t = List.rev t.filters_rev
end

(* ───── Global Config ───── *)

(* Thread-safety: the runner sets this once before test execution begins
   (runner.ml). Tests read it but never write to it. This is safe as long as
   tests run sequentially. If parallel test execution is added, this must be
   replaced with thread-local storage or passed explicitly. *)
let global_config = ref (Config.default ())
let set_config c = global_config := c

(* ───── Canonicalization ───── *)

let canonicalize config s =
  let s = Text.normalize_newlines s in
  let s = List.fold_left (fun acc f -> f acc) s (Config.filters config) in
  ensure_trailing_newline s

(* ───── Myers Diff ───── *)

let line_count s =
  let parts = String.split_on_char '\n' s in
  match List.rev parts with
  | "" :: rev_rest -> List.length rev_rest
  | _ -> List.length parts

let unified_diff ~context ~expected_label ~actual_label expected actual =
  let max_total = line_count expected + line_count actual in
  if max_total > 1500 then
    Pp.str "Diff omitted (snapshot too large: %d lines total)." max_total
  else
    Myers.diff ~context ~expected_label ~actual_label expected actual

(* ───── Snapshot Path Resolution ───── *)

let location_parts ?here ?pos () =
  match (here, pos) with
  | Some h, _ ->
      let col = h.Lexing.pos_cnum - h.Lexing.pos_bol in
      (Some h.Lexing.pos_fname, Some h.Lexing.pos_lnum, Some col)
  | None, Some (file, line, col, _end_col) -> (Some file, Some line, Some col)
  | None, None -> (None, None, None)

let snapshot_file_path config ?here ?pos ?name () =
  let file_opt, line_opt, col_opt = location_parts ?here ?pos () in
  let has_loc =
    Option.is_some file_opt && Option.is_some line_opt && Option.is_some col_opt
  in
  let has_name = Option.is_some name in
  if (not has_loc) && not has_name then
    Failure.raise_failure ?here ?pos
      "snapshot requires ~pos:__POS__ (or ~here) or an explicit ~name";

  let prj = Path_ops.project_root () in

  let file_for_path = Option.value ~default:"_unknown.ml" file_opt in
  let abs_file =
    if Filename.is_relative file_for_path then
      Filename.concat (Sys.getcwd ()) file_for_path
    else file_for_path
  in
  let abs_file = strip_dune_build_prefix abs_file in

  let base =
    match name with
    | Some n -> Path_ops.sanitize_component n
    | None -> "snapshot"
  in
  let key =
    match (line_opt, col_opt, name) with
    | Some l, Some c, Some _ -> Pp.str "%s__L%d_C%d" base l c
    | Some l, Some c, None -> Pp.str "L%d_C%d" l c
    | None, _, Some _ -> base
    | _ -> base
  in

  let dir =
    match config.Config.root_dir with
    | Some root ->
        (* Centralized mode: root / rel_path_no_ext *)
        let rel =
          let rel0 = make_relative_to ~root:prj abs_file in
          if
            String.length rel0 > 0
            && (rel0.[0] = '/' || (String.length rel0 > 1 && rel0.[1] = ':'))
          then
            let h = Digest.to_hex (Digest.string (normalize_sep abs_file)) in
            "_external/" ^ h ^ "/" ^ Filename.basename abs_file
          else rel0
        in
        let rel_no_ext = chop_extension_safe rel in
        Filename.concat root (to_os_path rel_no_ext)
    | None ->
        (* Colocated mode: source_dir / __snapshots__ / source_basename *)
        let source_dir = Filename.dirname abs_file in
        let source_base = chop_extension_safe (Filename.basename abs_file) in
        let snap_dir = Filename.concat source_dir "__snapshots__" in
        Filename.concat snap_dir source_base
  in

  Filename.concat dir (key ^ ".snap")

(* ───── Snapshot Assertions ───── *)

let snapshot ?here ?pos ?name actual =
  let config = !global_config in
  let path = snapshot_file_path config ?here ?pos ?name () in
  let actual = canonicalize config actual in
  let max_bytes = Config.max_bytes config in
  match Config.mode config with
  | Update ->
      let existing =
        if Path_ops.file_exists path then Some (read_file path) else None
      in
      let existing = Option.map (canonicalize config) existing in
      if existing <> Some actual then begin
        write_file path actual;
        if Config.report_updates config then
          Pp.epr "Windtrap.Snapshot: %s %s@."
            (if Option.is_some existing then "updated" else "created")
            path
      end
  | Check ->
      if not (Path_ops.file_exists path) then begin
        if Env.in_ci () then
          Failure.raise_failure ?here ?pos
            (Pp.str
               "Snapshot file does not exist: %s@.Run tests locally with \
                WINDTRAP_UPDATE=1 to create it."
               path)
        else begin
          (* Auto-create missing snapshots outside CI *)
          write_file path actual;
          if Config.report_updates config then
            Pp.epr "Windtrap.Snapshot: created %s@." path
        end
      end
      else
        let expected_raw = read_file path in
        let expected = canonicalize config expected_raw in
        if expected <> actual then begin
          let diff_context = Config.diff_context config in
          let diff =
            if diff_context <= 0 then "Diff disabled."
            else
              unified_diff ~context:diff_context ~expected_label:path
                ~actual_label:"<actual>" expected actual
          in
          (* Don't pass ~expected/~actual - the diff already shows the difference *)
          Failure.raise_failure ?here ?pos
            (Pp.str "Snapshot mismatch: %s@.@.%s" path
               (Text.truncate_bytes_utf8 (max_bytes * 2) diff))
        end

let snapshot_pp ?here ?pos ?name pp v =
  snapshot ?here ?pos ?name (Pp.str "%a" pp v)

let snapshotf ?here ?pos ?name fmt =
  Format.kasprintf (fun s -> snapshot ?here ?pos ?name s) fmt
