(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Workflow-command emission adapts windtrap v1's progress.ml
   emit_github_annotation, rebuilt over typed Failure payloads with the
   documented data and property percent-encodings — the renderer owns its
   transport's validity.
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* Workflow-command data encoding: '%' first, then CR and LF. *)
let escape_data s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '%' -> Buffer.add_string buf "%25"
      | '\r' -> Buffer.add_string buf "%0D"
      | '\n' -> Buffer.add_string buf "%0A"
      | c -> Buffer.add_char buf c)
    (Text.strip_ansi s);
  Buffer.contents buf

(* Property values additionally encode the command delimiters. *)
let escape_property s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | ':' -> Buffer.add_string buf "%3A"
      | ',' -> Buffer.add_string buf "%2C"
      | c -> Buffer.add_char buf c)
    (escape_data s);
  Buffer.contents buf

let group_start name = spf "::group::%s\n" (escape_data name)
let group_end = "::endgroup::\n"

let rec drop_trailing_newlines s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\n' then
    drop_trailing_newlines (String.sub s 0 (len - 1))
  else s

let annotation ?(invocation = `Mirrors) ~path (f : Failure.t) =
  let path_string = Test_tree.path_to_string path in
  let location =
    match f.loc with
    | Some { Loc.file; line; _ } ->
        spf "file=%s,line=%d," (escape_property file) line
    | None -> ""
  in
  let title = escape_property (spf "Test failure: %s" path_string) in
  let message =
    drop_trailing_newlines
      (Pp.str "%a"
         (fun ppf f ->
           Render.pp_failure ~ansi:false ~filter:path_string ~invocation ppf f)
         f)
  in
  spf "::error %stitle=%s::%s\n" location title (escape_data message)

let annotations ?invocation results =
  let buf = Buffer.create 256 in
  List.iter
    (fun (r : Run.result) ->
      (* Counted failures only (the record's bit): an excused expected
         failure did not fail the run and annotates nothing. *)
      match r.outcome with
      | Failure.Fail fs when r.counted ->
          List.iter
            (fun f ->
              Buffer.add_string buf (annotation ?invocation ~path:r.path f))
            fs
      | Failure.Fail _ | Failure.Pass | Failure.Skip _ -> ())
    results;
  Buffer.contents buf
