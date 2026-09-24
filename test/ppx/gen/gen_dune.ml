(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The golden rules of a rewriter's directory, written from its fixture list,
   and the normalization its rejection parity is diffed under.

   [gen_dune.exe rules DRIVER FILE...] prints the dune.inc of the directory
   whose files are [FILE...]. Every [X.ml] but the driver's own source and the
   [parity_*] fixtures is a golden fixture: DRIVER runs over it and dune diffs
   the output against [X.expected]. A [reject_*] fixture must exit 1 and its
   whole output, standard error included, is the golden; any other fixture
   must exit 0 and its standard output is the golden. A [FILE] with a
   directory part is a twin in the sibling rewriter's directory: each
   [reject_X.ml] present on both sides gets a parity rule diffing the two
   drivers' outputs under [normalize].

   [gen_dune.exe normalize FILE] prints [FILE] with the rewriter's name
   ([coverage], [mutate], and the words its messages spell it with) replaced
   by [NS], and with the width that name gives a located span erased: the end
   of a [characters A-B] range and the run of carets under the span. Two
   rejections that differ only by the attribute's namespace normalize to the
   same bytes. *)

let replace_all ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then
      Buffer.add_string b (String.sub s i (String.length s - i))
    else if String.sub s i n = sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let namespace_words =
  [ "coverage"; "Coverage"; "mutate"; "Mutate"; "mutation"; "Mutation" ]

(* [characters 18-35] -> [characters 18-]. *)
let erase_range_end line =
  let key = "characters " in
  let len = String.length line in
  let rec find i =
    if i > len - String.length key then None
    else if String.sub line i (String.length key) = key then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> line
  | Some i -> (
      match String.index_from_opt line (i + String.length key) '-' with
      | None -> line
      | Some dash ->
          let rec digits j =
            if j < len && match line.[j] with '0' .. '9' -> true | _ -> false
            then digits (j + 1)
            else j
          in
          let stop = digits (dash + 1) in
          String.sub line 0 (dash + 1) ^ String.sub line stop (len - stop))

(* A line of blanks then carets keeps its blanks and one caret. *)
let erase_caret_width line =
  let trimmed = String.trim line in
  if trimmed <> "" && String.for_all (fun c -> c = '^') trimmed then
    String.sub line 0 (String.index line '^') ^ "^"
  else line

let normalize text =
  let text =
    List.fold_left
      (fun text word -> replace_all ~sub:word ~by:"NS" text)
      text namespace_words
  in
  String.split_on_char '\n' text
  |> List.map (fun line -> erase_caret_width (erase_range_end line))
  |> String.concat "\n"

let read_file path =
  In_channel.with_open_bin path (fun ic -> In_channel.input_all ic)

let has_prefix ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* [form ~indent ~closers atoms] is the list of [atoms] as dune's formatter
   lays it out at [indent] columns, followed by [closers] parentheses: on one
   line when that line fits in 80 columns, one atom per line otherwise. *)
let form ~indent ~closers atoms =
  let pad = String.make indent ' ' in
  let line = "(" ^ String.concat " " atoms ^ ")" in
  let closing = String.make closers ')' in
  if indent + String.length line + closers <= 80 then pad ^ line ^ closing
  else pad ^ "(" ^ String.concat ("\n" ^ pad ^ " ") atoms ^ ")" ^ closing

let lines = String.concat "\n"

let diff_rule expected actual =
  lines
    [
      "(rule";
      " (alias runtest)";
      " (action";
      form ~indent:2 ~closers:2 [ "diff"; expected; actual ];
    ]

let golden_rule ~driver stem =
  let run =
    [ "run"; "./" ^ driver; "--impl"; Printf.sprintf "%%{dep:%s.ml}" stem ]
  in
  let produce =
    if has_prefix ~prefix:"reject_" stem then
      lines
        [
          "(rule";
          Printf.sprintf " (target %s.actual)" stem;
          " (action";
          "  (with-outputs-to";
          Printf.sprintf "   %s.actual" stem;
          "   (with-accepted-exit-codes";
          "    1";
          form ~indent:4 ~closers:4 run;
        ]
    else
      lines
        [
          "(rule";
          " (with-stdout-to";
          Printf.sprintf "  %s.actual" stem;
          form ~indent:2 ~closers:2 run;
        ]
  in
  lines [ produce; ""; diff_rule (stem ^ ".expected") (stem ^ ".actual") ]

let parity_rule ~twin_dir stem =
  let normalize target actual =
    lines
      [
        "(rule";
        " (with-stdout-to";
        "  " ^ target;
        form ~indent:2 ~closers:2
          [
            "run";
            "../gen/gen_dune.exe";
            "normalize";
            Printf.sprintf "%%{dep:%s}" actual;
          ];
      ]
  in
  lines
    [
      normalize (stem ^ ".here.parity") (stem ^ ".actual");
      "";
      normalize (stem ^ ".twin.parity")
        (Printf.sprintf "%s/%s.actual" twin_dir stem);
      "";
      diff_rule (stem ^ ".here.parity") (stem ^ ".twin.parity");
    ]

let rules ~driver files =
  let driver_stem = Filename.remove_extension driver in
  let ml_stems paths =
    List.filter_map
      (fun path ->
        if Filename.check_suffix path ".ml" then
          Some (Filename.remove_extension (Filename.basename path))
        else None)
      paths
    |> List.sort_uniq String.compare
  in
  let here, twins =
    List.partition (fun path -> Filename.dirname path = ".") files
  in
  let fixtures =
    List.filter
      (fun stem ->
        stem <> driver_stem && not (has_prefix ~prefix:"parity_" stem))
      (ml_stems here)
  in
  let twin_dir =
    match List.sort_uniq String.compare (List.map Filename.dirname twins) with
    | [] -> None
    | [ dir ] -> Some dir
    | dirs ->
        Printf.eprintf "gen_dune: twins in more than one directory: %s\n"
          (String.concat ", " dirs);
        exit 2
  in
  let parity =
    match twin_dir with
    | None -> []
    | Some twin_dir ->
        let twin_stems = ml_stems twins in
        List.filter_map
          (fun stem ->
            if has_prefix ~prefix:"reject_" stem && List.mem stem twin_stems
            then Some (parity_rule ~twin_dir stem)
            else None)
          fixtures
  in
  print_string
    "; Generated from this directory's fixture list by ../gen/gen_dune.exe:\n\
     ; do not edit. After adding or removing a fixture, regenerate with\n\
     ; dune build @gen-rules --auto-promote\n";
  List.iter
    (fun rule ->
      print_newline ();
      print_endline rule)
    (List.map (golden_rule ~driver) fixtures @ parity)

let () =
  match Array.to_list Sys.argv with
  | _ :: "rules" :: driver :: files -> rules ~driver files
  | [ _; "normalize"; file ] -> print_string (normalize (read_file file))
  | _ ->
      prerr_endline
        "usage: gen_dune.exe rules DRIVER FILE...\n\
        \       gen_dune.exe normalize FILE";
      exit 2
