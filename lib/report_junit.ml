(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Escaping *)

(* Past [Text.escape_controls], a well-formed UTF-8 sequence is outside XML
   1.0's [Char] production only as U+FFFE or U+FFFF: the escape leaves no
   control but TAB and LF, and a [Uchar.t] is never a surrogate. *)
let is_xml_char d =
  let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
  Uchar.utf_decode_is_valid d && c <> 0xFFFE && c <> 0xFFFF

(* [s] with each byte that [entity] names as its entity, and each sequence
   that is no XML character as U+FFFD. *)
let xml ~entity s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let n = Uchar.utf_decode_length d in
      (if not (is_xml_char d) then Buffer.add_string b "\u{FFFD}"
       else
         match entity s.[i] with
         | Some e -> Buffer.add_string b e
         | None -> Buffer.add_substring b s i n);
      loop (i + n)
    end
  in
  loop 0;
  Buffer.contents b

let text_entity = function
  | '&' -> Some "&amp;"
  | '<' -> Some "&lt;"
  | '>' -> Some "&gt;"
  | _ -> None

(* A parser normalizes a tab in an attribute value to a space. *)
let attr_entity = function
  | '"' -> Some "&quot;"
  | '\'' -> Some "&apos;"
  | '\t' -> Some "&#9;"
  | c -> text_entity c

let text s =
  let lines = String.split_on_char '\n' s in
  xml ~entity:text_entity
    (String.concat "\n" (List.map Text.escape_controls lines))

let attr s = xml ~entity:attr_entity (Text.escape_controls s)

(* Testcases *)

type child =
  | Failure of { message : string; body : string }
  | Skipped of string option
  | System_out of string

(* [children] is [None] for the empty-element tag of a pass; every other
   testcase has an end tag, even one whose failures are all its subtests'. *)
type testcase = {
  name : string;
  classname : string;
  time : float;
  children : child list option;
}

let failure ~invocation ~armed ~filter f =
  let body =
    Pp.str "%a"
      (fun ppf ->
        Report_sections.pp_failure ~ansi:false ~filter ~invocation ?armed ppf)
      f
  in
  Failure { message = Report_sections.headline f; body }

let system_out (tail : Failure.tail) =
  let omitted =
    if tail.omitted_bytes > 0 then
      strf "[%d earlier bytes omitted]\n" tail.omitted_bytes
    else ""
  in
  let log =
    match tail.log_path with
    | Some path -> strf "\nfull log: %s" (Os.display_artifact path)
    | None -> ""
  in
  System_out (omitted ^ tail.text ^ log)

(* A counted failure is the row's testcase, with the test's own failures and
   the first captured tail of any, then one testcase for each subtest
   failure. *)
let row_testcases ~suite ~failure (r : Run.result) =
  let name = Test_tree.path_to_string r.path in
  let groups =
    match List.rev r.path with [] -> [] | _ :: rev -> List.rev rev
  in
  let classname = String.concat "." (suite :: groups) in
  let testcase children = { name; classname; time = r.duration; children } in
  match r.outcome with
  | Failure.Pass when r.attempts > 1 ->
      (* JUnit has no state for a flaky pass, and a property on a testcase is
         not universally accepted, so the fact rides the one element every
         consumer allows there. *)
      [
        testcase (Some [ System_out (strf "passed on attempt %d" r.attempts) ]);
      ]
  | Failure.Pass -> [ testcase None ]
  | Failure.Skip reason -> [ testcase (Some [ Skipped reason ]) ]
  | Failure.Fail _ when not r.counted ->
      (* JUnit has no expected-failure state either. *)
      let message =
        match Option.bind r.xfail (fun (x : Test_tree.xfail) -> x.reason) with
        | Some reason -> "expected failure: " ^ reason
        | None -> "expected failure"
      in
      [ testcase (Some [ Skipped (Some message) ]) ]
  | Failure.Fail fs ->
      let subtests, own =
        List.partition Report_sections.is_subtest_failure fs
      in
      let tail = List.find_map (fun (f : Failure.t) -> f.output_tail) fs in
      let subtest f =
        match Report_sections.labeled_msg f with
        | Some label ->
            let children = Some [ failure ~filter:name f ] in
            { name = label; classname; time = 0.; children }
        | None -> assert false (* A subtest's failure always has a label. *)
      in
      testcase
        (Some
           (List.map (failure ~filter:name) own
           @ Option.to_list (Option.map system_out tail)))
      :: List.map subtest subtests

(* The document *)

let add_child b = function
  | Failure { message; body } ->
      Printf.bprintf b "      <failure message=\"%s\">%s</failure>\n"
        (attr message) (text body)
  | Skipped None -> Buffer.add_string b "      <skipped/>\n"
  | Skipped (Some message) ->
      Printf.bprintf b "      <skipped message=\"%s\"/>\n" (attr message)
  | System_out out ->
      Printf.bprintf b "      <system-out>%s</system-out>\n" (text out)

let add_testcase b t =
  Printf.bprintf b "    <testcase name=\"%s\" classname=\"%s\" time=\"%.3f\""
    (attr t.name) (attr t.classname) t.time;
  match t.children with
  | None -> Buffer.add_string b "/>\n"
  | Some children ->
      Buffer.add_string b ">\n";
      List.iter (add_child b) children;
      Buffer.add_string b "    </testcase>\n"

let count p testcases =
  let has t = List.exists p (Option.value ~default:[] t.children) in
  List.length (List.filter has testcases)

let render ~invocation ?armed ~suite ~results ~release_failures ~duration () =
  let failure = failure ~invocation ~armed in
  let rows = List.concat_map (row_testcases ~suite ~failure) results in
  (* A failed release is timed no more than a subtest is. *)
  let release f =
    let name = Report_sections.release_title in
    {
      name;
      classname = suite;
      time = 0.;
      children = Some [ failure ~filter:name f ];
    }
  in
  let testcases = rows @ List.map release release_failures in
  let counts =
    strf
      "tests=\"%d\" failures=\"%d\" errors=\"0\" skipped=\"%d\" time=\"%.3f\""
      (List.length testcases)
      (count
         (function Failure _ -> true | Skipped _ | System_out _ -> false)
         testcases)
      (count
         (function Skipped _ -> true | Failure _ | System_out _ -> false)
         testcases)
      duration
  in
  let b = Buffer.create 4096 in
  Buffer.add_string b "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  Printf.bprintf b "<testsuites name=\"windtrap\" %s>\n" counts;
  Printf.bprintf b "  <testsuite name=\"%s\" %s>\n" (attr suite) counts;
  List.iter (add_testcase b) testcases;
  Buffer.add_string b "  </testsuite>\n</testsuites>\n";
  Buffer.contents b

(* Writing *)

(* Under [dune runtest] each suite is a process of its own, so a directory
   target gives each its file where one fixed path would keep the last. *)
let names_file target = Filename.check_suffix target ".xml"

let path ~suite target =
  if names_file target then target
  else Filename.concat target (Os.sanitize_component suite ^ ".xml")

let write ~invocation ?armed ~suite ~duration ~results ~release_failures target
    =
  let file = path ~suite target in
  let document =
    render ~invocation ?armed ~suite ~results ~release_failures ~duration ()
  in
  try
    if not (names_file target) then Os.mkdir_p (Filename.dirname file);
    Os.atomic_write ~path:file document
  with (Sys_error _ | Unix.Unix_error _) as e ->
    Os.warn
      (strf "could not write JUnit report to %s: %s" (Os.display_path file)
         (Os.failure_reason ~path:file e))
