(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The testsuites/testsuite/testcase structure adapts windtrap v1's
   progress.ml write_junit_xml, rebuilt over typed Failure payloads with
   XML 1.0 field sanitization — the renderer owns its transport's validity.
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* [s] reduced to the XML 1.0 character range: every XML-invalid scalar
   value and every malformed UTF-8 byte becomes U+FFFD. [s] has been
   through the report's escape, so TAB and LF are the only control bytes
   left. *)
let xml_valid s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let d = String.get_utf_8_uchar s !i in
    let n = Uchar.utf_decode_length d in
    let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let xml_char =
      c = 0x9 || c = 0xA
      || (c >= 0x20 && c <= 0xD7FF)
      || (c >= 0xE000 && c <= 0xFFFD)
      || (c >= 0x10000 && c <= 0x10FFFF)
    in
    if Uchar.utf_decode_is_valid d && xml_char then
      Buffer.add_substring buf s !i n
    else Buffer.add_string buf "\u{FFFD}";
    i := !i + n
  done;
  Buffer.contents buf

let escape_common buf c =
  match c with
  | '&' -> Buffer.add_string buf "&amp;"
  | '<' -> Buffer.add_string buf "&lt;"
  | '>' -> Buffer.add_string buf "&gt;"
  | c -> Buffer.add_char buf c

(* Element content: the report's escape line by line, XML-1.0-ranged,
   escaped. *)
let text s =
  let s =
    xml_valid
      (String.concat "\n"
         (List.map Text.escape_controls (String.split_on_char '\n' s)))
  in
  let buf = Buffer.create (String.length s) in
  String.iter (escape_common buf) s;
  Buffer.contents buf

(* Attribute value: one line, the report's escape over the whole of it,
   XML-1.0-ranged, escaped with the quotes and the tab that a parser would
   normalize away. *)
let attr s =
  let s = xml_valid (Text.escape_controls s) in
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | '\t' -> Buffer.add_string buf "&#9;"
      | c -> escape_common buf c)
    s;
  Buffer.contents buf

let failure_text ~filter ~invocation ~armed f =
  Pp.str "%a"
    (fun ppf f ->
      Report_sections.pp_failure ~ansi:false ~filter ~invocation ?armed ppf f)
    f

(* One [Fail] outcome, projected: an excused expected failure, or a counted
   failure with its entries partitioned into the test's own and its
   subtests'. Classification is record-driven:
   a failing result that did not count is excused, and the annotation it
   carries names the expectation. *)
type fail_case =
  | Excused of Test_tree.xfail
  | Counted of { own : Failure.t list; subtests : Failure.t list }

let classify_fail (r : Run.result) fs =
  if r.counted then
    let subtests, own = List.partition Report_sections.is_subtest_failure fs in
    Counted { own; subtests }
  else Excused (Option.value ~default:{ Test_tree.reason = None } r.xfail)

let render ?(invocation = `Mirrors) ?armed ~suite ~results ~release_failures
    ~duration () =
  (* Counts range over emitted testcases, not results: each subtest failure
     is its own testcase, and an excused failure is a skip. *)
  let tests = ref 0 and failures = ref 0 and skipped = ref 0 in
  List.iter
    (fun (r : Run.result) ->
      incr tests;
      match r.outcome with
      | Failure.Pass -> ()
      | Failure.Skip _ -> incr skipped
      | Failure.Fail fs -> (
          match classify_fail r fs with
          | Excused _ -> incr skipped
          | Counted { own; subtests } ->
              tests := !tests + List.length subtests;
              failures := !failures + List.length subtests;
              if own <> [] then incr failures))
    results;
  tests := !tests + List.length release_failures;
  failures := !failures + List.length release_failures;
  let buf = Buffer.create 4096 in
  let counts =
    spf "tests=\"%d\" failures=\"%d\" errors=\"0\" skipped=\"%d\" time=\"%.3f\""
      !tests !failures !skipped duration
  in
  Buffer.add_string buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  Buffer.add_string buf (spf "<testsuites name=\"windtrap\" %s>\n" counts);
  Buffer.add_string buf
    (spf "  <testsuite name=\"%s\" %s>\n" (attr suite) counts);
  let add_failure ~filter f =
    Buffer.add_string buf
      (spf "      <failure message=\"%s\">%s</failure>\n"
         (attr (Report_sections.headline f))
         (text (failure_text ~filter ~invocation ~armed f)))
  in
  List.iter
    (fun (r : Run.result) ->
      let path_string = Test_tree.path_to_string r.path in
      let groups =
        match List.rev r.path with [] -> [] | _ :: rev -> List.rev rev
      in
      let classname =
        match groups with
        | [] -> suite
        | gs -> suite ^ "." ^ String.concat "." gs
      in
      let open_case =
        spf "    <testcase name=\"%s\" classname=\"%s\" time=\"%.3f\""
          (attr path_string) (attr classname) r.duration
      in
      let add_failure = add_failure ~filter:path_string in
      let add_tail fs =
        match List.find_map (fun (f : Failure.t) -> f.output_tail) fs with
        | Some tail ->
            let omitted =
              if tail.omitted_bytes > 0 then
                spf "[%d earlier bytes omitted]\n" tail.omitted_bytes
              else ""
            in
            let log =
              match tail.log_path with
              | Some p -> spf "\nfull log: %s" (Os.display_artifact p)
              | None -> ""
            in
            Buffer.add_string buf
              (spf "      <system-out>%s</system-out>\n"
                 (text (omitted ^ tail.text ^ log)))
        | None -> ()
      in
      match r.outcome with
      | Failure.Pass when r.attempts > 1 ->
          (* A flaky pass: JUnit has no state for it, and a property on a
             testcase is not universally accepted, so the fact rides the
             one element every consumer allows there. *)
          Buffer.add_string buf (open_case ^ ">\n");
          Buffer.add_string buf
            (spf "      <system-out>%s</system-out>\n"
               (text (spf "passed on attempt %d" r.attempts)));
          Buffer.add_string buf "    </testcase>\n"
      | Failure.Pass -> Buffer.add_string buf (open_case ^ "/>\n")
      | Failure.Skip reason ->
          Buffer.add_string buf (open_case ^ ">\n");
          (match reason with
          | Some reason ->
              Buffer.add_string buf
                (spf "      <skipped message=\"%s\"/>\n" (attr reason))
          | None -> Buffer.add_string buf "      <skipped/>\n");
          Buffer.add_string buf "    </testcase>\n"
      | Failure.Fail fs -> (
          match classify_fail r fs with
          | Excused { Test_tree.reason } ->
              (* JUnit has no expected-failure state; the mapping is a skip
                 whose message names the expectation. *)
              let message =
                match reason with
                | Some reason -> "expected failure: " ^ reason
                | None -> "expected failure"
              in
              Buffer.add_string buf (open_case ^ ">\n");
              Buffer.add_string buf
                (spf "      <skipped message=\"%s\"/>\n" (attr message));
              Buffer.add_string buf "    </testcase>\n"
          | Counted { own; subtests } ->
              (* The parent testcase carries the test's own failures and its
                 captured tail; each subtest failure follows as a separate
                 testcase under the parent's classname. *)
              Buffer.add_string buf (open_case ^ ">\n");
              List.iter add_failure own;
              add_tail fs;
              Buffer.add_string buf "    </testcase>\n";
              List.iter
                (fun (f : Failure.t) ->
                  (* The name is the displayed label: the [parent › name]
                     components joined, plus the user's [?msg] suffix when
                     the entry carried one — the same spelling the terminal
                     block prints. *)
                  let name =
                    match Report_sections.labeled_msg f with
                    | Some label -> label
                    | None ->
                        path_string (* unreachable: subtests always label *)
                  in
                  Buffer.add_string buf
                    (spf
                       "    <testcase name=\"%s\" classname=\"%s\" \
                        time=\"0.000\">\n"
                       (attr name) (attr classname));
                  add_failure f;
                  Buffer.add_string buf "    </testcase>\n")
                subtests))
    results;
  (* A failed release is timed no more than a subtest is. *)
  List.iter
    (fun f ->
      let name = Report_sections.release_title in
      Buffer.add_string buf
        (spf "    <testcase name=\"%s\" classname=\"%s\" time=\"0.000\">\n"
           (attr name) (attr suite));
      add_failure ~filter:name f;
      Buffer.add_string buf "    </testcase>\n")
    release_failures;
  Buffer.add_string buf "  </testsuite>\n";
  Buffer.add_string buf "</testsuites>\n";
  Buffer.contents buf

(* Writing

   One process per suite is the normal case under `dune runtest` — a
   process per (test) stanza, and one per inline-test partition — so a
   single fixed path would have every suite overwrite the last, silently.
   A value naming an [.xml] file stays exactly that, for the one-process
   invocations `--junit` was written for; anything else is a directory,
   and each suite writes its own report into it for CI to glob. *)
let path ~suite target =
  if Filename.check_suffix target ".xml" then target
  else Filename.concat target (Os.sanitize_component suite ^ ".xml")

let write ~invocation ?armed ~suite ~duration ~results ~release_failures target
    =
  let file = path ~suite target in
  let document =
    render ~invocation ?armed ~suite ~results ~release_failures ~duration ()
  in
  match
    (* The directory form has to exist before the first suite writes into
       it, and nothing else creates it. The test is physical: [path] returns
       [target] itself for the file form and a fresh string otherwise. *)
    if file != target then Os.mkdir_p (Filename.dirname file);
    Os.atomic_write ~path:file document
  with
  | () -> ()
  | exception ((Sys_error _ | Unix.Unix_error _) as e) ->
      Os.warn
        (spf "could not write JUnit report to %s: %s" (Os.display_path file)
           (Os.failure_reason ~path:file e))
