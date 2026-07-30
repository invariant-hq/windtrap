(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The testsuites/testsuite/testcase structure adapts windtrap v1's
   progress.ml write_junit_xml, rebuilt over typed Failure payloads with
   XML 1.0 field sanitization (RFC v3 Law 4 commentary).
  ---------------------------------------------------------------------------*)

let spf = Printf.sprintf

(* [s] reduced to the XML 1.0 character range: every XML-invalid scalar
   value and every malformed UTF-8 byte becomes U+FFFD. *)
let xml_valid s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let d = String.get_utf_8_uchar s !i in
    let n = Uchar.utf_decode_length d in
    let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
    let xml_char =
      c = 0x9 || c = 0xA || c = 0xD
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

(* Element-content sanitization: ANSI-stripped, XML-1.0-ranged, escaped. *)
let text s =
  let s = xml_valid (Text.strip_ansi s) in
  let buf = Buffer.create (String.length s) in
  String.iter (escape_common buf) s;
  Buffer.contents buf

(* Attribute-value sanitization: as {!text}, plus quotes and the whitespace
   characters parsers would normalize away. *)
let attr s =
  let s = xml_valid (Text.strip_ansi s) in
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | '\n' -> Buffer.add_string buf "&#10;"
      | '\r' -> Buffer.add_string buf "&#13;"
      | '\t' -> Buffer.add_string buf "&#9;"
      | c -> escape_common buf c)
    s;
  Buffer.contents buf

let failure_text ~filter ~invocation f =
  Pp.str "%a"
    (fun ppf f -> Render.pp_failure ~ansi:false ~filter ~invocation ppf f)
    f

(* One [Fail] outcome, projected: an excused expected failure (amendment
   B12), or a counted failure with its entries partitioned into the test's
   own and its subtests' (amendment B13). *)
type fail_case =
  | Excused of Test_tree.xfail
  | Counted of { own : Failure.t list; subtests : Failure.t list }

let classify_fail ~excused (r : Run.result) fs =
  match List.assoc_opt (Test_tree.path_to_string r.path) excused with
  | Some xfail -> Excused xfail
  | None ->
      let subtests, own =
        List.partition (Render.is_subtest_failure ~path:r.path) fs
      in
      Counted { own; subtests }

let render_full ?(invocation = `Mirrors) ~excused ~suite ~results ~duration () =
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
          match classify_fail ~excused r fs with
          | Excused _ -> incr skipped
          | Counted { own; subtests } ->
              tests := !tests + List.length subtests;
              failures := !failures + List.length subtests;
              if own <> [] then incr failures))
    results;
  let buf = Buffer.create 4096 in
  let counts =
    spf "tests=\"%d\" failures=\"%d\" errors=\"0\" skipped=\"%d\" time=\"%.3f\""
      !tests !failures !skipped duration
  in
  Buffer.add_string buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  Buffer.add_string buf (spf "<testsuites name=\"windtrap\" %s>\n" counts);
  Buffer.add_string buf
    (spf "  <testsuite name=\"%s\" %s>\n" (attr suite) counts);
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
      let add_failure f =
        Buffer.add_string buf
          (spf "      <failure message=\"%s\">%s</failure>\n"
             (attr (Render.headline f))
             (text (failure_text ~filter:path_string ~invocation f)))
      in
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
              | Some p -> spf "\nfull log: %s" p
              | None -> ""
            in
            Buffer.add_string buf
              (spf "      <system-out>%s</system-out>\n"
                 (text (omitted ^ tail.text ^ log)))
        | None -> ()
      in
      match r.outcome with
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
          match classify_fail ~excused r fs with
          | Excused { Test_tree.reason } ->
              (* JUnit has no expected-failure state; the mapping (amendment
                 B12) is a skip whose message names the expectation. *)
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
                 testcase under the parent's classname (amendment B13). *)
              Buffer.add_string buf (open_case ^ ">\n");
              List.iter add_failure own;
              add_tail fs;
              Buffer.add_string buf "    </testcase>\n";
              List.iter
                (fun (f : Failure.t) ->
                  (* The name is the msg slot verbatim: the [parent › name]
                     label, plus the user's [?msg] suffix when the entry
                     carried one (the label cannot be split back out). *)
                  let name =
                    match f.Failure.msg with
                    | Some label -> label
                    | None ->
                        path_string (* unreachable: labels select subtests *)
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
  Buffer.add_string buf "  </testsuite>\n";
  Buffer.add_string buf "</testsuites>\n";
  Buffer.contents buf

let render ?invocation ~suite ~results ~duration () =
  render_full ?invocation ~excused:[] ~suite ~results ~duration ()
