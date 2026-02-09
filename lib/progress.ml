(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Constants ───── *)

let () = Env.setup_color ()
let in_ci = Env.in_ci
let in_github_actions = Env.in_github_actions
let pass_tag = "PASS"
let fail_tag = "FAIL"
let skip_tag = "SKIP"
let tag_width = 4
let compact_line_width = 60
let slowest_count = 5
let slowest_threshold = 5.0

(* ───── Types ───── *)

type mode = Compact | Verbose | Tap | Junit

type test_result =
  | Pass of { time : float; attempts : int }
  | Fail of {
      time : float;
      failure : Failure.t;
      output_file : string option;
      attempts : int;
    }
  | Skip of { reason : string option }

type junit_result = {
  jr_path : string;
  jr_groups : string list;
  jr_result : test_result;
}

type t = {
  mode : mode;
  is_tty : bool;
  total_tests : int;
  mutable tests_so_far : int;
  mutable total_time : float;
  mutable failures : (string * Failure.t * string option) list;
  mutable printed_groups : string list;
  mutable junit_results : junit_result list;
  mutable junit_file : string option;
  mutable slowest : (float * string) list;
}

(* ───── Construction ───── *)

let create ~mode ~total_tests =
  {
    mode;
    is_tty = Lazy.force Env.is_tty_stdout;
    total_tests;
    tests_so_far = 0;
    total_time = 0.0;
    failures = [];
    printed_groups = [];
    junit_results = [];
    junit_file = None;
    slowest = [];
  }

let set_junit_file t file = t.junit_file <- Some file
let failed_paths t = List.rev_map (fun (path, _, _) -> path) t.failures

(* ───── Header And Formatting ───── *)

let print_header t ~name ~run_id:_ =
  match t.mode with
  | Tap -> Pp.pr "TAP version 13@."
  | Junit -> ()
  | Verbose | Compact ->
      if in_github_actions () then Pp.pr "::group::%s@." name;
      Pp.pr "Testing %a.@.@." (Pp.styled `Bold Pp.string) name

let print_rule width =
  let rule_width = min width 60 in
  let buf = Buffer.create (rule_width * 3) in
  for _ = 1 to rule_width do
    Buffer.add_string buf "─"
  done;
  Pp.pr "%a@." (Pp.styled `Faint Pp.string) (Buffer.contents buf)

let pp_time ppf = function
  | t when t < 0.001 -> Pp.pf ppf "%dμs" (int_of_float (t *. 1_000_000.0))
  | t when t < 1.0 -> Pp.pf ppf "%.0fms" (t *. 1000.0)
  | t when t < 60.0 -> Pp.pf ppf "%.2fs" t
  | t ->
      let mins = int_of_float (t /. 60.0) in
      let secs = t -. (float_of_int mins *. 60.0) in
      Pp.pf ppf "%dm%.0fs" mins secs

(* ───── Result Reporting ───── *)

let extract_test_name path =
  let sep = " › " in
  let sep_len = String.length sep in
  let rec find_last_sep pos =
    if pos < 0 then None
    else if
      pos + sep_len <= String.length path && String.sub path pos sep_len = sep
    then Some pos
    else find_last_sep (pos - 1)
  in
  match find_last_sep (String.length path - sep_len) with
  | Some i -> String.sub path (i + sep_len) (String.length path - i - sep_len)
  | None -> path

let report_start t ~path ~groups:_ =
  match t.mode with
  | Verbose when t.is_tty && t.total_tests > 0 ->
      let test_name = extract_test_name path in
      Pp.pr "\r\027[2K%a"
        (Pp.styled `Faint Pp.string)
        (Pp.str "  Running [%d/%d] %s..." (t.tests_so_far + 1) t.total_tests
           test_name);
      Pp.flush Format.std_formatter ()
  | _ -> ()

let collect_failure t ~path result =
  match result with
  | Fail { failure; output_file; _ } ->
      t.failures <- (path, failure, output_file) :: t.failures
  | Pass _ | Skip _ -> ()

(* Maintain a descending-sorted list of the N slowest tests. *)
let record_slowest t ~path time =
  let entry = (time, path) in
  let rec insert = function
    | [] -> [ entry ]
    | ((t', _) as x) :: rest ->
        if time > t' then entry :: x :: rest else x :: insert rest
  in
  let updated = insert t.slowest in
  t.slowest <-
    (if List.length updated > slowest_count then
       List.filteri (fun i _ -> i < slowest_count) updated
     else updated)

let result_time = function
  | Pass { time; _ } | Fail { time; _ } -> time
  | Skip _ -> 0.0

let result_attempts = function
  | Pass { attempts; _ } | Fail { attempts; _ } -> attempts
  | Skip _ -> 1

(* Only print group headers that differ from the previously printed groups,
   avoiding redundant output when consecutive tests share parent groups. *)
let print_group_headers t groups =
  let rec find_common_prefix printed groups =
    match (printed, groups) with
    | p :: ps, g :: gs when p = g -> g :: find_common_prefix ps gs
    | _ -> []
  in
  let common = find_common_prefix t.printed_groups groups in
  let common_len = List.length common in
  let new_groups = List.filteri (fun i _ -> i >= common_len) groups in
  List.iteri
    (fun i name ->
      let depth = common_len + i in
      let indent = String.make (depth * 2) ' ' in
      Pp.pr "%s%a %a@." indent
        (Pp.styled `Cyan Pp.string)
        "›"
        (Pp.styled `Cyan Pp.string)
        name)
    new_groups;
  t.printed_groups <- groups

let report_result t ~path ~groups result =
  t.tests_so_far <- t.tests_so_far + 1;
  t.total_time <- t.total_time +. result_time result;
  (match result with
  | Pass { time; _ } | Fail { time; _ } -> record_slowest t ~path time
  | Skip _ -> ());
  match t.mode with
  | Compact ->
      let style, c =
        match result with
        | Pass _ -> (`Green, '.')
        | Fail _ -> (`Red, 'F')
        | Skip _ -> (`Yellow, 'S')
      in
      Pp.pr "%a" (Pp.styled style Pp.char) c;
      if t.tests_so_far mod compact_line_width = 0 then
        if t.total_tests > 0 then
          Pp.pr " %a@."
            (Pp.styled `Faint Pp.string)
            (Pp.str "[%d/%d]" t.tests_so_far t.total_tests)
        else Pp.pr "@.";
      Pp.flush Format.std_formatter ();
      collect_failure t ~path result
  | Verbose ->
      if t.is_tty then Pp.pr "\r\027[2K";
      print_group_headers t groups;
      let depth = List.length groups in
      let indent = String.make (depth * 2) ' ' in
      let test_name = extract_test_name path in
      let tag, style =
        match result with
        | Pass _ -> (pass_tag, `Green)
        | Fail _ -> (fail_tag, `Red)
        | Skip _ -> (skip_tag, `Yellow)
      in
      let attempts = result_attempts result in
      let time_str =
        match result with
        | Skip _ -> ""
        | _ ->
            if attempts > 1 then
              Pp.str " %a (%d attempts)" pp_time (result_time result) attempts
            else Pp.str " %a" pp_time (result_time result)
      in
      if time_str = "" then
        Pp.pr "%s%a %s@." indent (Pp.styled style Pp.string) tag test_name
      else
        Pp.pr "%s%a %s%a@." indent
          (Pp.styled style Pp.string)
          tag test_name
          (Pp.styled `Faint Pp.string)
          time_str;
      Pp.flush Format.std_formatter ();
      collect_failure t ~path result
  | Tap ->
      (match result with
      | Pass _ -> Pp.pr "ok %d - %s@." t.tests_so_far path
      | Fail { failure; _ } ->
          Pp.pr "not ok %d - %s@." t.tests_so_far path;
          let msg =
            String.map (function '\n' -> ' ' | c -> c) failure.message
          in
          Pp.pr "# %s@." msg
      | Skip { reason } -> (
          match reason with
          | Some r -> Pp.pr "ok %d - %s # SKIP %s@." t.tests_so_far path r
          | None -> Pp.pr "ok %d - %s # SKIP@." t.tests_so_far path));
      Pp.flush Format.std_formatter ();
      collect_failure t ~path result
  | Junit ->
      t.junit_results <-
        { jr_path = path; jr_groups = groups; jr_result = result }
        :: t.junit_results

(* ───── Failure Display ───── *)

let print_failure_header ~tag ~path =
  Pp.pr "@.%a %a@." (Pp.styled `Red Pp.string) tag
    (Pp.styled `Bold Pp.string)
    path

let tail_errors_limit = Env.tail_errors ()

let read_tail ~limit path =
  try
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let lines = ref [] in
        (try
           while true do
             lines := input_line ic :: !lines
           done
         with End_of_file -> ());
        let all_lines = List.rev !lines in
        let total = List.length all_lines in
        match limit with
        | None -> (all_lines, 0)
        | Some n when total <= n -> (all_lines, 0)
        | Some n ->
            let selected = List.filteri (fun i _ -> i >= total - n) all_lines in
            (selected, total - n))
  with Sys_error _ -> ([], 0)

let print_output_tail ~limit output_file =
  match output_file with
  | None -> ()
  | Some path when not (Sys.file_exists path) -> ()
  | Some path ->
      (* Intentionally suppress captured-output rendering for failures.
         We still read the tail to preserve current plumbing and avoid dead-code
         warnings until this path is removed entirely. *)
      ignore (read_tail ~limit path)

let ends_with_newline s =
  let len = String.length s in
  len > 0 && s.[len - 1] = '\n'

(* ───── GitHub Actions Integration ───── *)

(* Emit a workflow command so failures appear as annotations in the PR diff. *)
let emit_github_annotation ~path (failure : Failure.t) =
  let file, line =
    match failure.location with
    | Failure.No_location -> (None, None)
    | Failure.Pos (file, line, _, _) -> (Some file, Some line)
    | Failure.Here { pos_fname; pos_lnum; _ } -> (Some pos_fname, Some pos_lnum)
  in
  let location_part =
    match (file, line) with
    | Some f, Some l -> Printf.sprintf "file=%s,line=%d" f l
    | Some f, None -> Printf.sprintf "file=%s" f
    | None, _ -> ""
  in
  let title = Printf.sprintf "Test failure: %s" path in
  if location_part = "" then
    Pp.pr "::error title=%s::%s@." title failure.message
  else Pp.pr "::error %s,title=%s::%s@." location_part title failure.message

(* ───── JUnit XML Output ───── *)

let escape_xml s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '&' -> Buffer.add_string buf "&amp;"
      | '"' -> Buffer.add_string buf "&quot;"
      | '\'' -> Buffer.add_string buf "&apos;"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let write_junit_xml t ~passed ~failed ~skipped ~time =
  match t.junit_file with
  | None -> ()
  | Some path ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () ->
          let total = passed + failed + skipped in
          Printf.fprintf oc
            {|<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="Windtrap" tests="%d" failures="%d" skipped="%d" time="%.3f">
  <testsuite name="Tests" tests="%d" failures="%d" skipped="%d" time="%.3f">
|}
            total failed skipped time total failed skipped time;
          List.iter
            (fun { jr_path; jr_groups; jr_result } ->
              let test_name = escape_xml jr_path in
              let classname =
                escape_xml
                  (if jr_groups = [] then "windtrap"
                   else String.concat "." jr_groups)
              in
              match jr_result with
              | Pass { time; _ } ->
                  Printf.fprintf oc
                    {|    <testcase name="%s" classname="%s" time="%.3f"/>
|}
                    test_name classname time
              | Skip { reason } ->
                  let reason_str =
                    Option.fold ~none:"" ~some:escape_xml reason
                  in
                  Printf.fprintf oc
                    {|    <testcase name="%s" classname="%s" time="0">
      <skipped message="%s"/>
    </testcase>
|}
                    test_name classname reason_str
              | Fail { time; failure; _ } ->
                  Printf.fprintf oc
                    {|    <testcase name="%s" classname="%s" time="%.3f">
      <failure type="AssertionError" message="%s"/>
    </testcase>
|}
                    test_name classname time
                    (escape_xml failure.message))
            (List.rev t.junit_results);
          Printf.fprintf oc {|  </testsuite>
</testsuites>
|})

(* ───── Finish And Summary ───── *)

let finish t =
  (match (t.mode, t.tests_so_far mod compact_line_width) with
  | Compact, n when n <> 0 -> Pp.pr "@."
  | Tap, _ -> ()
  | Junit, _ -> ()
  | _ -> ());
  (match t.mode with Tap | Junit -> () | Verbose | Compact -> Pp.pr "@.");
  match t.mode with
  | Tap | Junit -> ()
  | Verbose | Compact -> (
      let failures = List.rev t.failures in
      match failures with
      | [] -> ()
      | failures ->
          print_rule 80;
          List.iter
            (fun (path, failure, output_file) ->
              if in_github_actions () then emit_github_annotation ~path failure;
              print_failure_header ~tag:fail_tag ~path;
              Pp.pr "%a@." Failure.pp failure;
              print_output_tail ~limit:tail_errors_limit output_file;
              if not (ends_with_newline failure.message) then Pp.pr "@.";
              print_rule 80)
            failures)

let total_time t = t.total_time

let print_slowest t =
  if
    t.slowest <> []
    && t.total_time >= slowest_threshold
    && t.tests_so_far >= slowest_count
  then begin
    Pp.pr "@.%a@." (Pp.styled `Faint Pp.string) "Slowest tests:";
    List.iter
      (fun (time, path) ->
        Pp.pr "  %a  %s@." (Pp.styled `Faint pp_time) time path)
      t.slowest
  end

let print_summary t ~passed ~failed ~skipped ~time () =
  write_junit_xml t ~passed ~failed ~skipped ~time;
  match t.mode with
  | Junit -> ()
  | Tap ->
      let total = passed + failed + skipped in
      Pp.pr "1..%d@." total
  | Verbose | Compact ->
      let total = passed + failed + skipped in
      let time_str = Pp.str "%a" pp_time time in
      let plural = if total = 1 then "" else "s" in
      let skip_suffix =
        if skipped > 0 then Printf.sprintf " (%d skipped)" skipped else ""
      in
      if failed > 0 then
        Pp.pr "%a in %s. %d test%s run.%s@."
          (Pp.styled `Bold (Pp.styled `Red Pp.string))
          (Printf.sprintf "%d failed" failed)
          time_str total plural skip_suffix
      else
        Pp.pr "%a in %s. %d test%s run.%s@."
          (Pp.styled `Bold (Pp.styled `Green Pp.string))
          "All tests passed" time_str total plural skip_suffix;
      print_slowest t;
      let cov = Windtrap_coverage.data () in
      if Hashtbl.length cov > 0 then begin
        let s = Windtrap_coverage.summarize cov in
        let pct = Windtrap_coverage.percentage s in
        let style = (Windtrap_coverage.coverage_style s :> Pp.style) in
        Pp.pr "Coverage: %d/%d (%a) of expressions.@." s.visited s.total
          (Pp.styled style Pp.string)
          (Printf.sprintf "%.2f%%" pct)
      end;
      if in_github_actions () then Pp.pr "::endgroup::@."
