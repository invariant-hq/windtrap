(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let failf fmt = Printf.ksprintf failwith fmt

let pp_int_list xs =
  xs |> List.map string_of_int |> String.concat "; " |> Printf.sprintf "[%s]"

let expect_int_list ~label expected actual =
  if expected <> actual then
    failf "%s: expected %s, got %s" label (pp_int_list expected)
      (pp_int_list actual)

let expect_bool ~label expected actual =
  if expected <> actual then
    failf "%s: expected %b, got %b" label expected actual

let pp_range_list xs =
  xs
  |> List.map (fun (s, e) -> Printf.sprintf "(%d, %d)" s e)
  |> String.concat "; "
  |> Printf.sprintf "[%s]"

let expect_range_list ~label expected actual =
  if expected <> actual then
    failf "%s: expected %s, got %s" label (pp_range_list expected)
      (pp_range_list actual)

let expect_string ~label expected actual =
  if expected <> actual then failf "%s: expected %S, got %S" label expected actual

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let test_reports_map_uncovered_lines () =
  let source_path = Filename.temp_file "windtrap-coverage" ".ml" in
  let source_dir = Filename.dirname source_path in
  let source_name = Filename.basename source_path in
  Fun.protect
    ~finally:(fun () -> Sys.remove source_path)
    (fun () ->
      write_file source_path "let a = 1\nlet b = 2\nlet c = 3\n";
      let coverage : Windtrap_coverage.coverage = Hashtbl.create 1 in
      Hashtbl.add coverage source_name
        {
          Windtrap_coverage.filename = source_name;
          points = [| 0; 10; 20; 21 |];
          counts = [| 1; 0; 0; 0 |];
        };
      let reports =
        Windtrap_coverage.reports ~source_paths:[ source_dir ] coverage
      in
      match reports with
      | [ report ] ->
          expect_bool ~label:"source_available" true report.source_available;
          expect_int_list ~label:"uncovered_offsets" [ 10; 20; 21 ]
            report.uncovered_offsets;
          expect_int_list ~label:"uncovered_lines" [ 2; 3 ] report.uncovered_lines
      | _ -> failwith "expected exactly one coverage report")

let test_reports_without_source () =
  let coverage : Windtrap_coverage.coverage = Hashtbl.create 1 in
  Hashtbl.add coverage "missing.ml"
    {
      Windtrap_coverage.filename = "missing.ml";
      points = [| 2; 8 |];
      counts = [| 0; 0 |];
    };
  let reports = Windtrap_coverage.reports coverage in
  match reports with
  | [ report ] ->
      expect_bool ~label:"source_available" false report.source_available;
      expect_int_list ~label:"uncovered_offsets" [ 2; 8 ] report.uncovered_offsets;
      expect_int_list ~label:"uncovered_lines" [] report.uncovered_lines
  | _ -> failwith "expected exactly one coverage report"

let test_collapse_ranges () =
  expect_range_list ~label:"empty" []
    (Windtrap_coverage.collapse_ranges []);
  expect_range_list ~label:"single" [ (5, 5) ]
    (Windtrap_coverage.collapse_ranges [ 5 ]);
  expect_range_list ~label:"contiguous" [ (1, 3) ]
    (Windtrap_coverage.collapse_ranges [ 1; 2; 3 ]);
  expect_range_list ~label:"two ranges" [ (1, 3); (7, 8) ]
    (Windtrap_coverage.collapse_ranges [ 1; 2; 3; 7; 8 ]);
  expect_range_list ~label:"gaps" [ (1, 1); (3, 3); (5, 5) ]
    (Windtrap_coverage.collapse_ranges [ 1; 3; 5 ])

let test_format_ranges () =
  expect_string ~label:"empty" ""
    (Windtrap_coverage.format_ranges []);
  expect_string ~label:"single line" "5"
    (Windtrap_coverage.format_ranges [ (5, 5) ]);
  expect_string ~label:"range" "1-3"
    (Windtrap_coverage.format_ranges [ (1, 3) ]);
  expect_string ~label:"mixed" "1-3, 7-8"
    (Windtrap_coverage.format_ranges [ (1, 3); (7, 8) ]);
  expect_string ~label:"singles and ranges" "1, 3-5, 9"
    (Windtrap_coverage.format_ranges [ (1, 1); (3, 5); (9, 9) ])

let () =
  test_reports_map_uncovered_lines ();
  test_reports_without_source ();
  test_collapse_ranges ();
  test_format_ranges ()
