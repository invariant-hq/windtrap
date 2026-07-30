(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Stands in for an instrumented test executable: registers a synthetic
   point table exactly as a PPX-generated per-module initializer would
   (the frozen interchange contract — [register ~file ~points ~counts]),
   then runs a real windtrap suite, so the Law-12 seam, the inline
   coverage line, and the report/full modes are exercised through the
   real facade and exit path. Driven by environment variables:

   CHILD_FILE      source file to register (default "lib/fake.ml")
   CHILD_TOTAL     block count; 0 skips registration (default 10)
   CHILD_VISITED   how many leading blocks count as visited (default 0)
   CHILD_LINE_LEN  bytes per source line, newline included (default 10):
                   block [i] spans line [i + 1] minus its newline
   CHILD_FAIL      "1" adds a failing test

   The remaining argv is windtrap's (--quiet, --color, --coverage, ...).
   Callers must set WINDTRAP_COVERAGE_FILE: the registration installs the
   at_exit dump, which must never land in the real _build/_coverage. *)

let env name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

let () =
  let total = int_of_string (env "CHILD_TOTAL" "10") in
  let visited = int_of_string (env "CHILD_VISITED" "0") in
  let line_len = int_of_string (env "CHILD_LINE_LEN" "10") in
  let file = env "CHILD_FILE" "lib/fake.ml" in
  if total > 0 then begin
    let points =
      Array.init total (fun i ->
          {
            Windtrap_coverage.start_ofs = i * line_len;
            end_ofs = (i * line_len) + line_len - 1;
          })
    in
    let counts = Array.init total (fun i -> if i < visited then 1 else 0) in
    Windtrap_coverage.register ~file ~points ~counts
  end;
  let tests =
    Windtrap.test "passes" (fun () -> Windtrap.is_true true)
    ::
    (if env "CHILD_FAIL" "0" = "1" then
       [ Windtrap.test "fails" (fun () -> Windtrap.is_true false) ]
     else [])
  in
  Windtrap.run "coverage-child" tests
