(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Loc = Windtrap.Private.Loc

let line_of ((_, line, _, _) : Loc.pos) = line
let in_this_file (loc : Loc.t) = Filename.basename loc.Loc.file = "test_loc.ml"

let tests =
  [
    test "of_pos keeps file, line, and start column" (fun () ->
        let ((file, line, col, _) as p) = __POS__ in
        let loc = Loc.of_pos p in
        equal ~msg:"file" string file loc.Loc.file;
        equal ~msg:"line" int line loc.Loc.line;
        equal ~msg:"column" int col loc.Loc.column);
    test "capture attributes to the caller's frame" (fun () ->
        (* capture skips windtrap's own frames (Loc.capture itself is a
           windtrap frame). Both bindings sit on one line so the captured
           line number is known. *)
        let p = __POS__ and l = Loc.capture () in
        match l with
        | None -> fail "capture returns a location"
        | Some loc ->
            is_true ~msg:"capture attributes to the caller's file"
              (in_this_file loc);
            equal ~msg:"capture line is the call line" int (line_of p)
              loc.Loc.line);
    test "capture is immune to a handled exception's backtrace" (fun () ->
        (* capture walks the current stack, not the last exception's
           backtrace, so a caught-and-handled exception before the capture
           does not pollute the result. *)
        let boom () = failwith "boom" in
        let result =
          try boom ()
          with Stdlib.Failure _ ->
            let p = __POS__ and l = Loc.capture () in
            (p, l)
        in
        match result with
        | _, None -> fail "capture inside handler returns a location"
        | p, Some loc ->
            is_true ~msg:"capture inside handler attributes to this file"
              (in_this_file loc);
            equal ~msg:"capture uses the capture line, not the raise" int
              (line_of p) loc.Loc.line);
    test "capture through a stdlib higher-order call" (fun () ->
        (* stdlib frames (List.map) are never attributed; the closure in
           this file is. *)
        let results = List.map (fun () -> (__POS__, Loc.capture ())) [ () ] in
        match results with
        | [ (p, Some loc) ] ->
            is_true ~msg:"capture through List.map attributes to the closure"
              (in_this_file loc);
            equal ~msg:"capture through List.map line" int (line_of p)
              loc.Loc.line
        | [ (_, None) ] -> fail "capture through List.map returns a location"
        | _ -> fail "List.map shape");
    test "delimit stops capture instead of escaping the boundary" (fun () ->
        (* [f] tail-calls capture, so its own frame is gone at capture time;
           the walk must stop at the delimiter with None — never surface this
           test's frame beyond it. Pins delimiter recognition by defname: a
           toolchain or wrapping change that renames the frame fails here
           loudly. *)
        let f () = Loc.capture () in
        is_true ~msg:"capture under delimit with a consumed frame is None"
          (Loc.delimit f = None));
    test "capture below a delimiter still finds the user frame" (fun () ->
        (* Non-tail capture inside the delimited callback: the callback's
           frame is live, so the delimiter must not regress the working
           case. *)
        let p, l =
          Loc.delimit (fun () ->
              let p = __POS__ and l = Loc.capture () in
              (p, l))
        in
        match l with
        | None -> fail "capture below a delimiter returns a location"
        | Some loc ->
            is_true ~msg:"capture below a delimiter attributes to this file"
              (in_this_file loc);
            equal ~msg:"capture below a delimiter keeps the call line" int
              (line_of p) loc.Loc.line);
    test "delimit is transparent to values and exceptions" (fun () ->
        equal ~msg:"delimit returns fn's value" int 7
          (Loc.delimit (fun () -> 7));
        raises ~msg:"delimit re-raises fn's exception" (Stdlib.Failure "boom")
          (fun () -> Loc.delimit (fun () -> failwith "boom")));
    test "resolve prefers ?pos over the backtrace" (fun () ->
        (match Loc.resolve ~pos:("other.ml", 42, 7, 20) () with
        | Some loc ->
            equal ~msg:"resolve prefers pos file" string "other.ml" loc.Loc.file;
            equal ~msg:"resolve prefers pos line" int 42 loc.Loc.line;
            equal ~msg:"resolve keeps pos column" int 7 loc.Loc.column
        | None -> fail "resolve with pos is Some");
        let p = __POS__ and l = Loc.resolve () in
        match l with
        | Some loc ->
            is_true ~msg:"resolve without pos captures" (in_this_file loc);
            equal ~msg:"resolve without pos captures the call line" int
              (line_of p) loc.Loc.line
        | None -> fail "resolve without pos is Some");
    test "observers" (fun () ->
        let loc = Loc.of_pos ("test/foo.ml", 12, 4, 9) in
        equal ~msg:"to_string formats file:line" string "test/foo.ml:12"
          (Loc.to_string loc);
        is_true ~msg:"equal reflexive" (Loc.equal loc loc);
        is_false ~msg:"equal distinguishes columns"
          (Loc.equal loc (Loc.of_pos ("test/foo.ml", 12, 5, 9)));
        is_true ~msg:"compare equal is 0" (Loc.compare loc loc = 0);
        is_true ~msg:"compare orders by line"
          (Loc.compare loc (Loc.of_pos ("test/foo.ml", 13, 0, 0)) < 0);
        is_true ~msg:"compare orders by file first"
          (Loc.compare loc (Loc.of_pos ("test/zzz.ml", 1, 0, 0)) < 0));
  ]
