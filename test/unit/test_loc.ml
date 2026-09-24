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
    test "capture skips a Camlinternal frame that calls it" (fun () ->
        (* The lazy's closure is [Loc.capture] itself, so the frame that calls
           it is CamlinternalLazy's; the first eligible frame is this
           line's. *)
        let p = __POS__ and l = Lazy.force (Lazy.from_fun Loc.capture) in
        match l with
        | None -> fail "capture under Lazy.force returns a location"
        | Some loc ->
            is_true ~msg:"the location is this file's" (in_this_file loc);
            equal ~msg:"the location is the force's line" int (line_of p)
              loc.Loc.line);
    test "capture counts an inlined frame" (fun () ->
        let[@inline always] here () = (__POS__, Loc.capture ()) in
        match here () with
        | p, Some loc ->
            equal ~msg:"the inlined function's line, not its caller's" int
              (line_of p) loc.Loc.line
        | _, None -> fail "capture in an inlined function returns a location");
    test "capture is None when every frame is the standard library's" (fun () ->
        is_true ~msg:"a domain whose stack holds no user frame"
          (Domain.join (Domain.spawn Loc.capture) = None));
    test "capture reads the 24 innermost entries only" (fun () ->
        (* [Seq.forever] calls [Loc.capture] from a Stdlib frame, and each
           [Seq.map] forces the one inside it from a Stdlib frame of its own:
           [depth] entries stand between the capture and this function. *)
        let under depth =
          let rec nest n s =
            if n = 0 then s else nest (n - 1) (Seq.map Fun.id s)
          in
          match (nest depth (Seq.forever Loc.capture)) () with
          | Seq.Cons (l, _) -> l
          | Seq.Nil -> assert false
        in
        is_true ~msg:"a user frame within reach is found"
          (Option.is_some (under 4));
        is_true ~msg:"a user frame beyond the 24th entry is not"
          (under 30 = None));
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
    test "delimit re-raises with the exception's backtrace" (fun () ->
        let recording = Printexc.backtrace_status () in
        Printexc.record_backtrace true;
        Fun.protect ~finally:(fun () -> Printexc.record_backtrace recording)
        @@ fun () ->
        let[@inline never] raiser () = raise (Stdlib.Failure "deep") in
        let raise_line = line_of __POS__ - 1 in
        match Loc.delimit (fun () -> ignore (raiser ())) with
        | () -> fail "delimit returned"
        | exception Stdlib.Failure _ ->
            let slots =
              Option.value ~default:[||]
                (Printexc.backtrace_slots (Printexc.get_raw_backtrace ()))
            in
            let raised_here slot =
              match Printexc.Slot.location slot with
              | Some l -> l.Printexc.line_number = raise_line
              | None -> false
            in
            is_true ~msg:"the backtrace reaches the raise inside fn"
              (Array.exists raised_here slots));
    test "own_unit names windtrap's units by whole name" (fun () ->
        List.iter
          (fun (name, own) -> equal ~msg:name bool own (Loc.own_unit name))
          [
            ("Windtrap", true);
            ("Windtrap.run", true);
            ("Windtrap__Check.raises", true);
            ("Windtrap_runtime", true);
            ("Windtrap_runtime.Coverage.hit", true);
            ("Windtrap_runtime__Mutate.arm", true);
            ("Windtrap_helpers.f", false);
            ("Windtrapper", false);
            ("Stdlib__List.map", false);
            ("Dune__exe__Test_loc.f", false);
            ("Dune__exe__Test_loc.Windtrap__Check", false);
          ]);
    test "resolve prefers ?__POS__ over the backtrace" (fun () ->
        (match Loc.resolve ~__POS__:("other.ml", 42, 7, 20) () with
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
        (* All three fields are load-bearing: a field [equal] ignored would
           make two distinct sites equal. *)
        is_false ~msg:"equal distinguishes columns"
          (Loc.equal loc (Loc.of_pos ("test/foo.ml", 12, 5, 9)));
        is_false ~msg:"equal distinguishes lines"
          (Loc.equal loc (Loc.of_pos ("test/foo.ml", 13, 4, 9)));
        is_false ~msg:"equal distinguishes files"
          (Loc.equal loc (Loc.of_pos ("test/zzz.ml", 12, 4, 9))));
  ]

let () = exit @@ Windtrap.run "loc" tests
