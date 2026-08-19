(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The compiled mirror of doc/cookbook.md: every recipe in the cookbook
   appears here as a running test, in the cookbook's order and shape, so a
   recipe that rots breaks the build. The Eio adapter itself needs a
   dependency windtrap does not have; its guarantees are tested instead
   (recipe 1). A plain windtrap suite: this executable is a runtest test and
   must exit 0. *)

open Windtrap

(* Recipe 1: the Eio guarantees

   The [scoped Eio_main.run] adapter is a fragment (windtrap has no eio
   dependency; [scoped] itself is covered in test/unit);
   what the cookbook guarantees is that assertion failures are ordinary
   exceptions classified by identity, not catch site — storing one in a
   ref and re-raising it outside the assertion's dynamic extent keeps its
   structured payload. *)

let eio_tests =
  group "eio guarantees"
    [
      test "assertion failures survive store-and-reraise" (fun () ->
          let stored = ref None in
          (try equal int 1 2 with e -> stored := Some e);
          match !stored with
          | Some
              (Private.Failure.Check_failure
                 { Private.Failure.kind = Private.Failure.Equality _; _ }) ->
              ()
          | Some e -> raise e
          | None -> fail "the assertion did not raise");
    ]

(* Recipe 2: subprocess workers via a role env var

   The dispatch on COOKBOOK_ROLE happens at the bottom of this file,
   before [run] — the worker path never touches windtrap's CLI parsing or
   process exit. *)

let spawn_self ~role =
  (* [setenv] binds it for the rest of the test and the runner puts it
     back; the child inherits the process environment. *)
  setenv "COOKBOOK_ROLE" (Some role);
  let read_end, write_end = Unix.pipe ~cloexec:false () in
  let pid =
    Unix.create_process Sys.executable_name [| Sys.executable_name |] Unix.stdin
      write_end Unix.stderr
  in
  Unix.close write_end;
  let buffer = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec drain () =
    match Unix.read read_end bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buffer bytes 0 n;
        drain ()
  in
  drain ();
  Unix.close read_end;
  let _, status = Unix.waitpid [] pid in
  (match status with Unix.WEXITED 0 -> () | _ -> fail "worker did not exit 0");
  Buffer.contents buffer

let subprocess_tests =
  group "subprocess role pattern"
    [
      test "the worker role re-executes this binary" (fun () ->
          contains ~sub:"lock acquired" (spawn_self ~role:"worker"));
    ]

(* Recipe 3: two-phase keyed comparison *)

type tensor = { shape : int array; data : float array }

let equal_tensor ?pos expected actual =
  equal ?pos ~msg:"shape" (array int) expected.shape actual.shape;
  equal ?pos ~msg:"values" (array (float 1e-9)) expected.data actual.data

let keyed_tests =
  group "two-phase comparison"
    [
      test "equal tensors pass both phases" (fun () ->
          equal_tensor
            { shape = [| 2; 2 |]; data = [| 1.; 2.; 3.; 4. |] }
            { shape = [| 2; 2 |]; data = [| 1.; 2.; 3.; 4. |] });
      test "a shape mismatch fails in phase one" (fun () ->
          match
            equal_tensor
              { shape = [| 2 |]; data = [| 1.; 2. |] }
              { shape = [| 1; 2 |]; data = [| 1.; 2. |] }
          with
          | () -> fail "shape mismatch must fail"
          | exception Private.Failure.Check_failure failure ->
              equal (option string) (Some "shape") failure.Private.Failure.msg);
    ]

(* Recipe 4: complex tolerance testable *)

let complex ~rel ~abs : Complex.t testable =
  let close = Testable.equal (float_rel ~rel ~abs) in
  Testable.make
    ~pp:(fun ppf { Complex.re; im } ->
      Format.fprintf ppf "(%.17g %+.17gi)" re im)
    ~equal:(fun a b ->
      close a.Complex.re b.Complex.re && close a.Complex.im b.Complex.im)

let complex_tests =
  group "complex tolerance"
    [
      test "componentwise tolerance" (fun () ->
          equal
            (complex ~rel:1e-9 ~abs:1e-12)
            { Complex.re = 1.; im = 2. }
            { Complex.re = 1. +. 1e-13; im = 2. -. 1e-13 });
    ]

(* Recipe 5: scripted seams — the tape *)

type 'a tape = { name : string; mutable entries : 'a list; mutable dealt : int }

let next ?pos t =
  match t.entries with
  | [] -> failf ?pos "tape %s: exhausted after %d entries" t.name t.dealt
  | e :: rest ->
      t.entries <- rest;
      t.dealt <- t.dealt + 1;
      e

let next_opt t =
  match t.entries with
  | [] -> None
  | e :: rest ->
      t.entries <- rest;
      t.dealt <- t.dealt + 1;
      Some e

let remainder t =
  let rest = t.entries in
  t.entries <- [];
  t.dealt <- t.dealt + List.length rest;
  rest

let check_consumed t =
  if t.entries <> [] then
    failf "tape %s: %d of %d entries never consumed" t.name
      (List.length t.entries)
      (t.dealt + List.length t.entries)

let with_tape name entries =
  bracket
    ~setup:(fun () -> { name; entries; dealt = 0 })
    ~teardown:check_consumed

(* The recipe's seam consumer: retry once past a transient error. *)
let run_turn provider =
  match provider () with
  | Ok reply -> reply
  | Error _ -> (
      match provider () with
      | Ok reply -> reply
      | Error _ -> fail "gave up after one retry")

let message_of failure =
  match failure.Private.Failure.kind with
  | Private.Failure.Message m -> m
  | _ -> fail "failf raises a Message"

let tape_tests =
  group "scripted seams"
    [
      (* The constructor in real use: the whole script consumed, the
         teardown check passing silently on the way out. *)
      with_tape "provider"
        [ Error "timeout"; Ok "done" ]
        "a retry consumes the script exactly" (fun provider ->
          equal string "done" (run_turn (fun () -> next provider));
          is_none (next_opt provider));
      test "exhaustion names the tape and the position" (fun () ->
          let t = { name = "provider"; entries = []; dealt = 3 } in
          match next t with
          | _ -> fail "an exhausted tape must fail"
          | exception Private.Failure.Check_failure f ->
              contains ~sub:"tape provider: exhausted after 3 entries"
                (message_of f));
      test "unconsumed entries fail the teardown check" (fun () ->
          let t = { name = "provider"; entries = [ 1; 2 ]; dealt = 1 } in
          match check_consumed t with
          | () -> fail "a leftover script must fail"
          | exception Private.Failure.Check_failure f ->
              contains ~sub:"tape provider: 2 of 3 entries never consumed"
                (message_of f));
      test "remainder discharges the obligation" (fun () ->
          let t = { name = "events"; entries = [ "a"; "b" ]; dealt = 0 } in
          equal string "a" (next t);
          equal (list string) [ "b" ] (remainder t);
          check_consumed t);
    ]

(* Recipe 6: counting occurrences *)

let count ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if n = 0 || i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let count_tests =
  group "counting occurrences"
    [
      test "the local count is leftmost-first and non-overlapping" (fun () ->
          let log = "retry retry retry" in
          equal int 3 (count ~sub:"retry" log);
          equal int 1 (count ~sub:"aa" "aaa");
          equal int 0 (count ~sub:"absent" log);
          (* The two shapes the recipe hands the reader. *)
          equal int 3 (count ~sub:"retry" log);
          satisfies ~claim:"more than 2 retries" int
            (fun n -> n > 2)
            (count ~sub:"retry" log));
    ]

(* Recipe 7: convergence *)

let eventually ?(attempts = 100) ?diagnose ~step probe =
  let rec go n =
    match probe () with
    | Some v -> v
    | None when n >= attempts ->
        failf "no convergence in %d attempts%s" attempts
          (match diagnose with
          | None -> ""
          | Some d -> ": " ^ String.concat "; " (d ()))
    | None ->
        step ();
        go (n + 1)
  in
  go 1

let convergence_tests =
  group "convergence"
    [
      test "the loop probes first, then alternates" (fun () ->
          let pending = Queue.create () in
          List.iter (fun x -> Queue.add x pending) [ 1; 2; 3 ];
          let drained = ref [] in
          let reply =
            eventually
              ~step:(fun () -> drained := Queue.pop pending :: !drained)
              (fun () -> if Queue.is_empty pending then Some !drained else None)
          in
          (* Three steps drained the queue; the fourth probe converged, so
             a budget of n probes drove n-1 steps. *)
          equal (list int) [ 3; 2; 1 ] reply);
      test "an already-converged probe drives nothing" (fun () ->
          let steps = ref 0 in
          let () = eventually ~step:(fun () -> incr steps) (fun () -> Some ()) in
          equal int 0 !steps);
      test "a spent budget fails, and the diagnosis says what it saw"
        (fun () ->
          let pending = Queue.create () in
          Queue.add 1 pending;
          let failed =
            match
              eventually ~attempts:3
                ~diagnose:(fun () ->
                  [ Printf.sprintf "pending: %d" (Queue.length pending) ])
                ~step:(fun () -> ())
                (fun () -> if Queue.is_empty pending then Some () else None)
            with
            | () -> None
            | exception e -> Some (Printexc.to_string e)
          in
          let message = require_some failed in
          contains ~sub:"no convergence in 3 attempts" message;
          contains ~sub:"pending: 1" message);
    ]

(* The role dispatch and the suite *)

let () =
  match Sys.getenv_opt "COOKBOOK_ROLE" with
  | Some "worker" ->
      print_string "lock acquired\n";
      exit 0
  | Some _ | None ->
      run "cookbook"
        [
          eio_tests;
          subprocess_tests;
          keyed_tests;
          complex_tests;
          tape_tests;
          count_tests;
          convergence_tests;
        ]
