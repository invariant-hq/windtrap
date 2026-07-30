(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The compile matrix: every replacement spelling in the migration
   reference — CHANGES.md's 0.2.0 entry, whose breaking-changes section
   doubles as the migration guide from 0.1 — and every OCaml call site in
   the RFC guide, compiled against the facade as written (modulo the
   local stubs standing in for the fictional libraries). Construction
   alone is a claim — a test list that constructs is a test list that
   runs (RFC Law 6) — and the deterministic subset also executes through
   Runner.execute. If an edit to the facade breaks a documented spelling,
   this file stops compiling. *)

(* The RFC guide's call site: user modules named [Cli] and [Text], declared
   before [open Windtrap]. The facade must not shadow them (G5 review) — its
   only module exports are [Testable], [Gen], [Exn], and [Private]. This
   file therefore never opens [Windtrap.Private]; internals are addressed
   qualified. [Runner] probes that internals stay out of scope: it must
   still name this module after [open Windtrap]. *)
module Cli = struct
  let help () = "usage"
end

module Text = struct
  let x = 1
end

module Runner = struct
  let tag = "user-runner"
end

open Windtrap

(* The shared meta harness carries the checks, the environment hygiene,
   the temp roots, and the tree-wide summary dialect. Addressed
   qualified, never opened: an [open Harness] would shadow facade
   spellings this file exists to compile (e.g. [contains]). *)
let () = Harness.init "migrating"
let check = Harness.check

(* User modules survive [open Windtrap]. *)
let () =
  check "user Cli is not shadowed" (Cli.help () = "usage");
  check "user Text is not shadowed" (Text.x = 1);
  check "user Runner is not shadowed" (Runner.tag = "user-runner")

let with_temp_root f = Harness.with_temp_root ~prefix:"windtrap-migrating-" f

(* ───── Stubs for the documents' fictional libraries ───── *)

type shape = Circle of float | Rect of float * float

let area = function Circle r -> Float.pi *. r *. r | Rect (w, h) -> w *. h

let pp_shape ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let add = ( + )
let f n = abs n + 1

let parse port =
  if port > 0 && port < 65_536 then Ok port
  else Error ("invalid port: " ^ string_of_int port)

let greet () = print_string "hello"

module Db = struct
  type t = unit

  let connect () = ()
  let close () = ()
end

module Server = struct
  type t = unit

  let start () = ()
  let stop (_ : t) = ()
  let ping (_ : t) = true
end

module Mytool = struct
  let help () = "Usage: mytool [OPTIONS] COMMAND\n"
end

module Point = struct
  type t = { x : int; y : int }

  let pp ppf { x; y } = Format.fprintf ppf "(%d, %d)" x y
  let equal = ( = )
end

module Sessions = struct
  let all () = [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
end

let store = ()

(* ───── Migration reference: properties ───── *)

(* Boolean laws (prop) *)
let prop_rev =
  prop "rev involutive"
    Gen.(list int)
    (fun l -> equal (list int) l (List.rev (List.rev l)))

(* Assertion-style bodies (prop') *)
let prop_positive = prop "positive" Gen.nat (fun n -> is_true (f n > 0))

(* Multi-argument laws (prop2..4) *)
let prop_add =
  prop "add commutes"
    Gen.(pair int int)
    (fun (a, b) -> equal int (add a b) (add b a))

(* Custom generators: one pp feeds both worlds. *)
let shape = Testable.make ~pp:pp_shape ~equal:( = )

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0. 100.);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0. 100.) (float_range 0. 100.));
      ])
  |> Gen.with_pp pp_shape

(* Generator configuration: ~count and ~examples. *)
let prop_area =
  prop "area non-negative" ~count:500
    ~examples:[ Rect (2., 0.) ]
    gen_shape
    (fun s -> is_true (Float.compare (area s) 0. >= 0))

(* ───── Migration reference: cases ───── *)

let cases_indexed =
  cases "ports parse" [ 1; 80; 8080 ] (fun p -> ignore (require_ok (parse p)))

let cases_named =
  cases "ports parse" ~name:string_of_int [ 1; 80; 8080 ] (fun p ->
      ignore (require_ok (parse p)))

(* ───── Migration reference: snapshots — and the guide's snapshot call
   site.

   The guide spells the call [snapshot "help" (Cli.help ())] against a
   user module named [Cli]; examples/04 spells the user module [Mytool].
   Both spellings compile under [open Windtrap]. *)

let snap_help = test "help" (fun () -> snapshot "help" (Mytool.help ()))

let guide_help =
  test "guide help" (fun () -> snapshot "guide-help" (Cli.help ()))

let snap_report =
  let n = 42 in
  test "report" (fun () -> snapshot "report" (Printf.sprintf "%d rows" n))

(* ───── Migration reference: the expect-string family ───── *)

let greets_output =
  test "greets" (fun () ->
      print_string "hello";
      equal string "hello" (output ()))

let greets_snapshot =
  test "greets snapshot" (fun () ->
      greet ();
      snapshot "greeting" (output ()))

(* ───── Migration reference: group hooks — and the guide's resources
   example ───── *)

let with_db = bracket ~setup:Db.connect ~teardown:Db.close

let storage =
  group "storage"
    [
      with_db "insert" (fun db -> ignore (db : Db.t));
      with_db "delete" (fun db -> ignore (db : Db.t));
    ]

let server = fixture ~teardown:Server.stop Server.start

let api =
  group "api" [ test "responds" (fun () -> is_true (Server.ping (server ()))) ]

(* ───── Migration reference: testables ───── *)

let point_structural = Testable.structural ~pp:Point.pp
let point = Testable.make ~pp:Point.pp ~equal:Point.equal

let testable_tests =
  group "testables"
    [
      test "structural" (fun () ->
          equal point_structural { Point.x = 1; y = 2 } { Point.x = 1; y = 2 });
      test "made" (fun () ->
          equal point { Point.x = 1; y = 2 } { Point.x = 1; y = 2 });
      test "shape" (fun () -> equal shape (Circle 1.) (Circle 1.));
    ]

(* ───── Migration reference: odds and ends ───── *)

let odds =
  group "odds"
    [
      test "require_ok unwraps in flow" (fun () ->
          let v = require_ok (parse 80) in
          equal int 80 v);
      test "raises Invalid_argument spelling" (fun () ->
          raises (Invalid_argument "boom") (fun () -> invalid_arg "boom"));
    ]

(* ───── Migration reference: exceptions, is_true residue, temp
   lifecycles (the B-package rows) ───── *)

type addr = Tcp of int | Unix_socket of string

let resolve name = if name = "db" then Tcp 8080 else Unix_socket name

module Uop = struct
  let pp = Format.pp_print_int
  let compare = Int.compare
  let zero = 0
end

let uop = Testable.make ~pp:Uop.pp ~equal:Int.equal

let b_package =
  group "b-package rows"
    [
      test "raises with an exact message" (fun () ->
          raises (Invalid_argument "index out of bounds") (fun () ->
              invalid_arg "index out of bounds"));
      test "raises_match with Exn.invalid_arg" (fun () ->
          raises_match Exn.invalid_arg (fun () -> invalid_arg "any"));
      test "raises_match with Exn.failure substring" (fun () ->
          raises_match (Exn.failure ~substring:"unhandled op") (fun () ->
              failwith "step: unhandled op 3"));
      test "satisfies replaces the is_true wrap" (fun () ->
          satisfies ~msg:"positive" uop (fun r -> Uop.compare r Uop.zero > 0) 4);
      test "contains replaces the hand helper" (fun () ->
          contains ~sub:"REDACTED" "token=REDACTED";
          not_contains ~sub:"secret_key" "token=REDACTED");
      test "require_match asserts and unwraps" (fun () ->
          equal int 8080
            (require_match
               (function Tcp p -> Some p | Unix_socket _ -> None)
               (resolve "db")));
      test "temp_dir replaces with_temp_dir" (fun () ->
          let dir = temp_dir () in
          is_true (Sys.is_directory dir));
      test "srandom replaces the WINDTRAP_SEED parser" (fun () ->
          let state = srandom () in
          ignore (Random.State.bits state));
      test "of_module witnesses a conventional module" (fun () ->
          equal
            (Testable.of_module (module Point))
            { Point.x = 1; y = 2 } { Point.x = 1; y = 2 });
    ]

(* ───── Migration reference: Gen renames ───── *)

let gen_renames =
  group "gen renames"
    [
      prop "of_list"
        (Gen.of_list [ 1; 2; 3 ])
        (fun n -> satisfies int (fun n -> n >= 1 && n <= 3) n);
      prop "one_of" Gen.(one_of [ nat; small_int ]) (fun _ -> ());
      prop "list ~size"
        Gen.(list ~size:(int_range 2 5) bool)
        (fun l -> satisfies int (fun n -> n >= 2 && n <= 5) (List.length l));
      prop "string_of ~size"
        Gen.(string_of ~size:(int_range 1 8) (char_range 'a' 'z'))
        (fun s -> is_true (String.length s >= 1));
      prop "bytes_of ~size"
        Gen.(bytes_of ~size:(int_range 1 8) (char_range 'a' 'z'))
        (fun b -> is_true (Bytes.length b >= 1));
      prop "pure is kept" (Gen.pure 42) (fun n -> equal int 42 n);
    ]

(* ───── RFC guide, "A failing assertion" (here with equal sides) ───── *)

let sessions =
  test "sessions after login" (fun () ->
      equal
        (list (pair string (list int)))
        [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
        (Sessions.all store))

(* ───── Construction and execution ───── *)

(* Snapshot-bearing and name-colliding trees only construct: baselines do
   not exist in this synthetic suite, and [cases_indexed]/[cases_named]
   share a path. *)
let () =
  let constructed =
    Windtrap.Private.Test_tree.flatten
      [ cases_indexed; snap_help; guide_help; snap_report; greets_snapshot ]
  in
  check "documented spellings construct" (List.length constructed = 7)

let () =
  with_temp_root @@ fun root ->
  let module Run = Windtrap.Private.Run in
  let module Runner = Windtrap.Private.Runner in
  let module Test_tree = Windtrap.Private.Test_tree in
  let config =
    { (Run.default_config ()) with Run.seed = 0x5eedL; log_dir = root }
  in
  let suite =
    [
      prop_rev;
      prop_positive;
      prop_add;
      prop_area;
      cases_named;
      greets_output;
      storage;
      api;
      testable_tests;
      odds;
      b_package;
      gen_renames;
      sessions;
    ]
  in
  match Runner.execute ~config ~suite:"migrating" suite with
  | Error error ->
      check "documented snippets execute" false;
      Printf.printf "  startup error: %s\n%!" (Runner.startup_message error)
  | Ok outcome ->
      check "documented snippets all pass" (outcome.Runner.exit_code = 0);
      check "cases are individually addressable sub-tests"
        (List.exists
           (fun case -> case.Test_tree.path = [ "ports parse"; "8080" ])
           outcome.Runner.selected)

(* ───── Summary ───── *)

let () = Harness.finish ()
