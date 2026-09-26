(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A capture and the [__POS__] it is compared with share a line, so the
   captured line is known. *)

open Windtrap
module Loc = Windtrap.Private.Loc

let strf = Printf.sprintf

let loc =
  Testable.make
    ~pp:(fun ppf (l : Loc.t) ->
      Format.fprintf ppf "%s:%d:%d" l.file l.line l.column)
    ~equal:(fun (a : Loc.t) (b : Loc.t) ->
      String.equal a.file b.file && a.line = b.line && a.column = b.column)

let here ((file, line, _, _) : Loc.pos) = strf "%s:%d" file line

let where = function
  | Some (l : Loc.t) -> strf "%s:%d" l.file l.line
  | None -> "no location"

(* Locations *)

let locations =
  group "Locations"
    [
      test "of_pos keeps the file, the line and the start column" (fun () ->
          equal loc
            { Loc.file = "test/foo.ml"; line = 12; column = 4 }
            (Loc.of_pos ("test/foo.ml", 12, 4, 9)));
      test "to_string spells a location file:line" (fun () ->
          equal string "test/foo.ml:12"
            (Loc.to_string { Loc.file = "test/foo.ml"; line = 12; column = 4 }));
    ]

(* Capturing *)

let direct () =
  let p = __POS__ and l = Loc.capture () in
  (p, l)

let in_a_handler () =
  try failwith "boom"
  with Stdlib.Failure _ ->
    let p = __POS__ and l = Loc.capture () in
    (p, l)

let through_the_stdlib () =
  List.hd (List.map (fun () -> (__POS__, Loc.capture ())) [ () ])

(* The lazy's closure is [Loc.capture] itself, so its caller is a frame of
   CamlinternalLazy. *)
let under_lazy () =
  let p = __POS__ and l = Lazy.force (Lazy.from_fun Loc.capture) in
  (p, l)

let inlined () =
  let[@inline always] here () = (__POS__, Loc.capture ()) in
  here ()

let below_a_delimiter () =
  Loc.delimit (fun () ->
      let p = __POS__ and l = Loc.capture () in
      (p, l))

let resolved () =
  let p = __POS__ and l = Loc.resolve () in
  (p, l)

let captures =
  [
    ("a direct call", direct);
    ("in the handler of an exception raised before", in_a_handler);
    ("through List.map", through_the_stdlib);
    ("called from CamlinternalLazy", under_lazy);
    ("in an inlined function", inlined);
    ("below a delimiter", below_a_delimiter);
  ]

(* A domain's stack holds no user frame. It runs in a forked child: once a
   process spawned a domain, OCaml refuses it every later [fork], the
   mutation loop's included. *)
let domain_capture () =
  if Sys.win32 then skip ~reason:"POSIX only: the domain runs in a fork" ();
  flush_all ();
  let pid =
    match Unix.fork () with
    | 0 ->
        Unix._exit
          (match Domain.join (Domain.spawn Loc.capture) with
          | None -> 0
          | Some _ -> 1)
    | pid -> pid
  in
  let ended =
    match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED 0 -> "no location"
    | WEXITED 1 -> "a location"
    | WEXITED n -> strf "exited %d" n
    | WSIGNALED n | WSTOPPED n -> strf "signal %d" n
  in
  equal string "no location" ended

(* [Seq.forever] calls [Loc.capture] from a frame of the standard library, and
   each [Seq.map] forces the node inside it from one of its own: [depth]
   entries stand between the capture and [beneath]'s frame. *)
let beneath depth =
  let rec nest n s = if n = 0 then s else nest (n - 1) (Seq.map Fun.id s) in
  match nest depth (Seq.forever Loc.capture) () with
  | Seq.Cons (l, _) -> l
  | Seq.Nil -> None

let tail_capture () = Loc.capture ()

let backtrace_lines () =
  let[@inline never] raiser () = raise (Stdlib.Failure "deep") in
  let raised_at = strf "%s:%d" __FILE__ (__LINE__ - 1) in
  match Loc.delimit (fun () -> ignore (raiser ())) with
  | () -> (raised_at, [])
  | exception Stdlib.Failure _ ->
      let slots =
        Option.value ~default:[||]
          (Printexc.backtrace_slots (Printexc.get_raw_backtrace ()))
      in
      let at slot =
        Option.map
          (fun (l : Printexc.location) -> strf "%s:%d" l.filename l.line_number)
          (Printexc.Slot.location slot)
      in
      (raised_at, List.filter_map at (Array.to_list slots))

let keeps_the_backtrace () =
  let recording = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace recording)
  @@ fun () ->
  let raised_at, lines = backtrace_lines () in
  satisfies
    ~claim:("a frame at " ^ raised_at)
    (list string) (List.mem raised_at) lines

let capturing =
  group "Capturing"
    [
      cases
        "capture locates the innermost frame of neither windtrap nor the \
         standard library"
        ~name:fst captures (fun (_, located) ->
          let p, l = located () in
          equal string (here p) (where l));
      test "capture is None when every frame is the standard library's"
        domain_capture;
      cases "capture reads the 24 innermost entries of the call stack" ~name:fst
        [
          ("a frame 4 entries in", (4, Some __FILE__));
          ("a frame 30 entries in", (30, None));
        ]
        (fun (_, (depth, file)) ->
          equal (option string) file
            (Option.map (fun (l : Loc.t) -> l.file) (beneath depth)));
      test "capture under delimit stops at its frame" (fun () ->
          equal string "no location" (where (Loc.delimit tail_capture)));
      test "delimit returns what its function returns" (fun () ->
          equal int 7 (Loc.delimit (fun () -> 7)));
      test "delimit raises again what its function raises" (fun () ->
          raises (Stdlib.Failure "boom") (fun () ->
              Loc.delimit (fun () -> failwith "boom")));
      test "delimit raises again with the exception's backtrace"
        keeps_the_backtrace;
      test "resolve with a __POS__ is of_pos of it" (fun () ->
          equal (option loc)
            (Some { Loc.file = "other.ml"; line = 42; column = 7 })
            (Loc.resolve ~__POS__:("other.ml", 42, 7, 20) ()));
      test "resolve without a __POS__ is capture ()" (fun () ->
          let p, l = resolved () in
          equal string (here p) (where l));
      cases "own_unit is true for windtrap's units, by whole unit name"
        ~name:fst
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
        ]
        (fun (name, own) -> equal bool own (Loc.own_unit name));
    ]

let () = exit (run "loc" [ locations; capturing ])
