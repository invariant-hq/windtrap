(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

open Windtrap
module Choice = Windtrap.Private.Choice
module Seed = Windtrap.Private.Seed

let choices = Testable.(array int64)

let read reads src =
  List.iter (fun bound -> ignore (Choice.choice src ~bound None)) reads;
  Choice.choices src

let any_choices =
  Gen.(
    with_pp (Testable.pp choices)
      (map Array.of_list (list (one_of [ int64; map Int64.of_int nat ]))))

let tokens () =
  equal string "r1:AH-AAf___________wE:d1"
    (Choice.token [| 0L; 127L; 128L; -1L |] ~digest:"d1")

let sources =
  group "Sources"
    [
      test "a replayed choice above its bound reads the bound" (fun () ->
          equal choices [| 3L; 0L; 9L |]
            (read [ 3L; 0L; 100L ] (Choice.replaying [| 7L; 5L; 9L |])));
      test "a choice past the end of a replay reads 0, bound 0 included"
        (fun () ->
          let src = Choice.replaying [| 1L |] in
          let read = read [ 5L; 0L; 5L ] src in
          equal (pair choices choices)
            ([| 1L; 0L; 0L |], [| 5L; 0L; 5L |])
            (read, Choice.bounds src));
      test "a generating source records what it is given, from its stream"
        (fun () ->
          let src = Choice.generating (Seed.make 7L) in
          let drawn = Choice.sample src (Seed.below ~bound:10L) in
          let v = Choice.choice src ~bound:9L drawn in
          equal (pair int64 choices)
            (fst (Seed.below ~bound:10L (Seed.make 7L)), [| v |])
            (v, Choice.choices src);
          equal (option int64) None
            (Choice.sample (Choice.replaying [||]) (Seed.below ~bound:10L)));
      test "a span covers the choices its function read, if any" (fun () ->
          let src = Choice.replaying [||] in
          ignore (Choice.choice src ~bound:1L None);
          Choice.span src (fun () ->
              ignore (Choice.choice src ~bound:1L None);
              ignore (Choice.choice src ~bound:1L None));
          Choice.span src ignore;
          equal
            (list (pair int int))
            [ (1, 3) ]
            (List.map
               (fun (s : Choice.span) -> (s.start, s.stop))
               (Choice.spans src)));
    ]

let token_group =
  group "Tokens"
    [
      test "a token spells its choices as LEB128 in base64url" tokens;
      prop "a token reads back as its choices and digest" any_choices (fun c ->
          equal
            (result (pair choices string) string)
            (Ok (c, "0a1b2c3d"))
            (Choice.of_token (Choice.token c ~digest:"0a1b2c3d")));
      test "of_token refuses any other text" (fun () ->
          List.iter
            (fun text -> is_error (Choice.of_token text))
            [
              "";
              "r1:";
              "r1:AA:";
              "r2:AA:d";
              "r1:A*:d";
              "r1:AA:d:e";
              "r1:gICAgICAgICAgAE:d";
            ]);
    ]

let () = exit (run "choice" [ sources; token_group ])
