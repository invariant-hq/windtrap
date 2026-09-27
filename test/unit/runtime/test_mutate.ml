(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The registry is a global of the process, and this executable's own
   instrumented libraries register in it too. Each test registers files of its
   own under [t/], a directory no instrumented source is in, and every test
   that arms disarms on its way out, so a test passes alone as in the suite. *)

open Windtrap
module Mutate = Windtrap_runtime.Mutate
module Child = Windtrap_test_support.Child

let strf = Printf.sprintf
let id file line col rewrite = { Mutate.file; line; col; rewrite }

let site ?dismissed ?(before = "b") ?(after = "a") line col rewrite =
  { Mutate.line; col; rewrite; before; after; dismissed }

let register_only ~file ~sites =
  let _guard : int -> bool = Mutate.register ~file ~sites in
  ()

(* Rows *)

let mutant_row (m : Mutate.mutant) =
  let off =
    match m.dismissed with
    | None -> ""
    | Some reason -> " (off: " ^ reason ^ ")"
  in
  strf "%s %s -> %s%s" (Mutate.id_to_string m.id) m.before m.after off

let ids mutants =
  String.concat ", "
    (List.map (fun (m : Mutate.mutant) -> Mutate.id_to_string m.id) mutants)

let error_row = function
  | Mutate.Malformed { spec; reason } -> strf "malformed %S: %s" spec reason
  | Uncatalogued { id } -> "uncatalogued " ^ Mutate.id_to_string id
  | Unmatched { id; candidates } ->
      strf "unmatched %s: %s" (Mutate.id_to_string id) (ids candidates)
  | Ambiguous { id; candidates } ->
      strf "ambiguous %s: %s" (Mutate.id_to_string id)
        (String.concat ", " (List.map mutant_row candidates))

let armed = function Ok m -> "armed " ^ mutant_row m | Error e -> error_row e

let catalogued file =
  List.filter
    (fun (m : Mutate.mutant) -> String.equal m.id.file file)
    (Mutate.catalogue ())

(* The drain closes the window whatever the rows keep, so it is always called
   whole. *)
let drained file =
  let row (r : Mutate.reached) =
    if String.equal r.mutant.id.file file then
      Some
        (strf "%d:%d:%s x%d" r.mutant.id.line r.mutant.id.col
           r.mutant.id.rewrite r.hits)
    else None
  in
  String.concat ", " (List.filter_map row (Mutate.drain ()))

let fresh () =
  ignore (Mutate.drain ());
  Mutate.next_epoch ()

(* [arm] disarms before it resolves, so an identifier of no catalogued file
   leaves nothing armed. *)
let disarm () = ignore (Mutate.arm (id "t/nowhere.ml" 1 0 "not"))
let disarming f () = Fun.protect ~finally:disarm f

let arm_ok ?budget id =
  require_ok ~pp:Mutate.pp_arm_error (Mutate.arm ?budget id)

(* What each of [n] evaluations of [guard 0] answers, or the runaway it
   raised. *)
let evaluations guard n =
  let rec loop k =
    if k = 0 then []
    else
      let answer =
        match guard 0 with
        | b -> string_of_bool b
        | exception Mutate.Runaway { id; hits; budget } ->
            strf "runaway %s %d/%d" (Mutate.id_to_string id) hits budget
      in
      answer :: loop (k - 1)
  in
  loop n

let malformed_spec = function
  | Error (Mutate.Malformed { spec; _ }) -> Some spec
  | Ok _ | Error (Uncatalogued _ | Unmatched _ | Ambiguous _) -> None

let malformed_reason = function
  | Error (Mutate.Malformed { reason; _ }) -> Some reason
  | Ok _ | Error (Uncatalogued _ | Unmatched _ | Ambiguous _) -> None

(* Identifiers *)

let pp_id ppf id = Format.pp_print_string ppf (Mutate.id_to_string id)

let small_id =
  Gen.with_pp pp_id
    (Gen.map
       (fun (file, line, col, rewrite) -> id file line col rewrite)
       (Gen.quad
          (Gen.of_list [ "a"; "b" ])
          (Gen.int_range 1 2) (Gen.int_range 0 1)
          (Gen.of_list [ "and"; "or" ])))

let any_id =
  let file =
    Gen.map
      (fun s -> "f" ^ s)
      (Gen.string_of (Gen.of_list [ 'a'; ':'; '/'; '.'; ' '; '\\' ]))
  in
  Gen.with_pp pp_id
    (Gen.map
       (fun (file, line, col, rewrite) -> id file line col rewrite)
       (Gen.quad file (Gen.map succ Gen.nat) Gen.nat
          (Gen.of_list Mutate.rewrites)))

let sign n = Int.compare n 0

let orders_as_tuples ((a : Mutate.id), (b : Mutate.id)) =
  equal int
    (sign
       (compare
          (a.file, a.line, a.col, a.rewrite)
          (b.file, b.line, b.col, b.rewrite)))
    (sign (Mutate.compare_id a b))

let reads_back i =
  equal (result string string)
    (Ok (Mutate.id_to_string i))
    (Result.map_error
       (Format.asprintf "%a" Mutate.pp_arm_error)
       (Result.map Mutate.id_to_string
          (Mutate.id_of_string (Mutate.id_to_string i))))

let spellings =
  [
    (id "lib/calc.ml" 9 12 "add", "lib/calc.ml:9:12:add");
    (id "a.ml" 1 0 "not", "a.ml:1:0:not");
    (id "a:b.ml" 44 3 "fsub", "a:b.ml:44:3:fsub");
    (id "C:/x/calc.ml" 9 12 "add", "C:/x/calc.ml:9:12:add");
  ]

let refused =
  [
    "";
    "add";
    "lib/calc.ml";
    "lib/calc.ml:9";
    "a.ml:1:";
    "lib/calc.ml:9:12:frobnicate";
    "lib/calc.ml:9:12:plus";
    "lib/calc.ml:9:12:LT";
    "lib/calc.ml:9:12:drop";
    ":add";
    "x:add";
    "lib/calc.ml:9:add";
    "lib/calc.ml:9:x:add";
    "lib/calc.ml:9:-1:add";
    "lib/calc.ml:9:+1:add";
    "lib/calc.ml:312-317:add";
    "lib/calc.ml:x:12:add";
    ":x:1:add";
    "lib/calc.ml:0:12:add";
    "lib/calc.ml:0x10:2:add";
    "lib/calc.ml:1_0:2:add";
    "lib/calc.ml:+9:2:add";
    "a.ml:99999999999999999999:1:add";
    ":9:12:add";
    ":0:1:add";
  ]

let reasons () =
  let row spec =
    strf "%S: %s" spec
      (require_match malformed_reason (Mutate.id_of_string spec))
  in
  expect (String.concat "\n" (List.map row refused))
  @@ __POS_OF__
       {|
    "": no ':' separator
    "add": no ':' separator
    "lib/calc.ml": no ':' separator
    "lib/calc.ml:9": unknown rewrite "9" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    "a.ml:1:": unknown rewrite "" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    "lib/calc.ml:9:12:frobnicate": unknown rewrite "frobnicate" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    "lib/calc.ml:9:12:plus": unknown rewrite "plus" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    "lib/calc.ml:9:12:LT": unknown rewrite "LT" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    "lib/calc.ml:9:12:drop": unknown rewrite "drop" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or)
    ":add": no position before the rewrite
    "x:add": no position before the rewrite
    "lib/calc.ml:9:add": no line number
    "lib/calc.ml:9:x:add": invalid column "x"
    "lib/calc.ml:9:-1:add": invalid column "-1"
    "lib/calc.ml:9:+1:add": invalid column "+1"
    "lib/calc.ml:312-317:add": invalid column "312-317"
    "lib/calc.ml:x:12:add": invalid line "x"
    ":x:1:add": invalid line "x"
    "lib/calc.ml:0:12:add": line numbers are 1-based
    "lib/calc.ml:0x10:2:add": invalid line "0x10"
    "lib/calc.ml:1_0:2:add": invalid line "1_0"
    "lib/calc.ml:+9:2:add": invalid line "+9"
    "a.ml:99999999999999999999:1:add": invalid line "99999999999999999999"
    ":9:12:add": empty file name
    ":0:1:add": empty file name
    |}

let identifiers =
  group "Identifiers"
    [
      test "rewrites is the closed vocabulary, in its order" (fun () ->
          equal (list string)
            [
              "not";
              "lt";
              "le";
              "gt";
              "ge";
              "eq";
              "neq";
              "add";
              "sub";
              "fadd";
              "fsub";
              "and";
              "or";
            ]
            Mutate.rewrites);
      cases "id_to_string is <file>:<line>:<col>:<rewrite>" ~name:snd spellings
        (fun (i, spelling) -> equal string spelling (Mutate.id_to_string i));
      prop "compare_id orders by file, then line, column and rewrite"
        (Gen.pair small_id small_id)
        orders_as_tuples;
      prop "id_of_string reads back every spelling of id_to_string"
        ~examples:(List.map fst spellings @ [ id "f" max_int max_int "or" ])
        any_id reads_back;
      cases "id_of_string refuses a spelling as Malformed, naming it"
        ~name:(fun spec -> if spec = "" then "the empty string" else spec)
        refused
        (fun spec ->
          equal string spec
            (require_match malformed_spec (Mutate.id_of_string spec)));
      test "a refusal names the first field read from the right that is wrong"
        reasons;
    ]

(* Sites and the catalogue *)

let bad_tables =
  [
    ("line 0", site 0 0 "lt");
    ("a negative column", site 1 (-2) "lt");
    ("an unknown rewrite", site 1 0 "plus");
    ("drop, which no instrumenter emits", site 1 0 "drop");
    ("an operator's name", site 1 0 "LT");
  ]

let refused_table (_, bad) =
  raises_match
    (Exn.invalid_arg ~substring:"Windtrap_runtime.Mutate: t/bad.ml: site 1:")
    (fun () ->
      register_only ~file:"t/bad.ml" ~sites:[| site 1 0 "lt"; bad; bad |])

let refusal_messages () =
  let message (_, bad) =
    match register_only ~file:"t/bad.ml" ~sites:[| site 1 0 "lt"; bad |] with
    | () -> "registered"
    | exception Invalid_argument m -> m
  in
  expect (String.concat "\n" (List.map message bad_tables))
  @@ __POS_OF__
       {|
    Windtrap_runtime.Mutate: t/bad.ml: site 1: line 0 is not 1-based
    Windtrap_runtime.Mutate: t/bad.ml: site 1: negative column -2
    Windtrap_runtime.Mutate: t/bad.ml: site 1: unknown rewrite "plus"
    Windtrap_runtime.Mutate: t/bad.ml: site 1: unknown rewrite "drop"
    Windtrap_runtime.Mutate: t/bad.ml: site 1: unknown rewrite "LT"
    |}

let empty_file_name () =
  register_only ~file:"" ~sites:[| site 1 0 "or" |];
  let m =
    require_match (function [ m ] -> Some m | _ -> None) (catalogued "")
  in
  equal string "\":1:0:or\": empty file name"
    (strf "%S: %s" (Mutate.id_to_string m.id)
       (require_match malformed_reason
          (Mutate.id_of_string (Mutate.id_to_string m.id))))

let catalogue_order () =
  register_only ~file:"t/cat_b.ml"
    ~sites:[| site 9 2 "or"; site 1 2 "and" ~dismissed:"equivalent" |];
  register_only ~file:"t/cat_a.ml"
    ~sites:[| site 4 0 "sub" ~before:"a - b" ~after:"a + b" |];
  equal (list string)
    [
      "t/cat_a.ml:4:0:sub a - b -> a + b";
      "t/cat_b.ml:1:2:and b -> a (off: equivalent)";
      "t/cat_b.ml:9:2:or b -> a";
    ]
    (List.map mutant_row (catalogued "t/cat_a.ml" @ catalogued "t/cat_b.ml"))

let registered_twice () =
  let sites = [| site 2 4 "gt" |] in
  let g1 = Mutate.register ~file:"t/twice.ml" ~sites in
  let g2 = Mutate.register ~file:"t/twice.ml" ~sites:(Array.copy sites) in
  fresh ();
  List.iter (fun g -> ignore (g 0 : bool)) [ g1; g2; g2 ];
  equal string "2:4:gt x3" (drained "t/twice.ml");
  equal string "t/twice.ml:2:4:gt" (ids (catalogued "t/twice.ml"))

let conflict_warning () =
  register_only ~file:"t/warned.ml" ~sites:[| site 1 0 "lt" |];
  ignore (output ());
  register_only ~file:"t/warned.ml" ~sites:[| site 1 0 "lt" ~before:"x < y" |];
  expect (output ())
  @@ __POS_OF__
       {|
    windtrap: warning: t/warned.ml: conflicting instrumentation tables in one executable (stale build artifacts? rebuild from clean); ignoring one module's sites
    |}

let conflict_dropped () =
  register_only ~file:"t/conflict.ml" ~sites:[| site 1 0 "lt" |];
  let inert =
    Mutate.register ~file:"t/conflict.ml"
      ~sites:[| site 1 0 "lt" ~before:"x < y" |]
  in
  fresh ();
  equal (list bool) [ false; false; false ] (List.map inert [ 0; 99; -1 ]);
  equal string "" (drained "t/conflict.ml");
  equal (list string)
    [ "t/conflict.ml:1:0:lt b -> a" ]
    (List.map mutant_row (catalogued "t/conflict.ml"))

let sites_and_catalogue =
  group "Sites and the catalogue"
    [
      cases "register raises Invalid_argument on a malformed site" ~name:fst
        bad_tables refused_table;
      test "the message names the file and the first malformed site"
        refusal_messages;
      test "the empty file name is registered, and its identifier is refused"
        empty_file_name;
      test
        "the catalogue holds every registered mutant, with its renderings and \
         dismissal, in identifier order"
        catalogue_order;
      test "duplicate sites of one table are catalogued once" (fun () ->
          register_only ~file:"t/dup_cat.ml"
            ~sites:[| site 1 0 "or" ~before:"x"; site 1 0 "or" ~before:"y" |];
          equal string "t/dup_cat.ml:1:0:or" (ids (catalogued "t/dup_cat.ml")));
      test
        "a file registered twice with equal tables is catalogued once and \
         drains once, with the evaluations of both"
        registered_twice;
      test "a table that differs from the file's first warns on standard error"
        conflict_warning;
      test
        "the guard of a differing table answers false and counts nothing, and \
         the first table stays catalogued"
        conflict_dropped;
      cases "a guard raises Invalid_argument for an index outside its table"
        ~name:string_of_int [ 1; -1 ] (fun i ->
          let g =
            Mutate.register ~file:"t/bounds.ml" ~sites:[| site 1 0 "lt" |]
          in
          raises_match Exn.invalid_arg (fun () -> g i));
    ]

(* Arming *)

let near_sites = [| site 8 1 "add"; site 5 3 "lt" |]

let unmatched i =
  register_only ~file:"t/near.ml" ~sites:near_sites;
  equal string
    (strf "unmatched %s: t/near.ml:5:3:lt, t/near.ml:8:1:add"
       (Mutate.id_to_string i))
    (armed (Mutate.arm i))

let ambiguous () =
  let sites =
    [| site 1 0 "lt"; site 4 2 "eq" ~before:"x"; site 4 2 "eq" ~before:"y" |]
  in
  let g = Mutate.register ~file:"t/dup.ml" ~sites in
  register_only ~file:"t/dup.ml" ~sites:(Array.copy sites);
  equal string
    "ambiguous t/dup.ml:4:2:eq: t/dup.ml:4:2:eq x -> a, t/dup.ml:4:2:eq y -> a"
    (armed (Mutate.arm (id "t/dup.ml" 4 2 "eq")));
  equal (list bool) [ false; false ] [ g 1; g 2 ]

let one_site =
  [|
    site 3 10 "lt" ~before:"a < b" ~after:"not (b < a)";
    site 7 4 "add" ~before:"a + b" ~after:"a - b";
    site 11 6 "not" ~before:"n > 0" ~after:"not (n > 0)";
  |]

let arms_one_site () =
  let g = Mutate.register ~file:"t/one.ml" ~sites:one_site in
  equal string "armed t/one.ml:7:4:add a + b -> a - b"
    (armed (Mutate.arm (id "t/one.ml" 7 4 "add")));
  equal (list bool) [ false; true; false ] (List.map g [ 0; 1; 2 ])

let arms_every_registration () =
  let sites = [| site 2 4 "gt" |] in
  let g1 = Mutate.register ~file:"t/copies.ml" ~sites in
  let g2 = Mutate.register ~file:"t/copies.ml" ~sites:(Array.copy sites) in
  ignore (arm_ok (id "t/copies.ml" 2 4 "gt"));
  let armed_answers = [ g1 0; g2 0 ] in
  disarm ();
  equal (list bool) [ true; true; false; false ] (armed_answers @ [ g1 0; g2 0 ])

let refusal_disarms () =
  let g = Mutate.register ~file:"t/refuse.ml" ~sites:[| site 1 0 "or" |] in
  ignore (arm_ok (id "t/refuse.ml" 1 0 "or"));
  let before = g 0 in
  is_error (Mutate.arm (id "t/nothing.ml" 1 0 "or"));
  equal (list bool) [ true; false ] [ before; g 0 ]

let rearm () =
  let ga = Mutate.register ~file:"t/rearm_a.ml" ~sites:[| site 1 0 "lt" |] in
  let gb = Mutate.register ~file:"t/rearm_b.ml" ~sites:[| site 1 0 "gt" |] in
  ignore (arm_ok ~budget:2 (id "t/rearm_a.ml" 1 0 "lt"));
  Mutate.reset_reach ();
  let first = ga 0 in
  ignore (arm_ok (id "t/rearm_b.ml" 1 0 "gt"));
  equal (list string)
    [ "true"; "false"; "true"; "true"; "true"; "true" ]
    (List.map string_of_bool [ first; ga 0 ] @ evaluations gb 4)

let runaway () =
  let g = Mutate.register ~file:"t/runaway.ml" ~sites:[| site 2 0 "not" |] in
  ignore (arm_ok ~budget:3 (id "t/runaway.ml" 2 0 "not"));
  Mutate.reset_reach ();
  equal (list string)
    [
      "true";
      "true";
      "true";
      "runaway t/runaway.ml:2:0:not 4/3";
      "runaway t/runaway.ml:2:0:not 5/3";
    ]
    (evaluations g 5);
  disarm ();
  equal (list string) [ "false" ] (evaluations g 1)

let headroom () =
  let g = Mutate.register ~file:"t/forked.ml" ~sites:[| site 1 0 "and" |] in
  ignore (evaluations g 10);
  ignore (arm_ok ~budget:2 (id "t/forked.ml" 1 0 "and"));
  Mutate.reset_reach ();
  equal (list string)
    [ "true"; "true"; "runaway t/forked.ml:1:0:and 3/2" ]
    (evaluations g 3)

let per_registration () =
  let sites = [| site 1 0 "le" |] in
  let g1 = Mutate.register ~file:"t/per_copy.ml" ~sites in
  let g2 = Mutate.register ~file:"t/per_copy.ml" ~sites:(Array.copy sites) in
  ignore (arm_ok ~budget:2 (id "t/per_copy.ml" 1 0 "le"));
  Mutate.reset_reach ();
  let answers = evaluations g1 2 @ evaluations g2 2 in
  let hits = Mutate.armed_hits () in
  equal (list string)
    [ "true"; "true"; "true"; "true"; "4"; "runaway t/per_copy.ml:1:0:le 3/2" ]
    (answers @ [ string_of_int hits ] @ evaluations g1 1)

let counted_since_reset () =
  let g = Mutate.register ~file:"t/kept.ml" ~sites:[| site 1 0 "ge" |] in
  let the_id = id "t/kept.ml" 1 0 "ge" in
  ignore (arm_ok the_id);
  Mutate.reset_reach ();
  ignore (evaluations g 2);
  ignore (arm_ok the_id);
  let after_arm = Mutate.armed_hits () in
  Mutate.reset_reach ();
  equal (list int) [ 2; 0 ] [ after_arm; Mutate.armed_hits () ]

let refused_budget budget =
  let g = Mutate.register ~file:"t/kept_armed.ml" ~sites:[| site 1 0 "sub" |] in
  ignore (arm_ok (id "t/kept_armed.ml" 1 0 "sub"));
  raises
    (Invalid_argument "Windtrap_runtime.Mutate.arm: budget must be positive")
    (fun () -> Mutate.arm ~budget (id "t/kept_armed.ml" 1 0 "sub"));
  equal (list string) [ "true" ] (evaluations g 1)

let inert_armed () =
  register_only ~file:"t/inert.ml" ~sites:[| site 1 0 "eq" |];
  let inert =
    Mutate.register ~file:"t/inert.ml" ~sites:[| site 1 0 "eq" ~before:"x" |]
  in
  ignore (arm_ok ~budget:1 (id "t/inert.ml" 1 0 "eq"));
  equal (list bool)
    [ false; false; false; false; false ]
    (List.map inert [ 0; 0; 0; 99; -1 ])

let arm_error_messages () =
  register_only ~file:"t/seven.ml"
    ~sites:(Array.init 7 (fun i -> site (i + 1) 0 "or"));
  register_only ~file:"t/twins.ml"
    ~sites:[| site 4 2 "eq" ~before:"x"; site 4 2 "eq" ~before:"y" |];
  let message e = Format.asprintf "%a" Mutate.pp_arm_error e in
  let refused r =
    message
      (require_error
         ~pp:(fun ppf m -> Format.pp_print_string ppf (mutant_row m))
         r)
  in
  expect
    (String.concat "\n"
       [
         message (require_error ~pp:pp_id (Mutate.id_of_string "lib/calc.ml:9"));
         refused (Mutate.arm (id "t/absent.ml" 1 0 "lt"));
         refused (Mutate.arm (id "t/seven.ml" 99 0 "or"));
         refused (Mutate.arm (id "t/twins.ml" 4 2 "eq"));
       ])
  @@ __POS_OF__
       {|
    "lib/calc.ml:9" is not a mutant identifier: unknown rewrite "9" (expected one of not, lt, le, gt, ge, eq, neq, add, sub, fadd, fsub, and, or); expected <file>:<line>:<col>:<rewrite>
    t/absent.ml:1:0:lt: not this executable's mutant; it catalogues no site in t/absent.ml (if you expected one, is the library under test instrumented with ppx_windtrap.mutate?)
    t/seven.ml:99:0:or: no such mutation site; t/seven.ml has these:
        t/seven.ml:1:0:or
        t/seven.ml:2:0:or
        t/seven.ml:3:0:or
        t/seven.ml:4:0:or
        t/seven.ml:5:0:or
        t/seven.ml:6:0:or
        (and 1 more)
    t/twins.ml:4:2:eq: names 2 mutation sites, so no identifier can tell them apart (a rewriter duplicating locations?); dismiss the expression with [@mutate off] or exclude the file:
        t/twins.ml:4:2:eq
        t/twins.ml:4:2:eq
    |}

let runaway_printer () =
  expect
    (Printexc.to_string
       (Mutate.Runaway { id = id "t/print.ml" 2 0 "not"; hits = 4; budget = 3 }))
  @@ __POS_OF__
       {|
    Windtrap_runtime.Mutate.Runaway: t/print.ml:2:0:not evaluated 4 times (budget 3)
    |}

let arming =
  group "Arming"
    [
      test "with no mutant armed, every guard answers false" (fun () ->
          let g = Mutate.register ~file:"t/unarmed.ml" ~sites:one_site in
          disarm ();
          equal (list bool) [ false; false; false ] (List.map g [ 0; 1; 2 ]));
      test "an identifier of a file with no catalogued site is Uncatalogued"
        (disarming (fun () ->
             equal string "uncatalogued t/absent.ml:1:0:lt"
               (armed (Mutate.arm (id "t/absent.ml" 1 0 "lt")))));
      test "a file registered with an empty table is catalogued nowhere"
        (disarming (fun () ->
             register_only ~file:"t/empty.ml" ~sites:[||];
             equal string "uncatalogued t/empty.ml:1:0:lt"
               (armed (Mutate.arm (id "t/empty.ml" 1 0 "lt")))));
      cases
        "an identifier no site of a catalogued file matches is Unmatched, with \
         the file's mutants in order"
        ~name:Mutate.id_to_string
        [
          id "t/near.ml" 5 4 "lt";
          id "t/near.ml" 9 3 "lt";
          id "t/near.ml" 5 3 "gt";
          id "t/near.ml" 5 3 "plus";
        ]
        (fun i -> disarming (fun () -> unmatched i) ());
      test
        "an identifier of several sites of one file is Ambiguous, one \
         candidate per site, and arms none"
        (disarming ambiguous);
      test "arm is the mutant, and only its site's guard answers true"
        (disarming arms_one_site);
      test "a dismissed mutant arms"
        (disarming (fun () ->
             register_only ~file:"t/off.ml"
               ~sites:[| site 2 1 "add" ~dismissed:"equivalent" |];
             equal string "armed t/off.ml:2:1:add b -> a (off: equivalent)"
               (armed (Mutate.arm (id "t/off.ml" 2 1 "add")))));
      test "arm arms the site in every registration of an equal table"
        (disarming arms_every_registration);
      test "arm disarms the armed mutant first, even when it refuses"
        (disarming refusal_disarms);
      test "arming a second mutant drops the first's budget" (disarming rearm);
      test "the guard raises Runaway past the budget, at every later evaluation"
        (disarming runaway);
      test "reset_reach gives an armed site its whole budget again"
        (disarming headroom);
      test "the budget bounds each registration, and armed_hits adds them up"
        (disarming per_registration);
      test "armed_hits counts since reset_reach, not since arm"
        (disarming counted_since_reset);
      test "armed_hits is 0 when nothing is armed" (fun () ->
          disarm ();
          equal int 0 (Mutate.armed_hits ()));
      cases "a budget below 1 raises Invalid_argument and disarms nothing"
        ~name:string_of_int [ 0; -1; min_int ] (fun budget ->
          disarming (fun () -> refused_budget budget) ());
      test "the guard of a differing table never answers true nor raises"
        (disarming inert_armed);
      test "pp_arm_error names the identifier, the fault and the candidates"
        (disarming arm_error_messages);
      test "a runaway prints its mutant, its count and its budget"
        runaway_printer;
    ]

(* The reach map *)

type step = Eval of int | Drain | Next_epoch | Reset

let reach_sites = [| site 1 0 "lt"; site 2 0 "add" |]

let reach_rows =
  [
    ( "a drain is each site evaluated since the window opened, with its count",
      reach_sites,
      [ Eval 0; Eval 0; Eval 1; Drain ],
      [ "1:0:lt x2, 2:0:add x1" ] );
    ( "a drain empties the marks",
      reach_sites,
      [ Eval 0; Drain; Drain ],
      [ "1:0:lt x1"; "" ] );
    ( "a new epoch marks again the sites it evaluates, and only those",
      reach_sites,
      [
        Eval 0;
        Eval 1;
        Drain;
        Next_epoch;
        Eval 1;
        Drain;
        Next_epoch;
        Eval 0;
        Drain;
      ],
      [ "1:0:lt x1, 2:0:add x1"; "2:0:add x1"; "1:0:lt x1" ] );
    ( "a drain counts the evaluations of its window, not the earlier ones",
      reach_sites,
      [ Eval 0; Eval 0; Eval 0; Drain; Next_epoch; Eval 0; Drain ],
      [ "1:0:lt x3"; "1:0:lt x1" ] );
    ( "an evaluation between two windows goes to the next drain",
      reach_sites,
      [ Drain; Eval 0; Drain ],
      [ ""; "1:0:lt x1" ] );
    ( "the marks of an undrained epoch stay for the next drain",
      reach_sites,
      [ Eval 0; Next_epoch; Eval 1; Drain ],
      [ "1:0:lt x1, 2:0:add x1" ] );
    ( "an undrained mark counts every evaluation across epochs",
      reach_sites,
      [ Eval 0; Eval 0; Eval 0; Next_epoch; Eval 0; Drain ],
      [ "1:0:lt x4" ] );
    ( "an evaluation after a drain in the same epoch is never drained",
      reach_sites,
      [ Eval 0; Drain; Eval 0; Eval 0; Drain; Next_epoch; Eval 0; Drain ],
      [ "1:0:lt x1"; ""; "1:0:lt x1" ] );
    ( "reset_reach empties the marks and counts from zero",
      reach_sites,
      [ Eval 0; Eval 0; Eval 0; Reset; Drain; Eval 0; Drain ],
      [ ""; "1:0:lt x1" ] );
    ( "reset_reach opens a new epoch",
      reach_sites,
      [ Eval 0; Drain; Eval 0; Reset; Eval 0; Drain ],
      [ "1:0:lt x1"; "1:0:lt x1" ] );
    ( "duplicate sites of one table drain once, with every evaluation",
      [| site 1 0 "or" ~before:"x"; site 1 0 "or" ~before:"y" |],
      [ Eval 1; Eval 0; Eval 1; Drain ],
      [ "1:0:or x3" ] );
  ]

let drains (claim, sites, steps, expected) =
  let file = "t/reach/" ^ claim ^ ".ml" in
  let guard = Mutate.register ~file ~sites in
  fresh ();
  let run = function
    | Eval i ->
        ignore (guard i : bool);
        None
    | Drain -> Some (drained file)
    | Next_epoch ->
        Mutate.next_epoch ();
        None
    | Reset ->
        Mutate.reset_reach ();
        None
  in
  equal (list string) expected (List.filter_map run steps)

let reset_keeps_armed () =
  let g = Mutate.register ~file:"t/reset_armed.ml" ~sites:[| site 1 0 "ge" |] in
  ignore (arm_ok (id "t/reset_armed.ml" 1 0 "ge"));
  Mutate.reset_reach ();
  equal (list string) [ "true" ] (evaluations g 1)

let child_exe =
  Filename.concat
    (Filename.dirname Windtrap_runtime.Instr.executable)
    "mutate_child.exe"

(* The child runs in an empty directory with an identifier of its own in the
   variable that the core reads. *)
let child () =
  let cwd = temp_dir () in
  let r =
    Child.run ~cwd
      ~env:[ ("WINDTRAP_MUTATE_ARM", "lib/child.ml:3:10:lt") ]
      child_exe []
  in
  equal (pair int string) (0, "") (Child.exit_code r, r.err);
  (String.split_on_char '\n' r.out, Array.to_list (Sys.readdir cwd))

let reach_map =
  group "The reach map"
    [
      cases "drains" ~name:(fun (claim, _, _, _) -> claim) reach_rows drains;
      test "reset_reach does not disarm" (disarming reset_keeps_armed);
      test
        "in a fresh process, the first drain is module initialization's, and \
         the first window counts its own evaluations" (fun () ->
          let lines, _ = child () in
          equal (list string)
            [
              "initialization: lib/child.ml:3:10:lt x1";
              "window: lib/child.ml:3:10:lt x1";
            ]
            (List.filteri (fun i _ -> i < 2) lines));
      test "the runtime reads no environment variable and writes no file"
        (fun () ->
          let lines, left = child () in
          equal
            (pair (list string) (list string))
            ([ "guards: false false"; "" ], [])
            (List.filteri (fun i _ -> i >= 2) lines, left));
    ]

let () =
  exit (run "mutate" [ identifiers; sites_and_catalogue; arming; reach_map ])
