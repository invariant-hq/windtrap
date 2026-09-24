(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Windtrap_runtime.Verdicts: the verdict lattice (killed
   anywhere wins, and the algebraic laws that make merging any number of
   files in any order give one answer), the v3 verdict format (exact bytes, round
   trip, every corruption class), deterministic output filenames, and
   the atomic write - the last also end to end through a child
   executable standing in for a mutation run's writing side. The
   runtime's own suite is test/instr/mutate: identifiers, the registry,
   arming.

   A windtrap suite ([run] executes tests sequentially in declaration
   order). *)

open Windtrap
module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts
module Child = Windtrap_test_support.Child

(* Printers and lookups the module does not export: they are for
   diagnostics and assertions, which is a test's business rather than a
   published surface. *)
let pp_id ppf (i : M.id) = Format.pp_print_string ppf (M.id_to_string i)
let pp_witness ppf w = Format.pp_print_string ppf (String.concat " > " w)

let pp_verdict ppf = function
  | V.Killed -> Format.pp_print_string ppf "killed"
  | V.Survived { witness; others } ->
      Format.fprintf ppf "survived by %a"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
           pp_witness)
        (witness :: others)
  | V.Unreached -> Format.pp_print_string ppf "unreached"

let find t id =
  List.find_opt (fun (r : V.record) -> M.compare_id r.V.id id = 0) (V.records t)

let is_empty t = V.records t = []
let verdict_t = Testable.structural ~pp:pp_verdict
let id ~file ~line ~col ~rewrite = { M.file; line; col; rewrite }

(* A verdict-file record. The rendering defaults are the shortest that
   still round-trip; the tests that are about the rendering pass their
   own. *)
let record ?(before = "b") ?(after = "a") id verdict =
  { V.id; before; after; verdict }

let pp_record ppf (r : V.record) =
  Format.fprintf ppf "%a %s -> %s: %a" pp_id r.V.id r.V.before r.V.after
    pp_verdict r.V.verdict

let record_t = Testable.structural ~pp:pp_record

(* Most assertions below are about the verdict alone; [find] hands back
   the whole record. *)
let verdict_of t id = Option.map (fun (r : V.record) -> r.V.verdict) (find t id)

let ok_error name = function
  | Ok v -> v
  | Error e -> failf "%s: unexpected error: %a" name V.pp_error e

(* Hermeticity: all paths are absolute, so the suite behaves identically
   under dune's sandbox and when run by hand from anywhere. The child
   executable sits next to this one; scratch files live in each test's
   temp_dir. Nothing is ever written under _build/_mutants. *)
let exe_dir = Filename.dirname Sys.executable_name
let scratch path = Filename.concat (temp_dir ()) path

(* The verdict lattice *)

let sample_verdicts =
  [
    V.Unreached;
    V.survived [ [ "a" ] ];
    V.survived [ [ "b"; "c" ] ];
    V.survived [ [ "a" ]; [ "b"; "c" ] ];
    V.Killed;
  ]

let verdict_tests =
  [
    test "killed anywhere wins" (fun () ->
        List.iter
          (fun killed ->
            List.iter
              (fun other ->
                equal
                  ~msg:
                    (Format.asprintf "%a merged with %a" pp_verdict killed
                       pp_verdict other)
                  verdict_t killed
                  (V.merge_verdict killed other);
                equal ~msg:"the other way round" verdict_t killed
                  (V.merge_verdict other killed))
              [
                V.Unreached;
                V.survived [ [ "a" ] ];
                V.survived [ [ "a" ]; [ "b" ] ];
              ])
          [ V.Killed ]);
    test "a survivor names at least one test" (fun () ->
        (* [Survived] with no witness would print as "no test ran this line
           and none failed when it changed", which is [Unreached]'s
           finding wearing the survivor's remedy. *)
        raises_match ~msg:"the constructor refuses it" Exn.invalid_arg
          (fun () -> V.survived []);
        (* The variant itself cannot hold one: [Survived] takes a witness
           and the rest, so the empty case has no spelling. A verdict file
           claiming otherwise is corrupt (see the parse group). *)
        equal ~msg:"one witness is one test" verdict_t (V.survived [ [ "a" ] ])
          (V.Survived { witness = [ "a" ]; others = [] }));
    test "survived only when every executable that reached it survived"
      (fun () ->
        equal ~msg:"survived and unreached" verdict_t (V.survived [ [ "a" ] ])
          (V.merge_verdict (V.survived [ [ "a" ] ]) V.Unreached);
        equal ~msg:"unreached and unreached" verdict_t V.Unreached
          (V.merge_verdict V.Unreached V.Unreached);
        equal ~msg:"witnesses union and deduplicate" verdict_t
          (V.survived [ [ "a" ]; [ "b" ]; [ "c" ] ])
          (V.merge_verdict
             (V.survived [ [ "b" ]; [ "a" ] ])
             (V.survived [ [ "c" ]; [ "b" ] ])));
    test "merge_verdict is commutative, associative and idempotent" (fun () ->
        List.iter
          (fun a ->
            equal
              ~msg:(Format.asprintf "idempotent on %a" pp_verdict a)
              verdict_t a (V.merge_verdict a a);
            equal
              ~msg:(Format.asprintf "unreached is the unit of %a" pp_verdict a)
              verdict_t a
              (V.merge_verdict a V.Unreached);
            List.iter
              (fun b ->
                equal
                  ~msg:
                    (Format.asprintf "commutative on %a, %a" pp_verdict a
                       pp_verdict b)
                  verdict_t (V.merge_verdict a b) (V.merge_verdict b a);
                List.iter
                  (fun c ->
                    equal
                      ~msg:
                        (Format.asprintf "associative on %a, %a, %a" pp_verdict
                           a pp_verdict b pp_verdict c)
                      verdict_t
                      (V.merge_verdict (V.merge_verdict a b) c)
                      (V.merge_verdict a (V.merge_verdict b c)))
                  sample_verdicts)
              sample_verdicts)
          sample_verdicts);
    test "a mutant killed by one executable and surviving another is killed"
      (fun () ->
        (* The whole reason the verdict file exists: reporting the
           surviving executable's view alone is a false survivor. *)
        let m = id ~file:"lib/core.ml" ~line:12 ~col:4 ~rewrite:"add" in
        let a = V.add V.empty (record m V.Killed) in
        let b = V.add V.empty (record m (V.survived [ [ "cli"; "runs" ] ])) in
        let c = V.add V.empty (record m V.Unreached) in
        List.iter
          (fun (name, t) ->
            equal ~msg:name (option verdict_t) (Some V.Killed) (verdict_of t m))
          [
            ("a then b then c", V.merge (V.merge a b) c);
            ("c then b then a", V.merge (V.merge c b) a);
            ("b then c then a", V.merge (V.merge b c) a);
            ("b then a", V.merge b a);
          ];
        equal ~msg:"without the killer it is a survivor" (option verdict_t)
          (Some (V.survived [ [ "cli"; "runs" ] ]))
          (verdict_of (V.merge b c) m));
    test "add combines rather than replaces, and normalizes witnesses"
      (fun () ->
        let m = id ~file:"lib/core.ml" ~line:1 ~col:0 ~rewrite:"or" in
        let t =
          V.add V.empty (record m (V.survived [ [ "b" ]; [ "a" ]; [ "b" ] ]))
        in
        equal ~msg:"sorted and deduplicated" (option verdict_t)
          (Some (V.survived [ [ "a" ]; [ "b" ] ]))
          (verdict_of t m);
        let t = V.add t (record m (V.survived [ [ "c" ] ])) in
        equal ~msg:"a second add unions" (option verdict_t)
          (Some (V.survived [ [ "a" ]; [ "b" ]; [ "c" ] ]))
          (verdict_of t m);
        let t = V.add t (record m V.Killed) in
        equal ~msg:"a kill overrides" (option verdict_t) (Some V.Killed)
          (verdict_of t m));
    test "a record carries the rendering the report draws" (fun () ->
        (* The catalogue lives in the instrumented binary; [windtrap
           mutants] links none of them. A record that named only its
           mutant would leave the project-level report unable to draw
           [a - b  ->  a + b], which is the block's whole point. *)
        let m = id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add" in
        let r =
          record ~before:"a - b" ~after:"a + b" m
            (V.survived [ [ "calc"; "adds" ] ])
        in
        let t = V.add V.empty r in
        equal ~msg:"kept whole" (option record_t) (Some r) (find t m);
        let round_tripped, _ =
          ok_error "round trip" (V.of_string (V.to_string t))
        in
        equal ~msg:"and survives the file" (option record_t) (Some r)
          (find round_tripped m));
    test "records disagreeing on a rendering merge deterministically" (fun () ->
        (* Only two builds of one source can produce this, and the data
           says nothing about which one the reader has open. So the rule
           is a total order rather than a guess: the smaller (before,
           after) pair is kept, whichever record it came with and in
           whichever order the records arrive. It is what keeps [merge]
           commutative and associative. The smaller pair rides with the
           losing verdict here, so keeping the winner's rendering fails. *)
        let m = id ~file:"lib/calc.ml" ~line:9 ~col:12 ~rewrite:"add" in
        let both ~msg ~expected a b =
          equal ~msg:(msg ^ ", in order") (option record_t) (Some expected)
            (find (V.add (V.add V.empty a) b) m);
          equal ~msg:(msg ^ ", reversed") (option record_t) (Some expected)
            (find (V.add (V.add V.empty b) a) m);
          equal
            ~msg:(msg ^ ", through merge either way")
            text
            (V.to_string (V.merge (V.add V.empty a) (V.add V.empty b)))
            (V.to_string (V.merge (V.add V.empty b) (V.add V.empty a)))
        in
        both ~msg:"the smaller before wins"
          ~expected:(record ~before:"a - b" ~after:"x + y" m V.Killed)
          (record ~before:"a - b" ~after:"x + y" m V.Unreached)
          (record ~before:"x - y" ~after:"a + b" m V.Killed);
        both ~msg:"an equal before leaves it to the smaller after"
          ~expected:(record ~before:"a - b" ~after:"a + b" m V.Killed)
          (record ~before:"a - b" ~after:"a + b" m V.Unreached)
          (record ~before:"a - b" ~after:"b + a" m V.Killed));
    test "merging collections is commutative, associative and idempotent"
      (fun () ->
        (* The per-verdict laws above are not enough: [merge] folds one
           collection into another, and a left- or right-biased union
           would satisfy them and still make two files' answer depend on
           the order the reporting command happened to read them in. The
           three collections overlap on every kind of pair. *)
        let of_list entries =
          List.fold_left
            (fun t (file, line, v) ->
              V.add t (record (id ~file ~line ~col:0 ~rewrite:"lt") v))
            V.empty entries
        in
        let a =
          of_list
            [
              ("lib/a.ml", 1, V.Killed);
              ("lib/a.ml", 2, V.survived [ [ "p" ] ]);
              ("lib/a.ml", 3, V.Unreached);
              ("lib/only_a.ml", 1, V.Killed);
            ]
        and b =
          of_list
            [
              ("lib/a.ml", 1, V.Killed);
              ("lib/a.ml", 2, V.Unreached);
              ("lib/a.ml", 3, V.survived [ [ "q" ] ]);
              ("lib/only_b.ml", 1, V.Unreached);
            ]
        and c =
          of_list
            [
              ("lib/a.ml", 1, V.Unreached);
              ("lib/a.ml", 2, V.survived [ [ "p" ]; [ "r" ] ]);
              ("lib/a.ml", 3, V.Killed);
            ]
        in
        let bytes = V.to_string in
        equal ~msg:"idempotent" text (bytes a) (bytes (V.merge a a));
        equal ~msg:"empty is the unit" text (bytes a)
          (bytes (V.merge a V.empty));
        equal ~msg:"empty is the unit on the left" text (bytes a)
          (bytes (V.merge V.empty a));
        equal ~msg:"commutative" text
          (bytes (V.merge a b))
          (bytes (V.merge b a));
        equal ~msg:"associative" text
          (bytes (V.merge (V.merge a b) c))
          (bytes (V.merge a (V.merge b c)));
        (* And the answer itself, not merely its stability. *)
        equal ~msg:"the merged verdicts" (list string)
          [
            "lib/a.ml:1:0:lt killed";
            "lib/a.ml:2:0:lt survived by p, r";
            "lib/a.ml:3:0:lt killed";
            "lib/only_a.ml:1:0:lt killed";
            "lib/only_b.ml:1:0:lt unreached";
          ]
          (List.map
             (fun (r : V.record) ->
               Format.asprintf "%a %a" pp_id r.V.id pp_verdict r.V.verdict)
             (V.records (V.merge (V.merge c b) a))));
    test "collections order their bindings by identifier" (fun () ->
        let t =
          List.fold_left
            (fun t (file, line, v) ->
              V.add t (record (id ~file ~line ~col:0 ~rewrite:"lt") v))
            V.empty
            [
              ("lib/z.ml", 1, V.Unreached);
              ("lib/a.ml", 9, V.survived [ [ "t" ] ]);
              ("lib/a.ml", 2, V.Killed);
            ]
        in
        is_false ~msg:"not empty" (is_empty t);
        is_true ~msg:"empty is empty" (is_empty V.empty);
        equal ~msg:"bindings" (list string)
          [ "lib/a.ml:2:0:lt"; "lib/a.ml:9:0:lt"; "lib/z.ml:1:0:lt" ]
          (List.map (fun (r : V.record) -> M.id_to_string r.V.id) (V.records t));
        is_none ~msg:"an absent identifier"
          (find t (id ~file:"lib/a.ml" ~line:3 ~col:0 ~rewrite:"lt")));
  ]

(* The verdict file format *)

let sample_collection () =
  V.add
    (V.add
       (V.add V.empty
          (record ~before:"a - b" ~after:"a + b"
             (id ~file:"lib/a.ml" ~line:1 ~col:2 ~rewrite:"add")
             V.Unreached))
       (record ~before:"p && q" ~after:"not (p && q)"
          (id ~file:"lib/b.ml" ~line:3 ~col:4 ~rewrite:"not")
          V.Killed))
    (record ~before:"a || b" ~after:"a && b"
       (id ~file:"lib/b.ml" ~line:5 ~col:0 ~rewrite:"or")
       (V.survived [ [ "x" ]; [ "y"; "z" ] ]))

let sample_bytes =
  "windtrap-mutants-v3\n\
   3\n\
   8 lib/a.ml 1 2 3 add 5 a - b 5 a + b unreached\n\
   8 lib/b.ml 3 4 3 not 6 p && q 12 not (p && q) killed\n\
   8 lib/b.ml 5 0 2 or 6 a || b 6 a && b survived 2 1 1 x 2 1 y 1 z\n"

let digest = String.make 32 'a'

let format_tests =
  [
    (* The magic line and the records in identifier order are the
       format's stated contract (verdicts.mli, "Verdict files"; the header
       is instr.mli's grammar). The layout of a record is stated nowhere,
       and these bytes pin it as this release writes it. *)
    test "to_string is the v3 encoding: magic, count, records by identifier"
      (fun () ->
        equal ~msg:"exact bytes" text sample_bytes
          (V.to_string (sample_collection ())));
    test "an identity is recorded after the magic line" (fun () ->
        equal ~msg:"exact bytes" text
          ("windtrap-mutants-v3\nexe " ^ digest ^ " 10 test/a.exe\n0\n")
          (V.to_string ~identity:{ V.exe = "test/a.exe"; digest } V.empty);
        raises_match ~msg:"an empty exe is refused" Exn.invalid_arg (fun () ->
            V.to_string ~identity:{ V.exe = ""; digest } V.empty);
        raises_match ~msg:"a short digest is refused" Exn.invalid_arg (fun () ->
            V.to_string ~identity:{ V.exe = "a"; digest = "abc" } V.empty);
        raises_match ~msg:"a non-hex digest is refused" Exn.invalid_arg
          (fun () ->
            V.to_string
              ~identity:{ V.exe = "a"; digest = String.make 32 'X' }
              V.empty));
    test "of_string inverts to_string, identity included" (fun () ->
        let t = sample_collection () in
        let identity = { V.exe = "_build/test/a.exe"; digest } in
        let parsed, recorded =
          ok_error "round trip" (V.of_string (V.to_string ~identity t))
        in
        equal ~msg:"the collection" text (V.to_string t) (V.to_string parsed);
        equal ~msg:"the identity"
          (option (pair string string))
          (Some ("_build/test/a.exe", digest))
          (Option.map (fun (i : V.identity) -> (i.V.exe, i.V.digest)) recorded);
        let parsed, recorded =
          ok_error "no identity" (V.of_string (V.to_string t))
        in
        is_none ~msg:"none recorded" recorded;
        equal ~msg:"the collection" text (V.to_string t) (V.to_string parsed));
    test "witnesses holding spaces and newlines survive the round trip"
      (fun () ->
        let t =
          V.add V.empty
            (record ~before:"a\n= b" ~after:"a\n<> b"
               (id ~file:"lib/odd names.ml" ~line:1 ~col:0 ~rewrite:"eq")
               (V.survived [ [ "a group"; "a test\nwith a newline" ]; [ "" ] ]))
        in
        let parsed, _ = ok_error "round trip" (V.of_string (V.to_string t)) in
        equal ~msg:"identical" text (V.to_string t) (V.to_string parsed));
    test "serialization does not depend on construction order" (fun () ->
        let ids =
          [
            (id ~file:"lib/b.ml" ~line:2 ~col:0 ~rewrite:"lt", V.Unreached);
            ( id ~file:"lib/a.ml" ~line:1 ~col:0 ~rewrite:"or",
              V.survived [ [ "q" ]; [ "p" ] ] );
            (id ~file:"lib/a.ml" ~line:9 ~col:0 ~rewrite:"sub", V.Killed);
          ]
        in
        let build order =
          V.to_string
            (List.fold_left
               (fun t (i, v) -> V.add t (record i v))
               V.empty order)
        in
        equal ~msg:"reversed insertion" text (build ids) (build (List.rev ids)));
    test "empty collections round-trip" (fun () ->
        equal ~msg:"bytes" text "windtrap-mutants-v3\n0\n" (V.to_string V.empty);
        let parsed, _ =
          ok_error "parse" (V.of_string "windtrap-mutants-v3\n0\n")
        in
        is_true ~msg:"still empty" (is_empty parsed));
  ]

(* Rejections *)

let check_corrupt name ~sub s =
  match V.of_string s with
  | Error (V.Corrupt { reason; _ }) -> contains ~msg:name ~sub reason
  | Error e -> failf "%s: expected Corrupt, got %a" name V.pp_error e
  | Ok _ -> failf "%s: parsed, expected a rejection mentioning %S" name sub

let rejection_tests =
  [
    cases "an unknown header is refused, never converted"
      ~name:(fun (name, _) -> name)
      [
        ("empty", "");
        ("coverage file", "windtrap-coverage-v3\n1\n");
        ("future version", "windtrap-mutants-v4\n0\n");
        ("uppercase", "WINDTRAP-MUTANTS-V1\n0\n");
        ("prefix without separator", "windtrap-mutants-v30\n0\n");
        ("plain text", "hello\nworld\n");
      ]
      (fun (name, s) ->
        match V.of_string ~path:"f.mutants" s with
        | Error (V.Unknown_format { path; header }) ->
            equal ~msg:"path" string "f.mutants" path;
            let rendered =
              Format.asprintf "%a" V.pp_error
                (V.Unknown_format { path; header })
            in
            contains ~msg:"the message names the expected magic"
              ~sub:"windtrap-mutants-v3" rendered
        | Error e ->
            failf "%s: expected Unknown_format, got %a" name V.pp_error e
        | Ok _ -> failf "%s: parsed, expected a rejection" name);
    cases "corrupt data is refused"
      ~name:(fun (name, _, _) -> name)
      [
        ( "truncated records",
          "windtrap-mutants-v3\n2\n8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n",
          "expected" );
        ( "record count exceeds data",
          "windtrap-mutants-v3\n99999999\n",
          "exceeds data" );
        ("negative record count", "windtrap-mutants-v3\n-1\n", "negative");
        (* The magic alone is not an empty collection: a truncated file
           must not read as "this executable killed nothing". *)
        ("magic only", "windtrap-mutants-v3", "expected record count");
        ( "negative witness count",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a survived 1 -1\n",
          "negative test path length" );
        ( "line 0",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 0 2 3 add 1 b 1 a unreached\n",
          "1-based" );
        ( "negative line",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml -3 2 3 add 0 0 1 b 1 a unreached\n",
          "negative line" );
        ( "negative column",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 -2 3 add 0 0 1 b 1 a unreached\n",
          "negative column" );
        ( "empty file name",
          "windtrap-mutants-v3\n1\n0  1 2 3 add 0 0 1 b 1 a unreached\n",
          "empty file name" );
        ( "truncated file name",
          "windtrap-mutants-v3\n1\n80 lib/a.ml 1 2 3 add 1 b 1 a unreached\n",
          "truncated" );
        ( "unknown rewrite",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 4 plus 1 b 1 a unreached\n",
          "unknown rewrite" );
        ( "truncated rendering",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 80 b 1 a unreached\n",
          "truncated before" );
        ( "missing rendering",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add unreached\n",
          "before" );
        ( "unknown verdict",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a errored\n",
          "unknown verdict" );
        ( "missing verdict",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a\n",
          "expected verdict" );
        ( "truncated witness",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 2 1 1 g\n",
          "expected" );
        ( "witness count exceeds data",
          "windtrap-mutants-v3\n\
           1\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a survived 99999999\n",
          "exceeds data" );
        (* A survivor with no witness is not a survivor: it is what an
           unreached mutant looks like when a writer confuses the two, and
           it would render as "0 tests ran this line and none failed". *)
        ( "survivor with no witness",
          "windtrap-mutants-v3\n1\n8 lib/a.ml 1 2 3 add 1 b 1 a survived 0\n",
          "names no test" );
        ( "duplicate record",
          "windtrap-mutants-v3\n\
           2\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n\
           8 lib/a.ml 1 2 3 add 1 b 1 a killed\n",
          "duplicate record" );
        ("trailing data", "windtrap-mutants-v3\n0\nextra\n", "trailing data");
        ( "short identity digest",
          "windtrap-mutants-v3\nexe abcd 1 a\n0\n",
          "digest" );
        ( "empty identity exe",
          "windtrap-mutants-v3\nexe " ^ String.make 32 'a' ^ " 0 \n0\n",
          "empty executable identity" );
      ]
      (fun (name, s, sub) -> check_corrupt name ~sub s);
    test "load reports an unreadable file" (fun () ->
        match V.load (scratch "does-not-exist.mutants") with
        | Error (V.Unreadable { path; _ }) ->
            contains ~msg:"path" ~sub:"does-not-exist.mutants" path
        | Error e -> failf "expected Unreadable, got %a" V.pp_error e
        | Ok _ -> fail "a missing file must not parse");
    test "pp_error names the fix for a foreign file" (fun () ->
        contains ~msg:"suggests deleting the stale files"
          ~sub:"delete the stale verdict files"
          (Format.asprintf "%a" V.pp_error
             (V.Unknown_format { path = "f"; header = "junk" })));
  ]

(* Output paths *)

let filename_tests =
  [
    test "output_file is deterministic and sandbox-invariant" (fun () ->
        let direct = V.output_file ~exe:"/home/p/_build/default/test/t.exe" in
        let sandboxed =
          V.output_file
            ~exe:"/home/p/_build/.sandbox/deadbeef/default/test/t.exe"
        in
        equal ~msg:"the same file either way" string direct sandboxed;
        is_true ~msg:"under the project's _build/_mutants"
          (String.starts_with ~prefix:"/home/p/_build/_mutants/windtrap-" direct);
        is_true ~msg:"under a private build directory's own _mutants"
          (String.starts_with ~prefix:"/home/p/_build_ci/_mutants/windtrap-"
             (V.output_file ~exe:"/home/p/_build_ci/default/test/t.exe"));
        is_true ~msg:"under _windtrap/mutants for an executable outside any"
          (String.starts_with
             ~prefix:
               (Filename.concat (Sys.getcwd ()) "_windtrap/mutants/windtrap-")
             (V.output_file ~exe:"/usr/local/bin/t"));
        is_true ~msg:"named .mutants" (Filename.check_suffix direct ".mutants");
        not_equal ~msg:"a different executable gets a different file" string
          direct
          (V.output_file ~exe:"/home/p/_build/default/test/other.exe");
        (* One executable is one verdict file, whichever way the run that
           wrote it named the binary: a key that kept [.] and [..] would
           file one per spelling and leave all but the last for the
           report to call stale. *)
        equal ~msg:"a . component is not a directory" string direct
          (V.output_file ~exe:"/home/p/_build/default/test/./t.exe");
        equal ~msg:"a .. is the directory above it" string direct
          (V.output_file ~exe:"/home/p/_build/default/test/sub/../t.exe"));
    test "writer_identity digests the executable" (fun () ->
        let exe = Filename.concat exe_dir "save_child.exe" in
        match V.writer_identity ~exe with
        | None -> fail "the child executable must be readable"
        | Some i ->
            equal ~msg:"exe" string (V.exe_identity ~exe) i.V.exe;
            equal ~msg:"digest" string
              (Digest.to_hex (Digest.file exe))
              i.V.digest);
    test "writer_identity is None for an unreadable executable" (fun () ->
        is_none ~msg:"absent" (V.writer_identity ~exe:(scratch "no-such-exe")));
  ]

(* The atomic write, in this process and in another *)

let file_tests =
  [
    test "save writes atomically and load reads it back" (fun () ->
        let path = scratch "sub/dir/verdicts.mutants" in
        let t = sample_collection () in
        let identity = { V.exe = "test/a.exe"; digest } in
        V.save ~identity path t;
        let parsed, recorded = ok_error "load" (V.load path) in
        equal ~msg:"round trip" text (V.to_string t) (V.to_string parsed);
        equal ~msg:"identity" (option string) (Some "test/a.exe")
          (Option.map (fun (i : V.identity) -> i.V.exe) recorded);
        equal ~msg:"no temporary files are left behind" (list string) []
          (Sys.readdir (Filename.dirname path)
          |> Array.to_list
          |> List.filter (fun n -> Filename.check_suffix n ".tmp"));
        (* Re-saving replaces; verdicts never accumulate on disk. *)
        V.save path V.empty;
        let parsed, recorded = ok_error "reload" (V.load path) in
        is_true ~msg:"replaced" (is_empty parsed);
        is_none ~msg:"the identity is gone too" recorded);
    test "save refuses a malformed identity before touching the disk" (fun () ->
        let path = scratch "unwritten.mutants" in
        raises_match ~msg:"refused" Exn.invalid_arg (fun () ->
            V.save ~identity:{ V.exe = "a"; digest = "short" } path V.empty);
        is_false ~msg:"nothing was written" (Sys.file_exists path));
  ]

(* The child executable *)

let child_exe = Filename.concat exe_dir "save_child.exe"

let child_tests =
  [
    test "a child writes a verdict file the parent can load" (fun () ->
        let dir = temp_dir () in
        let path = Filename.concat dir "child.mutants" in
        let r = Child.run child_exe [ "save"; path ] in
        equal ~msg:"exit code" int 0 (Child.exit_code r);
        equal ~msg:"stderr" text "" r.Child.err;
        let t, recorded = ok_error "load" (V.load path) in
        equal ~msg:"the record" (option record_t)
          (Some
             (record ~before:"l < r" ~after:"not (r < l)"
                (id ~file:"lib/child.ml" ~line:3 ~col:10 ~rewrite:"lt")
                (V.survived [ [ "child"; "less" ] ])))
          (find t (id ~file:"lib/child.ml" ~line:3 ~col:10 ~rewrite:"lt"));
        equal ~msg:"the writer identity"
          (option (pair string string))
          (Some
             ( V.exe_identity ~exe:child_exe,
               Digest.to_hex (Digest.file child_exe) ))
          (Option.map (fun (i : V.identity) -> (i.V.exe, i.V.digest)) recorded);
        equal ~msg:"no temporary files are left behind" (list string) []
          (Sys.readdir dir |> Array.to_list
          |> List.filter (fun n -> Filename.check_suffix n ".tmp")));
  ]

(* The suite *)

let () =
  exit
  @@ run "mutate_verdicts"
       [
         group "verdicts" verdict_tests;
         group "format" format_tests;
         group "parse" rejection_tests;
         group "filenames" filename_tests;
         group "files" file_tests;
         group "child" child_tests;
       ]
