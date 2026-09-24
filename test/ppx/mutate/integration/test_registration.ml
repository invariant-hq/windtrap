(* The integration check. An instrumented library must compile, register
   its catalogue at module load, be observationally the original program
   with nothing armed, and actually change behaviour when a mutant is
   armed. *)

let baseline () =
  Printf.sprintf "%d %d %.1f %b %b %b %b %d %d %d %d %b"
    (Windtrap_mutate_forced.Forced.sum 2 3)
    (Windtrap_mutate_forced.Forced.diff 9 4)
    (Windtrap_mutate_forced.Forced.fsum 1.5 2.5)
    (Windtrap_mutate_forced.Forced.both true false)
    (Windtrap_mutate_forced.Forced.either false true)
    (Windtrap_mutate_forced.Forced.window 0 10 5)
    (Windtrap_mutate_forced.Forced.chain true true false)
    (Windtrap_mutate_forced.Forced.below 1 2)
    (Windtrap_mutate_forced.Forced.same 1 1)
    (Windtrap_mutate_forced.Forced.at_least 3 3)
    (Windtrap_mutate_forced.Forced.scaled 2 3)
    (Windtrap_mutate_forced.Forced.search (fun x -> x > 2) [ 1; 2; 3 ])

(* The behaviour battery. A golden proves the instrumenter emits what it
   emits; only running an armed program proves that what it emits means
   the rewrite the report names. [with_mutant ~file ~rewrite ~before f]
   finds the single mutant of [file] with that rendering, arms it, runs
   [f], and disarms - failing loudly if the mutant is not unique, which
   is also what keeps the [before] renderings under test. *)

let mutants_of file =
  List.filter
    (fun (m : Windtrap_runtime.Mutate.mutant) ->
      Filename.basename m.id.file = file)
    (Windtrap_runtime.Mutate.catalogue ())

let with_mutant ~file ~rewrite ~before f =
  let matching =
    List.filter
      (fun (m : Windtrap_runtime.Mutate.mutant) ->
        m.id.rewrite = rewrite && m.before = before)
      (mutants_of file)
  in
  (match matching with
  | [ m ] -> (
      match Windtrap_runtime.Mutate.arm m.Windtrap_runtime.Mutate.id with
      | Ok _ -> ()
      | Error e ->
          Format.kasprintf failwith "%a" Windtrap_runtime.Mutate.pp_arm_error e)
  | [] ->
      Format.kasprintf failwith "no %s mutant renders as %S in %s" rewrite
        before file
  | _ ->
      Format.kasprintf failwith "%d %s mutants render as %S in %s"
        (List.length matching) rewrite before file);
  f ();
  Windtrap_runtime.Mutate.disarm ()

let with_oracle_mutant = with_mutant ~file:"oracle.ml"

let check name condition =
  if not condition then Format.kasprintf failwith "behaviour check: %s" name

let behaviour () =
  let module O = Windtrap_mutate_forced.Oracle in
  (* Disarmed, every one of these is the original program. *)
  check "neg disarmed" (O.neg_pick true = 1);
  check "cmp disarmed" (O.cmp_lt 2 2 = 0 && O.cmp_le 2 2 = 1);
  check "con disarmed" ((not (O.con_and false true)) && O.con_or true false);
  check "ari disarmed" (O.ari_add 5 3 = 8 && O.ari_sub 5 3 = 2);
  (* [neg]: the condition, and only the condition, is negated. *)
  with_oracle_mutant ~rewrite:"not" ~before:"flag" (fun () ->
      check "neg armed true" (O.neg_pick true = 0);
      check "neg armed false" (O.neg_pick false = 1));
  (* [cmp]: each armed arm is the relation the report names, checked at
     the boundary where it and the original disagree, and agreeing with
     the original everywhere else. *)
  with_oracle_mutant ~rewrite:"le" ~before:"a < b" (fun () ->
      check "a < b -> a <= b" (O.cmp_lt 2 2 = 1 && O.cmp_lt 3 2 = 0));
  with_oracle_mutant ~rewrite:"lt" ~before:"a <= b" (fun () ->
      check "a <= b -> a < b" (O.cmp_le 2 2 = 0 && O.cmp_le 1 2 = 1));
  with_oracle_mutant ~rewrite:"ge" ~before:"a > b" (fun () ->
      check "a > b -> a >= b" (O.cmp_gt 2 2 = 1 && O.cmp_gt 1 2 = 0));
  with_oracle_mutant ~rewrite:"gt" ~before:"a >= b" (fun () ->
      check "a >= b -> a > b" (O.cmp_ge 2 2 = 0 && O.cmp_ge 3 2 = 1));
  with_oracle_mutant ~rewrite:"neq" ~before:"a = b" (fun () ->
      check "a = b -> a <> b" (O.cmp_eq 2 2 = 0 && O.cmp_eq 1 2 = 1));
  with_oracle_mutant ~rewrite:"eq" ~before:"a <> b" (fun () ->
      check "a <> b -> a = b" (O.cmp_ne 2 2 = 1 && O.cmp_ne 1 2 = 0));
  (* [con]: the whole four-row truth table of each connective, since the
     encoding expresses both through one branch and a row transcribed
     backwards would still pass a test that only checked one input. *)
  with_oracle_mutant ~rewrite:"or" ~before:"a && b" (fun () ->
      check "&& -> ||"
        (O.con_and true true && O.con_and true false && O.con_and false true
        && not (O.con_and false false)));
  with_oracle_mutant ~rewrite:"and" ~before:"a || b" (fun () ->
      check "|| -> &&"
        (O.con_or true true
        && (not (O.con_or true false))
        && (not (O.con_or false true))
        && not (O.con_or false false)));
  (* [ari]: the four operators, on operands that make each direction
     visible. *)
  with_oracle_mutant ~rewrite:"sub" ~before:"a + b" (fun () ->
      check "+ -> -" (O.ari_add 5 3 = 2));
  with_oracle_mutant ~rewrite:"add" ~before:"a - b" (fun () ->
      check "- -> +" (O.ari_sub 5 3 = 8));
  with_oracle_mutant ~rewrite:"fsub" ~before:"a +. b" (fun () ->
      check "+. -> -." (O.ari_fadd 5. 3. = 2.));
  with_oracle_mutant ~rewrite:"fadd" ~before:"a -. b" (fun () ->
      check "-. -> +." (O.ari_fsub 5. 3. = 8.));
  (* Evaluation order and multiplicity. Both operand-binding encodings
     evaluate each operand exactly once, right to left, armed or not. *)
  let order name run =
    ignore (run ());
    check name (O.record () = [ "r"; "l" ])
  in
  order "cmp order disarmed" (fun () -> O.cmp_order 1 2);
  order "ari order disarmed" (fun () -> O.ari_order 1 2);
  with_oracle_mutant ~rewrite:"le" ~before:"(note \"l\" a) < (note \"r\" b)"
    (fun () -> order "cmp order armed" (fun () -> O.cmp_order 1 2));
  with_oracle_mutant ~rewrite:"sub" ~before:"(note \"l\" a) + (note \"r\" b)"
    (fun () -> order "ari order armed" (fun () -> O.ari_order 1 2));
  (* Short-circuiting: the right operand runs exactly when the connective
     in force says it does, and never twice. *)
  let shortcut name a b expected =
    ignore (O.con_short a b);
    check name (O.record () = if expected then [ "r" ] else [])
  in
  shortcut "&& skips b on false" false true false;
  shortcut "&& runs b on true" true true true;
  with_oracle_mutant ~rewrite:"or" ~before:"a && (note \"r\" b)" (fun () ->
      shortcut "|| runs b on false" false true true;
      shortcut "|| skips b on true" true true false)

(* The typing-context corpus ([Disambiguate], see its dune stanza).
   That its library compiled at all is the regression test; what is
   left to prove here is that the tuple-lifted encodings still MEAN
   their rewrites, so one mutant of each reshaped shape - [ari]'s
   two-binder guard, its chain form, and [cmp]'s swapping form - is
   armed and its behaviour read at a point where the original and the
   mutant disagree. *)
let typing_context () =
  let module D = Windtrap_mutate_disambiguate.Disambiguate in
  let with_mutant = with_mutant ~file:"disambiguate.ml" in
  let rect = { D.Layout.x = 10; y = 20; width = 30; height = 40 } in
  let line = { D.Line.start = 2; end_ = 7 } in
  let sides left right = { D.Sides.left; right; top = 0.; bottom = 0. } in
  let item = { D.padding = sides 1. 2.; border = sides 3. 4. } in
  let collide = { D.Rect.x = 10; y = 20; width = 30; height = 40 } in
  check "disambiguate disarmed"
    (D.clip rect 0 0 100 100 = 130
    && D.span line = 5
    && D.inverted line = 0
    && D.horizontal item = 10.
    && D.clip_collide collide 5 0 100 100 = 130);
  with_mutant ~rewrite:"add" ~before:"rect.Rect.x - x0" (fun () ->
      check "clip_collide inner - -> +"
        (D.clip_collide collide 5 0 100 100 = 140));
  with_mutant ~rewrite:"add" ~before:"line.Line.end_ - line.start" (fun () ->
      check "span - -> +" (D.span line = 9));
  with_mutant ~rewrite:"le" ~before:"line.Line.end_ < line.start" (fun () ->
      check "inverted < -> <= at the boundary"
        (D.inverted { D.Line.start = 7; end_ = 7 } = 1));
  with_mutant ~rewrite:"fsub"
    ~before:
      "(((child.padding).left +. (child.padding).right) +. \
       (child.border).left) +. (child.border).right" (fun () ->
      check "horizontal chain +. -> -." (D.horizontal item = 2.))

(* The expected-type corpus ([Expected_type], same library): the same
   discipline. That it compiled is the regression test; one armed mutant
   of each pinned shape - a constructor and a record literal as the
   right operand - shows the annotated tuple still means its rewrite. *)
let expected_type () =
  let module E = Windtrap_mutate_disambiguate.Expected_type in
  let with_mutant = with_mutant ~file:"expected_type.ml" in
  check "expected_type disarmed"
    (E.before_green E.Color.Red = 1
    && E.before_green E.Color.Green = 0
    && E.past_green E.Color.Blue = 1
    && E.past_green E.Color.Green = 0
    && E.within E.Color.Green
    && (not (E.within E.Color.Blue))
    && E.below { E.Point.x = 0; y = 5 } = 1
    && E.below { E.Point.x = 0; y = 10 } = 0
    && E.is_green E.Color.Green = 1);
  with_mutant ~rewrite:"le" ~before:"c < Green" (fun () ->
      check "before_green < -> <= at the boundary"
        (E.before_green E.Color.Green = 1));
  with_mutant ~rewrite:"le" ~before:"p < { x = 0; y = 10 }" (fun () ->
      check "below < -> <= at the boundary"
        (E.below { E.Point.x = 0; y = 10 } = 1))

let () =
  behaviour ();
  typing_context ();
  expected_type ();
  let catalogue = Windtrap_runtime.Mutate.catalogue () in
  assert (Windtrap_runtime.Mutate.armed () = None);
  List.iter
    (fun (m : Windtrap_runtime.Mutate.mutant) ->
      assert (m.before <> "");
      assert (m.after <> ""))
    catalogue;
  (* Referenced so the linker keeps them: registration happens at module
     load, and the linker drops modules a binary never mentions. *)
  assert (Windtrap_mutate_forced.No_guard.cap 20 = 20);
  let forced =
    List.filter
      (fun (m : Windtrap_runtime.Mutate.mutant) ->
        Filename.basename m.id.file = "forced.ml")
      catalogue
  in
  assert (forced <> []);
  (* Arming changes what the program computes, and disarming puts it back
     exactly. Every mutant is tried rather than one named by position, so
     the check does not rot when the fixture is edited. *)
  let unarmed = baseline () in
  let changed =
    List.filter
      (fun (m : Windtrap_runtime.Mutate.mutant) ->
        (match Windtrap_runtime.Mutate.arm m.Windtrap_runtime.Mutate.id with
        | Ok _ -> ()
        | Error e ->
            Format.kasprintf failwith "%a" Windtrap_runtime.Mutate.pp_arm_error
              e);
        let armed = baseline () in
        Windtrap_runtime.Mutate.disarm ();
        assert (baseline () = unarmed);
        armed <> unarmed)
      forced
  in
  assert (changed <> []);
  assert (Windtrap_runtime.Mutate.armed () = None);
  Printf.printf "mutants: %d (forced: %d, %d of them observable here)\n"
    (List.length catalogue) (List.length forced) (List.length changed)
