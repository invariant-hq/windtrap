The coverage rewriter's expansions and refusals, as pp.exe prints them.
pp.exe is a standalone ppxlib driver linking every rewriter, and -apply
names the ones a run applies. The fixtures are read, never compiled.

  $ export OCAML_COLOR=never
  $ cov() { ../pp.exe -apply windtrap_coverage "$@"; }

An instrumented file opens with a module that registers its points,
shown here in full:

  $ cov --impl ./fixture_pipeline.ml
  [@@@ocaml.text "/*"]
  module Windtrap_cov_________fixture_pipeline___ml =
    struct
      let ___windtrap_visit___ =
        let counts = Array.make 5 0 in
        Windtrap_runtime.Coverage.register ~file:"./fixture_pipeline.ml"
          ~points:[|{
                      Windtrap_runtime.Coverage.start_ofs = 294;
                      Windtrap_runtime.Coverage.end_ofs = 299
                    };{
                        Windtrap_runtime.Coverage.start_ofs = 315;
                        Windtrap_runtime.Coverage.end_ofs = 326
                      };{
                          Windtrap_runtime.Coverage.start_ofs = 315;
                          Windtrap_runtime.Coverage.end_ofs = 336
                        };{
                            Windtrap_runtime.Coverage.start_ofs = 362;
                            Windtrap_runtime.Coverage.end_ofs = 373
                          };{
                              Windtrap_runtime.Coverage.start_ofs = 354;
                              Windtrap_runtime.Coverage.end_ofs = 384
                            }|] ~counts;
        (fun index -> Windtrap_runtime.Coverage.visit counts index)
      let ___windtrap_post_visit___ point_index result =
        ___windtrap_visit___ point_index; result
    end
  open Windtrap_cov_________fixture_pipeline___ml
  [@@@ocaml.text "/*"]
  let double x = ___windtrap_visit___ 0; x * 2
  let staged x =
    ___windtrap_visit___ 2;
    (___windtrap_post_visit___ 1 (x |> double)) |> double
  let bound x =
    ___windtrap_visit___ 4;
    (let y = ___windtrap_post_visit___ 3 (x |> double) in y + 1)

Every other expansion shows that module condensed by elide.exe to its
file and its points, each a byte extent of the source. elide.exe copies a
module of any other shape as it is.

  $ cov --impl ./fixture_all_off.ml | ../elide.exe
  let sign n = if n > 0 then 1 else 0[@@coverage off]
  let arms = ((function | 0 -> "zero" | _ -> "other")[@coverage off])
  module Off = struct let loop n = for _ = 1 to n do print_newline () done end
  [@@coverage off]
  [@@@coverage off]
  let rest n = match n with | 0 -> succ n | _ -> n

  $ cov --impl ./fixture_and_or.ml | ../elide.exe
  coverage points of "./fixture_and_or.ml" in Windtrap_cov_________fixture_and_or___ml, with ___windtrap_post_visit___:
    0: 459-460
    1: 454-460
    2: 478-479
    3: 483-484
    4: 503-504
    5: 508-509
    6: 513-514
    7: 570-573
    8: 570-573
    9: 543-554
  let both x y = ___windtrap_visit___ 1; x && ((___windtrap_visit___ 0; y))
  let either x y =
    ___windtrap_visit___ 2;
    if x
    then (___windtrap_visit___ 2; true)
    else if y then (___windtrap_visit___ 3; true) else false
  let chain a b c =
    ___windtrap_visit___ 4;
    if a
    then (___windtrap_visit___ 4; true)
    else
      if b
      then (___windtrap_visit___ 5; true)
      else if c then (___windtrap_visit___ 6; true) else false
  let rec search p =
    function
    | [] -> (___windtrap_visit___ 9; false)
    | x::rest ->
        (___windtrap_visit___ 8;
         if ___windtrap_post_visit___ 8 (p x)
         then (___windtrap_visit___ 7; true)
         else search p rest)

  $ cov --impl ./fixture_apply.ml | ../elide.exe
  coverage points of "./fixture_apply.ml" in Windtrap_cov_________fixture_apply___ml, with ___windtrap_post_visit___:
    0: 412-417
    1: 517-527
    2: 510-527
    3: 618-626
    4: 610-637
    5: 729-737
    6: 719-737
    7: 854-870
    8: 834-850
    9: 834-890
  let helper x = ___windtrap_visit___ 0; x + 1
  let nested x =
    ___windtrap_visit___ 2; helper (___windtrap_post_visit___ 1 (helper x))
  let bound () =
    ___windtrap_visit___ 4;
    (let r = ___windtrap_post_visit___ 3 (helper 1) in r + 1)
  let at_op x =
    ___windtrap_visit___ 6; helper @@ (___windtrap_post_visit___ 5 (helper x))
  let sequenced () =
    ___windtrap_visit___ 9;
    ___windtrap_post_visit___ 8 (print_string "a");
    ___windtrap_post_visit___ 7 (print_string "b");
    print_newline ()

  $ cov --impl ./fixture_class.ml | ../elide.exe
  coverage points of "./fixture_class.ml" in Windtrap_cov_________fixture_class___ml, with ___windtrap_post_visit___:
    0: 358-371
    1: 391-392
    2: 409-415
    3: 301-302
    4: 446-460
    5: 466-472
    6: 438-483
    7: 564-571
  class counter ?(step= ___windtrap_visit___ 3; 1)  () =
    object
      val mutable n = 0
      method bump = ___windtrap_visit___ 0; n <- n + step
      method value = ___windtrap_visit___ 1; n
      initializer ___windtrap_visit___ 2; n <- 0
    end
  let use () =
    ___windtrap_visit___ 6;
    (let c = ___windtrap_post_visit___ 4 ((new counter) ()) in
     ___windtrap_post_visit___ 5 c#bump; c#value)
  class virtual shape =
    object
      method virtual  area : int
      method name = ___windtrap_visit___ 7; "shape"
    end

  $ cov --impl ./fixture_empty.ml | ../elide.exe
  type t =
    | A 
    | B 
  let x = 1
  let s = "hello"

  $ cov --impl ./fixture_exclude.ml | ../elide.exe
  [@@@coverage exclude_file]
  let f n = if n > 0 then 1 else 0

  $ cov --impl ./fixture_if_loops.ml | ../elide.exe
  coverage points of "./fixture_if_loops.ml" in Windtrap_cov_________fixture_if_loops___ml, with ___windtrap_post_visit___:
    0: 184-185
    1: 176-178
    2: 162-185
    3: 155-156
    4: 141-185
    5: 215-240
    6: 202-240
    7: 300-306
    8: 300-306
    9: 261-319
    10: 380-399
    11: 335-416
  let sign n =
    ___windtrap_visit___ 4;
    if n > 0
    then (___windtrap_visit___ 3; 1)
    else
      (___windtrap_visit___ 2;
       if n < 0
       then (___windtrap_visit___ 1; (-1))
       else (___windtrap_visit___ 0; 0))
  let warn flag =
    ___windtrap_visit___ 6;
    if flag then (___windtrap_visit___ 5; print_endline "watch out")
  let count_up n =
    ___windtrap_visit___ 9;
    (let i = ref 0 in
     while (!i) < n do
       (___windtrap_visit___ 8; ___windtrap_post_visit___ 7 (incr i)) done;
     !i)
  let sum n =
    ___windtrap_visit___ 11;
    (let total = ref 0 in
     for i = 1 to n do (___windtrap_visit___ 10; total := ((!total) + i)) done;
     !total)

  $ cov --impl ./fixture_keys.ml | ../elide.exe
  coverage points of "./fixture_keys.ml" in Windtrap_cov_________fixture_keys___ml, with ___windtrap_post_visit___:
    0: 543-546
    1: 523-559
    2: 599-602
    3: 611-614
    4: 623-628
    5: 579-629
    6: 787-791
    7: 799-805
    8: 781-812
    9: 1123-1129
    10: 1133-1134
    11: 1123-1134
    12: 1157-1165
    13: 1169-1170
    14: 1157-1165
    15: 1157-1170
    16: 1382-1383
    17: 1387-1392
    18: 1374-1393
  let one f h =
    ___windtrap_visit___ 1;
    ignore
      (let a = ___windtrap_post_visit___ 0 (f 1) in
       ___windtrap_post_visit___ 0 (h a))
  let two f g h =
    ___windtrap_visit___ 5;
    ignore
      (let a = ___windtrap_post_visit___ 2 (f 1)
       and b = ___windtrap_post_visit___ 3 (g 2) in
       ___windtrap_post_visit___ 4 (h a b))
  let loop c f x =
    ___windtrap_visit___ 8;
    while ___windtrap_post_visit___ 6 (c ()) do
      (___windtrap_visit___ 7; ___windtrap_post_visit___ 7 (f @@ x)) done
  let bare x f y =
    ___windtrap_visit___ 11;
    if ___windtrap_post_visit___ 9 (x |> f)
    then (___windtrap_visit___ 9; true)
    else if y then (___windtrap_visit___ 10; true) else false
  let applied x f z y =
    ___windtrap_visit___ 15;
    if ___windtrap_post_visit___ 14 (x |> (f z))
    then (___windtrap_visit___ 12; true)
    else if y then (___windtrap_visit___ 13; true) else false
  let sent a (o : < get: bool   > ) =
    ___windtrap_visit___ 18;
    ignore
      (if a
       then (___windtrap_visit___ 16; true)
       else
         if ___windtrap_post_visit___ 17 o#get
         then (___windtrap_visit___ 17; true)
         else false)
  let extended = [%ext if true then succ 1 else 0]
  let attributed = ((0)[@attr if true then succ 1 else 0])
  [%%ext let x = if true then succ 1 else 0]
  type t = int[@@attr if true then succ 1 else 0]

  $ cov --impl ./fixture_lazy.ml | ../elide.exe
  coverage points of "./fixture_lazy.ml" in Windtrap_cov_________fixture_lazy___ml:
    0: 220-227
    1: 319-320
  let computed = lazy (___windtrap_visit___ 0; 1 + 2)
  let const = lazy 42
  let alias = lazy const
  let none = lazy None
  let thunk = lazy (fun x -> ___windtrap_visit___ 1; x)
  let constrained = lazy (42 : int)

  $ cov --impl ./fixture_letop.ml | ../elide.exe
  coverage points of "./fixture_letop.ml" in Windtrap_cov_________fixture_letop___ml:
    0: 144-147
    1: 167-173
    2: 219-224
    3: 203-224
    4: 266-272
  let ( let* ) x f = ___windtrap_visit___ 0; f x
  let ( and* ) a b = ___windtrap_visit___ 1; (a, b)
  let sum =
    let* a = 1
     in ___windtrap_visit___ 3; (let* b = 2
                                  in ___windtrap_visit___ 2; a + b)
  let pair = let* a = 1
             and* b = 2 in ___windtrap_visit___ 4; (a, b)

  $ cov --impl ./fixture_match.ml | ../elide.exe
  coverage points of "./fixture_match.ml" in Windtrap_cov_________fixture_match___ml, with ___windtrap_post_visit___:
    0: 127-138
    1: 150-155
    2: 143-169
    3: 181-186
    4: 174-200
    5: 110-222
    6: 261-281
    7: 246-255
    8: 242-281
  let classify n =
    ___windtrap_visit___ 5;
    (match n with
     | 0 -> (___windtrap_visit___ 0; "zero")
     | n when ___windtrap_visit___ 1; n > 0 ->
         (___windtrap_visit___ 2; "positive")
     | n when ___windtrap_visit___ 3; n < 0 ->
         (___windtrap_visit___ 4; "negative")
     | _ -> assert false)
  let safe_head l =
    ___windtrap_visit___ 8;
    (try ___windtrap_post_visit___ 7 (List.hd l)
     with | Failure _ -> (___windtrap_visit___ 6; "empty"))

  $ cov --impl ./fixture_off.ml | ../elide.exe
  coverage points of "./fixture_off.ml" in Windtrap_cov_________fixture_off___ml:
    0: 270-273
    1: 261-264
    2: 247-273
    3: 943-951
    4: 954-962
    5: 930-962
  let visible n =
    ___windtrap_visit___ 2;
    if n > 0
    then (___windtrap_visit___ 1; "p")
    else (___windtrap_visit___ 0; "n")
  let hidden n = ((if n > 0 then "p" else "n")[@coverage off])
  let skipped n = if n > 0 then "p" else "n"[@@coverage off]
  module Dark_module = struct let inside n = if n > 0 then "p" else "n" end
  [@@coverage off]
  [@@@coverage off]
  let dark n = match n with | 0 -> "z" | _ -> "x"
  module type T  = sig val v : int end
  module Packed = (val
    if Sys.word_size = 64
    then ((module struct let v = 1 end) : (module T))
    else ((module struct let v = 2 end) : (module T)))
  [@@@coverage on]
  let light n =
    ___windtrap_visit___ 5;
    (match n with
     | 0 -> (___windtrap_visit___ 3; "z")
     | _ -> (___windtrap_visit___ 4; "x"))

  $ cov --impl ./fixture_off_structure.ml | ../elide.exe
  coverage points of "./fixture_off_structure.ml" in Windtrap_cov_________fixture_off_structure___ml, with ___windtrap_post_visit___:
    0: 498-499
    1: 491-492
    2: 481-499
    3: 549-550
    4: 542-543
    5: 532-550
    6: 575-578
    7: 581-584
    8: 471-584
    9: 963-964
    10: 956-957
    11: 942-964
    12: 1064-1065
    13: 1057-1058
    14: 1043-1065
    15: 1240-1241
    16: 1233-1234
    17: 1219-1241
    18: 1342-1343
    19: 1335-1336
    20: 1321-1343
  module rec Dark:sig val f : int -> int end =
    struct let f n = if n > 0 then 1 else 0 end[@@coverage off]
  
  let local n =
    ___windtrap_visit___ 8;
    (let g x =
       ___windtrap_visit___ 2;
       if x then (___windtrap_visit___ 1; 1) else (___windtrap_visit___ 0; 0)
       [@@coverage off] in
     let h x =
       ___windtrap_visit___ 5;
       if x then (___windtrap_visit___ 4; 1) else (___windtrap_visit___ 3; 0)
       [@@coverage bogus] in
     (___windtrap_post_visit___ 6 (g n)) + (___windtrap_post_visit___ 7 (h n)))
  type t = int[@@coverage bogus]
  [@@@coverage off]
  module Inherits =
    struct
      let dark n = if n > 0 then 1 else 0
      [@@@coverage on]
      let lit_inside n =
        ___windtrap_visit___ 11;
        if n > 0
        then (___windtrap_visit___ 10; 1)
        else (___windtrap_visit___ 9; 0)
    end
  let still_dark n = if n > 0 then 1 else 0
  [@@@coverage on]
  let lit n =
    ___windtrap_visit___ 14;
    if n > 0
    then (___windtrap_visit___ 13; 1)
    else (___windtrap_visit___ 12; 0)
  module Unclosed =
    struct
      let lit_before n =
        ___windtrap_visit___ 17;
        if n > 0
        then (___windtrap_visit___ 16; 1)
        else (___windtrap_visit___ 15; 0)
      [@@@coverage off]
      let dark n = if n > 0 then 1 else 0
    end
  let after n =
    ___windtrap_visit___ 20;
    if n > 0
    then (___windtrap_visit___ 19; 1)
    else (___windtrap_visit___ 18; 0)
  let excluded = ((fun x -> ((x)[@coverage on]))[@coverage off])
  let excluded_binding x = ((x)[@coverage bogus])[@@coverage off]
  [@@@coverage off]
  let in_region x = ((x)[@coverage exclude_file])
  [@@@coverage on]

  $ cov --impl ./fixture_or_tail_branch.ml | ../elide.exe
  coverage points of "./fixture_or_tail_branch.ml" in Windtrap_cov_________fixture_or_tail_branch___ml, with ___windtrap_post_visit___:
    0: 796-801
    1: 818-839
    2: 796-839
    3: 858-863
    4: 900-905
    5: 881-894
    6: 858-905
    7: 1214-1219
    8: 1247-1274
    9: 1227-1241
    10: 1214-1274
  let rec or_match n =
    ___windtrap_visit___ 2;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (match n with | k -> (___windtrap_visit___ 1; or_match (k - 1)))
  let rec or_if n =
    ___windtrap_visit___ 6;
    if n = 0
    then (___windtrap_visit___ 3; true)
    else
      if n > 0
      then (___windtrap_visit___ 5; or_if (n - 1))
      else (___windtrap_visit___ 4; false)
  let rec or_try n =
    ___windtrap_visit___ 10;
    if n = 0
    then (___windtrap_visit___ 7; true)
    else
      (try ___windtrap_post_visit___ 9 (or_try (n - 1))
       with | Not_found -> (___windtrap_visit___ 8; or_try (n - 2)))

  $ cov --impl ./fixture_or_tail_scope.ml | ../elide.exe
  coverage points of "./fixture_or_tail_scope.ml" in Windtrap_cov_________fixture_or_tail_scope___ml:
    0: 516-521
    1: 516-562
    2: 586-591
    3: 586-635
    4: 664-669
    5: 664-724
    6: 756-761
    7: 756-813
    8: 834-837
    9: 862-867
    10: 895-905
    11: 862-905
  let rec or_let n =
    ___windtrap_visit___ 1;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (let next = n - 1 in or_let next)
  let rec or_open n =
    ___windtrap_visit___ 3;
    if n = 0
    then (___windtrap_visit___ 2; true)
    else (let open Stdlib in or_open (n - 1))
  let rec or_letmodule n =
    ___windtrap_visit___ 5;
    if n = 0
    then (___windtrap_visit___ 4; true)
    else (let module M = Stdlib in or_letmodule (n - 1))
  let rec or_letexception n =
    ___windtrap_visit___ 7;
    if n = 0
    then (___windtrap_visit___ 6; true)
    else (let exception E  in or_letexception (n - 1))
  let ( let* ) x f = ___windtrap_visit___ 8; f x
  let rec or_letop n =
    ___windtrap_visit___ 11;
    if n = 0
    then (___windtrap_visit___ 9; true)
    else (let* m = n - 1
           in ___windtrap_visit___ 10; or_letop m)

  $ cov --impl ./fixture_or_tail_wrap.ml | ../elide.exe
  coverage points of "./fixture_or_tail_wrap.ml" in Windtrap_cov_________fixture_or_tail_wrap___ml:
    0: 414-419
    1: 414-456
    2: 484-489
    3: 484-523
    4: 546-551
    5: 546-582
  let rec or_seq n =
    ___windtrap_visit___ 1;
    if n = 0
    then (___windtrap_visit___ 0; true)
    else (ignore n; or_seq (n - 1))
  let rec or_constraint n =
    ___windtrap_visit___ 3;
    if n = 0
    then (___windtrap_visit___ 2; true)
    else (or_constraint (n - 1) : bool)
  let rec or_coerce n =
    ___windtrap_visit___ 5;
    if n = 0
    then (___windtrap_visit___ 4; true)
    else (or_coerce (n - 1) :> bool)

  $ cov --impl ./fixture_out_edges.ml | ../elide.exe
  coverage points of "./fixture_out_edges.ml" in Windtrap_cov_________fixture_out_edges___ml, with ___windtrap_post_visit___:
    0: 205-206
    1: 275-286
    2: 373-384
    3: 365-395
    4: 503-517
    5: 503-517
    6: 542-556
    7: 542-561
    8: 703-719
    9: 695-726
    10: 745-765
    11: 857-864
    12: 867-878
    13: 835-878
    14: 974-975
    15: 967-968
    16: 946-975
    17: 1137-1143
    18: 1114-1143
    19: 1106-1150
    20: 1225-1243
    21: 1217-1250
    22: 1378-1381
    23: 1408-1426
    24: 1400-1433
    25: 1455-1473
    26: 1626-1633
    27: 1618-1640
  class counter = object method get = ___windtrap_visit___ 0; 0 end
  let make () = ___windtrap_visit___ 1; new counter
  let kept () =
    ___windtrap_visit___ 3;
    (let c = ___windtrap_post_visit___ 2 (new counter) in c#get)
  let checked x =
    ___windtrap_visit___ 5; ___windtrap_post_visit___ 4 (assert (x > 0))
  let checked_then x =
    ___windtrap_visit___ 7; ___windtrap_post_visit___ 6 (assert (x > 0)); x
  let qualified a b =
    ___windtrap_visit___ 9;
    (let r = ___windtrap_post_visit___ 8 (Stdlib.(+) a b) in r)
  let bare a b = ___windtrap_visit___ 10; (let r = a + b in r)
  let scrutinee l =
    ___windtrap_visit___ 13;
    (match List.rev l with
     | [] -> (___windtrap_visit___ 11; 0)
     | x::_ -> (___windtrap_visit___ 12; x))
  let condition l =
    ___windtrap_visit___ 16;
    if List.mem 0 l
    then (___windtrap_visit___ 15; 1)
    else (___windtrap_visit___ 14; 2)
  let at x =
    ___windtrap_visit___ 19;
    (let r =
       ___windtrap_post_visit___ 18
         ((Printf.sprintf "%d") @@ (___windtrap_post_visit___ 17 (succ x))) in
     r)
  let piped l =
    ___windtrap_visit___ 21;
    (let r = ___windtrap_post_visit___ 20 (l |> (List.map succ)) in r)
  let (|.) x f = ___windtrap_visit___ 22; f x
  let dotted l =
    ___windtrap_visit___ 24;
    (let r = ___windtrap_post_visit___ 23 (l |. (List.map succ)) in r)
  let dotted_tail l = ___windtrap_visit___ 25; l |. (List.map succ)
  let callee (o : < get: int -> int   > ) x =
    ___windtrap_visit___ 27;
    (let r = ___windtrap_post_visit___ 26 (o#get x) in r)

  $ cov --impl ./fixture_primitives.ml | ../elide.exe
  coverage points of "./fixture_primitives.ml" in Windtrap_cov_________fixture_primitives___ml, with ___windtrap_post_visit___:
    0: 290-291
    1: 309-310
    2: 1080-1083
    3: 277-1091
  let primitives a b x y r l s e f =
    ___windtrap_visit___ 3;
    (let _ = a && (___windtrap_visit___ 0; b) in
     let _ = a & (___windtrap_visit___ 1; b) in
     let _ = not a in
     let _ = x = y in
     let _ = x <> y in
     let _ = x < y in
     let _ = x <= y in
     let _ = x > y in
     let _ = x >= y in
     let _ = x == y in
     let _ = x != y in
     let _ = ref x in
     let _ = !r in
     let _ = r := x in
     let _ = l @ l in
     let _ = s ^ s in
     let _ = x + y in
     let _ = x - y in
     let _ = x * y in
     let _ = x / y in
     let _ = 1. +. 2. in
     let _ = 1. -. 2. in
     let _ = 1. *. 2. in
     let _ = 1. /. 2. in
     let _ = x mod y in
     let _ = x land y in
     let _ = x lor y in
     let _ = x lxor y in
     let _ = x lsl y in
     let _ = x lsr y in
     let _ = x asr y in
     let _ = raise e in
     let _ = raise_notrace e in
     let _ = failwith s in
     let _ = ignore x in
     let _ = Sys.opaque_identity x in
     let _ = Obj.magic x in
     let _ = x ## y in let _ = ___windtrap_post_visit___ 2 (f x) in ())

  $ cov --impl ./fixture_scope.ml | ../elide.exe
  coverage points of "./fixture_scope.ml" in Windtrap_cov_________fixture_scope___ml:
    0: 659-664
    1: 683-703
    2: 722-747
    3: 773-794
    4: 814-821
    5: 837-849
  let top_level = 1
  let greeting = "hello"
  let add a b = ___windtrap_visit___ 0; a + b
  let negate b = ___windtrap_visit___ 1; (let r = not b in r)
  let vanish x = ___windtrap_visit___ 2; (let () = ignore x in ())
  let labeled_only ~f = ___windtrap_visit___ 3; (let r = f ~x:1 in r)
  let tail_call x = ___windtrap_visit___ 4; add x 1
  let never () = ___windtrap_visit___ 5; assert false

  $ cov --impl ./fixture_tmc.ml | ../elide.exe
  coverage points of "./fixture_tmc.ml" in Windtrap_cov_________fixture_tmc___ml, with ___windtrap_post_visit___:
    0: 379-387
    1: 392-422
    2: 476-484
    3: 489-524
    4: 586-594
    5: 601-631
    6: 542-643
    7: 684-685
    8: 817-825
    9: 779-793
    10: 773-845
    11: 763-765
    12: 749-845
    13: 898-901
    14: 905-917
    15: 874-882
  let rec map f =
    function
    | [] -> (___windtrap_visit___ 0; [])
    | x::rest -> (___windtrap_visit___ 1; (f x) :: (map f rest))[@@tail_mod_cons
                                                                  ]
  let rec double =
    function
    | [] -> (___windtrap_visit___ 2; [])
    | x::rest -> (___windtrap_visit___ 3; (x * 2) :: (double rest))[@@ocaml.tail_mod_cons
                                                                     ]
  let local l =
    ___windtrap_visit___ 6;
    (let rec go =
       function
       | [] -> (___windtrap_visit___ 4; [])
       | x::rest -> (___windtrap_visit___ 5; (succ x) :: (go rest))[@@tail_mod_cons
                                                                     ] in
     go l)
  class cell = object method get = ___windtrap_visit___ 7; 0 end
  let rec cells (o : < get: int   > ) n =
    ___windtrap_visit___ 12;
    if n = 0
    then (___windtrap_visit___ 11; [])
    else
      (___windtrap_visit___ 10;
       ___windtrap_post_visit___ 9 (assert (n > 0));
       ignore o#get;
       (___windtrap_post_visit___ 8 (new cell))
       ::
       (cells o (n - 1)))[@@tail_mod_cons ]
  let rec plain f =
    function
    | [] -> (___windtrap_visit___ 15; [])
    | x::rest ->
        (___windtrap_visit___ 13;
         (___windtrap_post_visit___ 13 (f x))
         ::
         (___windtrap_post_visit___ 14 (plain f rest)))

Over code a deriver generated, for which generated.ml stands in:

  $ ../pp.exe -apply windtrap_test_generated,windtrap_coverage --impl ./fixture_generated.ml | ../elide.exe
  coverage points of "./fixture_generated.ml" in Windtrap_cov_________fixture_generated___ml, with ___windtrap_post_visit___:
    0: 343-373
    1: 397-405
    2: 390-405
    3: 647-648
    4: 675-676
    5: 705-720
    6: 723-732
  let entry x = ((print_int x)[@generated ])
  let edge x = ___windtrap_visit___ 0; ignore (((succ)[@generated ]) x)
  let written x =
    ___windtrap_visit___ 2; ignore (___windtrap_post_visit___ 1 (succ x))
  let arms =
    function
    | ((Some x)[@generated ]) -> (___windtrap_visit___ 3; x)
    | ((None)[@after_body ]) -> (___windtrap_visit___ 4; 0)
  let written_arms =
    function
    | Some n -> (___windtrap_visit___ 5; n + 1)
    | None -> (___windtrap_visit___ 6; 0)

Over a file the mutation rewriter ran on first, the order of the driver
dune builds for a stanza naming both backends:

  $ ../pp.exe -apply windtrap_mutate,windtrap_coverage --impl ./fixture_guards.ml | ../elide.exe
  coverage points of "./fixture_guards.ml" in Windtrap_cov_________fixture_guards___ml, with ___windtrap_post_visit___:
    0: 456-461
    1: 474-475
    2: 467-468
    3: 453-475
    4: 490-495
    5: 746-747
    6: 679-698
    7: 703-729
    8: 734-756
    9: 761-773
    10: 662-773
    11: 807-808
    12: 814-819
    13: 792-819
    14: 959-967
    15: 959-967
    16: 942-974
    17: 1008-1011
    18: 993-1012
    19: 1143-1144
  mutation sites of "./fixture_guards.ml" in Windtrap_mut_________fixture_guards___ml, with type 'a operands:
    0: 9:16 "le" "a < b" -> "a <= b"
    1: 10:14 "sub" "a + b" -> "a - b"
    2: 16:11 "not" "p x" -> "not (p x)"
    3: 17:11 "neq" "a = b" -> "a <> b"
    4: 18:11 "or" "a && b" -> "a || b"
    5: 21:27 "or" "a && b" -> "a || b"
    6: 21:20 "not" "c" -> "not c"
    7: 26:8 "not" "f x" -> "not (f x)"
    8: 30:24 "or" "(f x) && (g x)" -> "(f x) || (g x)"
    9: 34:17 "and" "a || b" -> "a && b"
  let lt a b =
    ___windtrap_visit___ 3;
    if
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
         ((a, b) : _ Windtrap_mut_________fixture_guards___ml.operands) in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 0
       then Stdlib.not (__windtrap_mut_0_r < __windtrap_mut_0_l)
       else (___windtrap_visit___ 0; __windtrap_mut_0_l < __windtrap_mut_0_r))
    then (___windtrap_visit___ 2; 1)
    else (___windtrap_visit___ 1; 0)
  let add a b =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
    if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l - __windtrap_mut_1_r
    else (___windtrap_visit___ 4; __windtrap_mut_1_l + __windtrap_mut_1_r)
  let arms x p a b =
    ___windtrap_visit___ 10;
    (match x with
     | 0 when
         let __windtrap_mut_2_p = p x in
         if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 2
         then Stdlib.not __windtrap_mut_2_p
         else __windtrap_mut_2_p -> (___windtrap_visit___ 6; "neg")
     | 1 when
         let __windtrap_mut_3_p = a = b in
         if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 3
         then Stdlib.not __windtrap_mut_3_p
         else __windtrap_mut_3_p -> (___windtrap_visit___ 7; "equality")
     | 2 when
         let __windtrap_mut_4_p = a in
         if
           Stdlib.(<>) (__windtrap_mut_4_p : Stdlib.Bool.t)
             (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 4)
         then (___windtrap_visit___ 5; b)
         else __windtrap_mut_4_p -> (___windtrap_visit___ 8; "con")
     | _ -> (___windtrap_visit___ 9; "other"))
  let both c a b =
    ___windtrap_visit___ 13;
    if
      (let __windtrap_mut_6_p = c in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 6
       then Stdlib.not __windtrap_mut_6_p
       else __windtrap_mut_6_p)
    then
      (let __windtrap_mut_5_p = a in
       if
         Stdlib.(<>) (__windtrap_mut_5_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 5)
       then (___windtrap_visit___ 11; b)
       else __windtrap_mut_5_p)
    else (___windtrap_visit___ 12; false)
  let rec loop f x =
    ___windtrap_visit___ 16;
    while
      (let __windtrap_mut_7_p = f x in
       if Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 7
       then Stdlib.not __windtrap_mut_7_p
       else __windtrap_mut_7_p)
      do (___windtrap_visit___ 15; ___windtrap_post_visit___ 14 (loop f x))
      done
  let left f g x =
    ___windtrap_visit___ 18;
    ignore
      (let __windtrap_mut_8_p = f x in
       if
         Stdlib.(<>) (__windtrap_mut_8_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 8)
       then (___windtrap_visit___ 17; ___windtrap_post_visit___ 17 (g x))
       else __windtrap_mut_8_p)
  let either a b =
    let __windtrap_mut_9_p = a in
    if
      Stdlib.(=) (__windtrap_mut_9_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_guards___ml.___windtrap_armed___ 9)
    then (___windtrap_visit___ 19; b)
    else __windtrap_mut_9_p

A file is instrumented under its own name, and returned as parsed under
an input name that is not a source file's:

  $ cov --impl ./fixture_input_name.ml | ../elide.exe
  coverage points of "./fixture_input_name.ml" in Windtrap_cov_________fixture_input_name___ml:
    0: 277-278
    1: 270-271
    2: 256-278
  let sign n =
    ___windtrap_visit___ 2;
    if n > 0 then (___windtrap_visit___ 1; 1) else (___windtrap_visit___ 0; 0)

  $ for name in //toplevel// '(stdin)' lib/.ocamlinit lib/topfind; do
  >   cov -loc-filename "$name" --impl ./fixture_input_name.ml
  > done
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0

A refusal is an error located at the attribute, and the driver exits 1:

  $ cov --impl ./reject_bad_payload.ml
  File "./reject_bad_payload.ml", line 1, characters 18-35:
  1 | let f n = (n + 1) [@coverage bogus]
                        ^^^^^^^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_double_off.ml
  File "./reject_double_off.ml", line 5, characters 0-17:
  5 | [@@@coverage off]
      ^^^^^^^^^^^^^^^^^
  Error: Coverage is already off.
  [1]

  $ cov --impl ./reject_empty_payload.ml
  File "./reject_empty_payload.ml", line 1, characters 18-29:
  1 | let f n = (n + 1) [@coverage]
                        ^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_exclude_file_binding.ml
  File "./reject_exclude_file_binding.ml", line 1, characters 16-41:
  1 | let f n = n + 1 [@@coverage exclude_file]
                      ^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_exclude_file_expr.ml
  File "./reject_exclude_file_expr.ml", line 1, characters 18-42:
  1 | let f n = (n + 1) [@coverage exclude_file]
                        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_misplaced_exclude_file.ml
  File "./reject_misplaced_exclude_file.ml", line 2, characters 2-28:
  2 |   [@@@coverage exclude_file]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: coverage exclude_file is not allowed here.
  [1]

  $ cov --impl ./reject_misplaced_on.ml
  File "./reject_misplaced_on.ml", line 1, characters 18-32:
  1 | let f n = (n + 1) [@coverage on]
                        ^^^^^^^^^^^^^^
  Error: coverage on is not allowed here.
  [1]

  $ cov --impl ./reject_off_reason.ml
  File "./reject_off_reason.ml", line 1, characters 18-42:
  1 | let f n = (n + 1) [@coverage off "reason"]
                        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]

  $ cov --impl ./reject_on_binding.ml
  File "./reject_on_binding.ml", line 1, characters 16-31:
  1 | let f n = n + 1 [@@coverage on]
                      ^^^^^^^^^^^^^^^
  Error: coverage on is not allowed here.
  [1]

  $ cov --impl ./reject_on_outside.ml
  File "./reject_on_outside.ml", line 3, characters 0-16:
  3 | [@@@coverage on]
      ^^^^^^^^^^^^^^^^
  Error: Coverage is already on.
  [1]

The two instrumenting rewriters read one attribute grammar, each under
its own name. Both accept every legal spelling:

  $ cov --impl ./spellings.ml > /dev/null
  $ sed s/coverage/mutate/g spellings.ml > mutate_spellings.ml
  $ ../pp.exe -apply windtrap_mutate --impl ./mutate_spellings.ml > /dev/null

The mutation rewriter refuses what the coverage rewriter refuses, in the
same words once the names are swapped and the width a name gives a span
is erased. reject_off_reason.ml has no twin: only the mutation rewriter's
off takes a reason.

  $ ns() { sed -E 's/[Cc]overage|[Mm]utate|[Mm]utation/NS/g; s/(characters [0-9]+-)[0-9]+/\1/; s/^( *)\^+$/\1^/'; }
  $ mkdir twin
  $ for f in reject_*.ml; do
  >   test "$f" = reject_off_reason.ml && continue
  >   sed s/coverage/mutate/g "$f" > "twin/$f"
  >   cov --impl "./$f" 2>&1 | ns > coverage.out
  >   (cd twin && ../../pp.exe -apply windtrap_mutate --impl "./$f" 2>&1) | ns > mutate.out
  >   diff coverage.out mutate.out && echo "$f: same"
  > done
  reject_bad_payload.ml: same
  reject_double_off.ml: same
  reject_empty_payload.ml: same
  reject_exclude_file_binding.ml: same
  reject_exclude_file_expr.ml: same
  reject_misplaced_exclude_file.ml: same
  reject_misplaced_on.ml: same
  reject_on_binding.ml: same
  reject_on_outside.ml: same
