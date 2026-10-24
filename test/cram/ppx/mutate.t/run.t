The mutation rewriter's expansions and refusals, as pp.exe prints them
(see ../coverage.t).

  $ export OCAML_COLOR=never
  $ mut() { ../pp.exe -apply windtrap_mutate "$@"; }

A mutated file opens with a module that registers its sites, shown here
in full. The file is mutated under its own name:

  $ mut --impl ./fixture_input_name.ml
  [@@@ocaml.text "/*"]
  module Windtrap_mut_________fixture_input_name___ml =
    struct
      type site = Windtrap_runtime.Mutate.site =
        {
        line: int ;
        col: int ;
        end_line: int ;
        end_col: int ;
        rewrite: string ;
        before: string ;
        after: string ;
        dismissed: string option }
      type 'a operands = ('a * 'a)
      let ___windtrap_armed___ =
        Windtrap_runtime.Mutate.register ~file:"./fixture_input_name.ml"
          ~sites:[|{
                     line = 5;
                     col = 34;
                     end_line = 5;
                     end_col = 35;
                     rewrite = "int";
                     before = "0";
                     after = "1";
                     dismissed = None
                   };{
                       line = 5;
                       col = 27;
                       end_line = 5;
                       end_col = 28;
                       rewrite = "int";
                       before = "1";
                       after = "0";
                       dismissed = None
                     };{
                         line = 5;
                         col = 16;
                         end_line = 5;
                         end_col = 21;
                         rewrite = "ge";
                         before = "n > 0";
                         after = "n >= 0";
                         dismissed = None
                       };{
                           line = 5;
                           col = 20;
                           end_line = 5;
                           end_col = 21;
                           rewrite = "int";
                           before = "0";
                           after = "1";
                           dismissed = None
                         }|]
    end
  [@@@ocaml.text "/*"]
  let sign n =
    if
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((n,
           (if
              Windtrap_mut_________fixture_input_name___ml.___windtrap_armed___
                3
            then 1
            else 0)) : _ Windtrap_mut_________fixture_input_name___ml.operands) in
      (if Windtrap_mut_________fixture_input_name___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r > __windtrap_mut_2_l)
       else __windtrap_mut_2_l > __windtrap_mut_2_r)
    then
      (if Windtrap_mut_________fixture_input_name___ml.___windtrap_armed___ 1
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_input_name___ml.___windtrap_armed___ 0
      then 1
      else 0

Every other expansion shows that module condensed by elide.exe to its
file and its sites: where each starts and ends, the rewrite, the text
before and after, and the reason of a dismissed site.

  $ mut --impl ./fixture_all_dismissed.ml | ../elide.exe
  mutation sites of "./fixture_all_dismissed.ml" in Windtrap_mut_________fixture_all_dismissed___ml:
    0: 9:7-9:38 "int" "16" -> "17", dismissed "a capacity"
    1: 8:5-8:16 "ge" "want > 16" -> "want >= 16", dismissed "both arms yield 16 at the boundary"
  let cap want =
    if ((want > 16)[@mutate off "both arms yield 16 at the boundary"])
    then want
    else ((16)[@mutate off "a capacity"])

  $ mut --impl ./fixture_ari.ml | ../elide.exe
  mutation sites of "./fixture_ari.ml" in Windtrap_mut_________fixture_ari___ml:
    0: 5:14-5:19 "sub" "a + b" -> "a - b"
    1: 6:15-6:20 "add" "a - b" -> "a + b"
    2: 7:15-7:21 "fsub" "a +. b" -> "a -. b"
    3: 8:16-8:22 "fadd" "a -. b" -> "a +. b"
    4: 13:13-13:18 "sub" "1 + 2" -> "1 - 2"
    5: 17:17-17:24 "sub" "a + b" -> "a - b"
    6: 17:27-17:28 "int" "2" -> "3"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let diff a b =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l + __windtrap_mut_1_r
    else __windtrap_mut_1_l - __windtrap_mut_1_r
  let fsum a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l -. __windtrap_mut_2_r
    else __windtrap_mut_2_l +. __windtrap_mut_2_r
  let fdiff a b =
    let (__windtrap_mut_3_l, __windtrap_mut_3_r) = (a, b) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 3
    then __windtrap_mut_3_l +. __windtrap_mut_3_r
    else __windtrap_mut_3_l -. __windtrap_mut_3_r
  let origin =
    let (__windtrap_mut_4_l, __windtrap_mut_4_r) = (1, 2) in
    if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 4
    then __windtrap_mut_4_l - __windtrap_mut_4_r
    else __windtrap_mut_4_l + __windtrap_mut_4_r
  let negate x = - x
  let scaled a b =
    (let (__windtrap_mut_5_l, __windtrap_mut_5_r) = (a, b) in
     if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 5
     then __windtrap_mut_5_l - __windtrap_mut_5_r
     else __windtrap_mut_5_l + __windtrap_mut_5_r) *
      (if Windtrap_mut_________fixture_ari___ml.___windtrap_armed___ 6
       then 3
       else 2)

  $ mut --impl ./fixture_del.ml | ../elide.exe
  mutation sites of "./fixture_del.ml" in Windtrap_mut_________fixture_del___ml:
    0: 7:2-7:18 "del" "log := (x :: (!log))" -> "()"
    1: 11:10-11:11 "int" "1" -> "0"
    2: 12:4-12:15 "del" "print_int i" -> "()"
    3: 25:2-25:62 "del" "print_string \"x\"" -> "()", dismissed "the output is not checked"
    4: 30:2-30:8 "del" "a || b" -> "()"
    5: 30:2-30:8 "and" "a || b" -> "a && b"
    6: 35:2-35:25 "del" "output_string oc \"done\"" -> "()"
  let log = ref []
  let push x =
    if Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 0
    then ()
    else (log := (x :: (!log)); ());
    List.length (!log)
  let loop n =
    for i =
      if Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 1
      then 0
      else 1 to n do
      if Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 2
      then ()
      else (print_int i; ());
      print_newline ()
    done
  let unit_first x = (); x
  let checked x = assert (x > 0); x
  let quiet x =
    ((print_string "x")[@mutate off "the output is not checked"]); x
  let either a b =
    if Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 4
    then ()
    else
      ((let __windtrap_mut_5_p = a in
        if
          Stdlib.(=) (__windtrap_mut_5_p : Stdlib.Bool.t)
            (Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 5)
        then b
        else __windtrap_mut_5_p);
       ());
    ()
  let finish oc ic fd dir =
    if Windtrap_mut_________fixture_del___ml.___windtrap_armed___ 6
    then ()
    else (output_string oc "done"; ());
    close_out oc;
    close_in_noerr ic;
    (try Unix.close fd with | Unix.Unix_error _ -> ());
    ignore (Unix.closedir dir);
    flush stdout

  $ mut --impl ./fixture_literals.ml | ../elide.exe
  mutation sites of "./fixture_literals.ml" in Windtrap_mut_________fixture_literals___ml, with type 'a operands:
    0: 4:30-4:31 "int" "1" -> "0"
    1: 4:23-4:24 "int" "7" -> "8"
    2: 4:16-4:17 "not" "b" -> "not b"
    3: 5:19-5:21 "int" "1l" -> "0l"
    4: 5:23-5:25 "int" "2L" -> "3L"
    5: 5:27-5:29 "int" "3n" -> "4n"
    6: 5:31-5:35 "int" "0x10" -> "17"
    7: 5:37-5:39 "int" "(-5)" -> "(-4)"
    8: 5:41-5:46 "int" "1_000" -> "1001"
    9: 9:15-9:19 "bool" "true" -> "false"
    10: 9:21-9:31 "or" "x && false" -> "x || false"
    11: 9:53-9:58 "bool" "false" -> "true"
    12: 9:36-9:40 "not" "true" -> "not true"
    13: 13:13-13:18 "sub" "x + 1" -> "x - 1"
    14: 14:13-14:18 "add" "1 - x" -> "1 + x"
    15: 16:35-16:36 "int" "3" -> "4"
    16: 16:28-16:29 "int" "2" -> "3"
    17: 16:17-16:22 "le" "x < 0" -> "x <= 0"
    18: 17:38-17:39 "int" "5" -> "6"
    19: 17:31-17:32 "int" "4" -> "5"
    20: 17:19-17:25 "lt" "x <= 1" -> "x < 1"
    21: 18:35-18:36 "int" "8" -> "9"
    22: 18:28-18:29 "int" "6" -> "7"
    23: 18:17-18:22 "ge" "x > 1" -> "x >= 1"
    24: 19:39-19:41 "int" "10" -> "11"
    25: 19:32-19:33 "int" "9" -> "10"
    26: 19:20-19:26 "gt" "x >= 1" -> "x > 1"
    27: 19:25-19:26 "int" "1" -> "0"
    28: 20:39-20:41 "int" "12" -> "13"
    29: 20:31-20:33 "int" "11" -> "12"
    30: 20:20-20:25 "le" "0 < x" -> "0 <= x"
    31: 20:20-20:21 "int" "0" -> "1"
    32: 22:18-22:20 "int" "65" -> "66"
    33: 25:13-25:15 "int" "66" -> "67"
    34: 30:20-30:22 "int" "67" -> "68"
  let pick b =
    if
      let __windtrap_mut_2_p = b in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 1
       then 8
       else 7)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 0
      then 0
      else 1
  let suffixed () =
    ((if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 3
      then 0l
      else 1l),
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 4
       then 3L
       else 2L),
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 5
       then 4n
       else 3n),
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 6
       then 17
       else 0x10),
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 7
       then (-4)
       else (-5)),
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 8
       then 1001
       else 1_000))
  let edges () = (max_int, 4611686018427387903, 9223372036854775807L, 4z)
  let flags x =
    ((if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 9
      then false
      else true),
      (let __windtrap_mut_10_p = x in
       if
         Stdlib.(<>) (__windtrap_mut_10_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 10)
       then false
       else __windtrap_mut_10_p),
      (if
         let __windtrap_mut_12_p = true in
         (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 12
          then Stdlib.not __windtrap_mut_12_p
          else __windtrap_mut_12_p)
       then x
       else
         if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 11
         then true
         else false))
  let succ x =
    let (__windtrap_mut_13_l, __windtrap_mut_13_r) = (x, 1) in
    if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 13
    then __windtrap_mut_13_l - __windtrap_mut_13_r
    else __windtrap_mut_13_l + __windtrap_mut_13_r
  let pred x =
    let (__windtrap_mut_14_l, __windtrap_mut_14_r) = (1, x) in
    if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 14
    then __windtrap_mut_14_l + __windtrap_mut_14_r
    else __windtrap_mut_14_l - __windtrap_mut_14_r
  let table () = Hashtbl.create 16
  let below x =
    if
      let (__windtrap_mut_17_l, __windtrap_mut_17_r) =
        ((x, 0) : _ Windtrap_mut_________fixture_literals___ml.operands) in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 17
       then Stdlib.not (__windtrap_mut_17_r < __windtrap_mut_17_l)
       else __windtrap_mut_17_l < __windtrap_mut_17_r)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 16
       then 3
       else 2)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 15
      then 4
      else 3
  let at_most x =
    if
      let (__windtrap_mut_20_l, __windtrap_mut_20_r) =
        ((x, 1) : _ Windtrap_mut_________fixture_literals___ml.operands) in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 20
       then Stdlib.not (__windtrap_mut_20_r <= __windtrap_mut_20_l)
       else __windtrap_mut_20_l <= __windtrap_mut_20_r)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 19
       then 5
       else 4)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 18
      then 6
      else 5
  let above x =
    if
      let (__windtrap_mut_23_l, __windtrap_mut_23_r) =
        ((x, 1) : _ Windtrap_mut_________fixture_literals___ml.operands) in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 23
       then Stdlib.not (__windtrap_mut_23_r > __windtrap_mut_23_l)
       else __windtrap_mut_23_l > __windtrap_mut_23_r)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 22
       then 7
       else 6)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 21
      then 9
      else 8
  let at_least x =
    if
      let (__windtrap_mut_26_l, __windtrap_mut_26_r) =
        ((x,
           (if
              Windtrap_mut_________fixture_literals___ml.___windtrap_armed___
                27
            then 0
            else 1)) : _ Windtrap_mut_________fixture_literals___ml.operands) in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 26
       then Stdlib.not (__windtrap_mut_26_r >= __windtrap_mut_26_l)
       else __windtrap_mut_26_l >= __windtrap_mut_26_r)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 25
       then 10
       else 9)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 24
      then 11
      else 10
  let mirrored x =
    if
      let (__windtrap_mut_30_l, __windtrap_mut_30_r) =
        (((if
             Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 31
           then 1
           else 0), x) : _ Windtrap_mut_________fixture_literals___ml.operands) in
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 30
       then Stdlib.not (__windtrap_mut_30_r < __windtrap_mut_30_l)
       else __windtrap_mut_30_l < __windtrap_mut_30_r)
    then
      (if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 29
       then 12
       else 11)
    else
      if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 28
      then 13
      else 12
  let limit = 64
  let later =
    lazy
      ((if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 32
        then 66
        else 65), ())
  module Make(X:sig  end) =
    struct
      let size =
        if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 33
        then 67
        else 66
    end
  class counter =
    object
      val mutable n =
        if Windtrap_mut_________fixture_literals___ml.___windtrap_armed___ 34
        then 68
        else 67
      method get = n
    end
  let pipe () = Unix.pipe ~cloexec:true ()

  $ mut --impl ./fixture_assert.ml | ../elide.exe
  mutation sites of "./fixture_assert.ml" in Windtrap_mut_________fixture_assert___ml, with type 'a operands:
    0: 7:2-7:7 "sub" "a + b" -> "a - b"
    1: 16:50-16:51 "int" "0" -> "1"
    2: 16:43-16:44 "int" "1" -> "0"
    3: 17:21-17:42 "or" "(a < b) && (assert false)" -> "(a < b) || (assert false)"
    4: 17:21-17:26 "le" "a < b" -> "a <= b"
    5: 28:7-28:8 "int" "0" -> "1"
    6: 27:7-27:8 "int" "1" -> "0"
    7: 25:4-26:9 "not" "assert (a < b); a > b" -> "not (assert (a < b); a > b)"
  let checked a b =
    assert (a < b);
    (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
     if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 0
     then __windtrap_mut_0_l - __windtrap_mut_0_r
     else __windtrap_mut_0_l + __windtrap_mut_0_r)
  let conjunction a b = assert (a && b)
  let arithmetic a b = assert ((a + b) > 0)
  let as_condition () =
    if assert false
    then
      (if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 2
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 1
      then 1
      else 0
  let as_operand a b =
    let __windtrap_mut_3_p =
      let (__windtrap_mut_4_l, __windtrap_mut_4_r) =
        ((a, b) : _ Windtrap_mut_________fixture_assert___ml.operands) in
      if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 4
      then Stdlib.not (__windtrap_mut_4_r < __windtrap_mut_4_l)
      else __windtrap_mut_4_l < __windtrap_mut_4_r in
    if
      Stdlib.(<>) (__windtrap_mut_3_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 3)
    then assert false
    else __windtrap_mut_3_p
  let sequenced a b =
    if
      let __windtrap_mut_7_p = assert (a < b); a > b in
      (if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 7
       then Stdlib.not __windtrap_mut_7_p
       else __windtrap_mut_7_p)
    then
      (if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 6
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_assert___ml.___windtrap_armed___ 5
      then 1
      else 0

  $ mut --impl ./fixture_chain.ml | ../elide.exe
  mutation sites of "./fixture_chain.ml" in Windtrap_mut_________fixture_chain___ml, with type 'a operands:
    0: 19:18-19:27 "sub" "(a + b) + c" -> "(a + b) - c"
    1: 20:18-20:27 "add" "(a - b) - c" -> "(a - b) + c"
    2: 21:19-21:30 "fsub" "(a +. b) +. c" -> "(a +. b) -. c"
    3: 22:44-22:45 "int" "0" -> "1"
    4: 22:37-22:38 "int" "1" -> "0"
    5: 22:25-22:31 "or" "b && c" -> "b || c"
    6: 31:26-31:37 "sub" "(a + b) + c" -> "(a + b) - c"
    7: 32:23-32:34 "sub" "(a + b) + c" -> "(a + b) - c"
    8: 33:31-33:37 "or" "b && c" -> "b || c"
    9: 39:23-39:38 "sub" "(a + b) + (c + d)" -> "(a + b) - (c + d)"
    10: 39:31-39:38 "sub" "c + d" -> "c - d"
    11: 43:18-43:27 "add" "(a + b) - c" -> "(a + b) + c"
    12: 43:18-43:23 "sub" "a + b" -> "a - b"
    13: 48:45-48:46 "int" "0" -> "1"
    14: 48:38-48:39 "int" "1" -> "0"
    15: 48:23-48:32 "le" "(a < b) < c" -> "(a < b) <= c"
  let three a b c =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = ((a + b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let minus a b c =
    let (__windtrap_mut_1_l, __windtrap_mut_1_r) = ((a - b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 1
    then __windtrap_mut_1_l + __windtrap_mut_1_r
    else __windtrap_mut_1_l - __windtrap_mut_1_r
  let floats a b c =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = ((a +. b), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l -. __windtrap_mut_2_r
    else __windtrap_mut_2_l +. __windtrap_mut_2_r
  let conn a b c =
    if
      a &&
        (let __windtrap_mut_5_p = b in
         (if
            Stdlib.(<>) (__windtrap_mut_5_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 5)
          then c
          else __windtrap_mut_5_p))
    then
      (if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 4
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 3
      then 1
      else 0
  let parens a b c =
    ignore
      (let (__windtrap_mut_6_l, __windtrap_mut_6_r) = ((a + b), c) in
       if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 6
       then __windtrap_mut_6_l - __windtrap_mut_6_r
       else __windtrap_mut_6_l + __windtrap_mut_6_r)
  let in_arg f a b c =
    f
      (let (__windtrap_mut_7_l, __windtrap_mut_7_r) = ((a + b), c) in
       if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 7
       then __windtrap_mut_7_l - __windtrap_mut_7_r
       else __windtrap_mut_7_l + __windtrap_mut_7_r)
  let conn_arg f a b c =
    f
      (a &&
         (let __windtrap_mut_8_p = b in
          if
            Stdlib.(<>) (__windtrap_mut_8_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 8)
          then c
          else __windtrap_mut_8_p))
  let siblings a b c d =
    let (__windtrap_mut_9_l, __windtrap_mut_9_r) =
      ((a + b),
        (let (__windtrap_mut_10_l, __windtrap_mut_10_r) = (c, d) in
         if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 10
         then __windtrap_mut_10_l - __windtrap_mut_10_r
         else __windtrap_mut_10_l + __windtrap_mut_10_r)) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 9
    then __windtrap_mut_9_l - __windtrap_mut_9_r
    else __windtrap_mut_9_l + __windtrap_mut_9_r
  let mixed a b c =
    let (__windtrap_mut_11_l, __windtrap_mut_11_r) =
      ((let (__windtrap_mut_12_l, __windtrap_mut_12_r) = (a, b) in
        if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 12
        then __windtrap_mut_12_l - __windtrap_mut_12_r
        else __windtrap_mut_12_l + __windtrap_mut_12_r), c) in
    if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 11
    then __windtrap_mut_11_l + __windtrap_mut_11_r
    else __windtrap_mut_11_l - __windtrap_mut_11_r
  let chained a b c =
    if
      let (__windtrap_mut_15_l, __windtrap_mut_15_r) =
        (((a < b), c) : _ Windtrap_mut_________fixture_chain___ml.operands) in
      (if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 15
       then Stdlib.not (__windtrap_mut_15_r < __windtrap_mut_15_l)
       else __windtrap_mut_15_l < __windtrap_mut_15_r)
    then
      (if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 14
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_chain___ml.___windtrap_armed___ 13
      then 1
      else 0

  $ mut --impl ./fixture_chain_off.ml | ../elide.exe
  mutation sites of "./fixture_chain_off.ml" in Windtrap_mut_________fixture_chain_off___ml:
    0: 4:19-4:50 "sub" "((a + (b - d))[@mutate off]) + c" -> "((a + (b - d))[@mutate off]) - c"
  let link a b c d =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) =
      (((a + (b - d))[@mutate off]), c) in
    if Windtrap_mut_________fixture_chain_off___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r

  $ mut --impl ./fixture_cmp.ml | ../elide.exe
  mutation sites of "./fixture_cmp.ml" in Windtrap_mut_________fixture_cmp___ml, with type 'a operands:
    0: 7:37-7:38 "int" "0" -> "1"
    1: 7:30-7:31 "int" "1" -> "0"
    2: 7:19-7:24 "le" "a < b" -> "a <= b"
    3: 8:40-8:41 "int" "0" -> "1"
    4: 8:33-8:34 "int" "1" -> "0"
    5: 8:21-8:27 "lt" "a <= b" -> "a < b"
    6: 11:8-11:13 "ge" "a > b" -> "a >= b"
    7: 15:49-15:50 "int" "1" -> "0"
    8: 15:39-15:45 "gt" "a >= b" -> "a > b"
    9: 15:58-15:59 "int" "0" -> "1"
    10: 16:36-16:37 "int" "0" -> "1"
    11: 16:29-16:30 "int" "1" -> "0"
    12: 16:18-16:23 "neq" "a = b" -> "a <> b"
    13: 17:40-17:41 "int" "0" -> "1"
    14: 17:33-17:34 "int" "1" -> "0"
    15: 17:21-17:27 "eq" "a <> b" -> "a = b"
    16: 20:21-20:39 "or" "(x >= lo) && (x <= hi)" -> "(x >= lo) || (x <= hi)"
    17: 20:21-20:28 "gt" "x >= lo" -> "x > lo"
    18: 20:32-20:39 "lt" "x <= hi" -> "x < hi"
  let below a b =
    if
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
       else __windtrap_mut_2_l < __windtrap_mut_2_r)
    then
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 1
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 0
      then 1
      else 0
  let at_most a b =
    if
      let (__windtrap_mut_5_l, __windtrap_mut_5_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 5
       then Stdlib.not (__windtrap_mut_5_r <= __windtrap_mut_5_l)
       else __windtrap_mut_5_l <= __windtrap_mut_5_r)
    then
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 4
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 3
      then 1
      else 0
  let above a b =
    while
      let (__windtrap_mut_6_l, __windtrap_mut_6_r) =
        ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 6
      then Stdlib.not (__windtrap_mut_6_r > __windtrap_mut_6_l)
      else __windtrap_mut_6_l > __windtrap_mut_6_r do () done
  let at_least a b =
    match a with
    | _ when
        let (__windtrap_mut_8_l, __windtrap_mut_8_r) =
          ((a, b) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
        if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 8
        then Stdlib.not (__windtrap_mut_8_r >= __windtrap_mut_8_l)
        else __windtrap_mut_8_l >= __windtrap_mut_8_r ->
        if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 7
        then 0
        else 1
    | _ ->
        if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 9
        then 1
        else 0
  let same a b =
    if
      let __windtrap_mut_12_p = a = b in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 12
       then Stdlib.not __windtrap_mut_12_p
       else __windtrap_mut_12_p)
    then
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 11
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 10
      then 1
      else 0
  let differs a b =
    if
      let __windtrap_mut_15_p = a <> b in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 15
       then Stdlib.not __windtrap_mut_15_p
       else __windtrap_mut_15_p)
    then
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 14
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 13
      then 1
      else 0
  let window lo hi x =
    let __windtrap_mut_16_p =
      let (__windtrap_mut_17_l, __windtrap_mut_17_r) =
        ((x, lo) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 17
      then Stdlib.not (__windtrap_mut_17_r >= __windtrap_mut_17_l)
      else __windtrap_mut_17_l >= __windtrap_mut_17_r in
    if
      Stdlib.(<>) (__windtrap_mut_16_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 16)
    then
      let (__windtrap_mut_18_l, __windtrap_mut_18_r) =
        ((x, hi) : _ Windtrap_mut_________fixture_cmp___ml.operands) in
      (if Windtrap_mut_________fixture_cmp___ml.___windtrap_armed___ 18
       then Stdlib.not (__windtrap_mut_18_r <= __windtrap_mut_18_l)
       else __windtrap_mut_18_l <= __windtrap_mut_18_r)
    else __windtrap_mut_16_p
  let ok a b = a < b
  let count a b = List.length (List.filter (fun x -> x < a) b)

  $ mut --impl ./fixture_con.ml | ../elide.exe
  mutation sites of "./fixture_con.ml" in Windtrap_mut_________fixture_con___ml:
    0: 8:15-8:21 "or" "a && b" -> "a || b"
    1: 9:17-9:23 "and" "a || b" -> "a && b"
    2: 10:45-10:46 "int" "0" -> "1"
    3: 10:38-10:39 "int" "1" -> "0"
    4: 10:22-10:32 "or" "ready && x" -> "ready || x"
    5: 11:34-11:39 "bool" "false" -> "true"
    6: 11:55-11:75 "and" "(p x) || (search p rest)" -> "(p x) && (search p rest)"
  let both a b =
    let __windtrap_mut_0_p = a in
    if
      Stdlib.(<>) (__windtrap_mut_0_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 0)
    then b
    else __windtrap_mut_0_p
  let either a b =
    let __windtrap_mut_1_p = a in
    if
      Stdlib.(=) (__windtrap_mut_1_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 1)
    then b
    else __windtrap_mut_1_p
  let gate ready x =
    if
      let __windtrap_mut_4_p = ready in
      (if
         Stdlib.(<>) (__windtrap_mut_4_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 4)
       then x
       else __windtrap_mut_4_p)
    then
      (if Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 3
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 2
      then 1
      else 0
  let rec search p =
    function
    | [] ->
        if Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 5
        then true
        else false
    | x::rest ->
        let __windtrap_mut_6_p = p x in
        if
          Stdlib.(=) (__windtrap_mut_6_p : Stdlib.Bool.t)
            (Windtrap_mut_________fixture_con___ml.___windtrap_armed___ 6)
        then search p rest
        else __windtrap_mut_6_p

  $ mut --impl ./fixture_contexts.ml | ../elide.exe
  mutation sites of "./fixture_contexts.ml" in Windtrap_mut_________fixture_contexts___ml, with type 'a operands:
    0: 5:19-5:29 "and" "(a < b) || c" -> "(a < b) && c"
    1: 5:19-5:24 "le" "a < b" -> "a <= b"
    2: 15:7-15:8 "int" "0" -> "1"
    3: 14:7-14:8 "int" "1" -> "0"
    4: 12:4-13:9 "not" "let x = a in x < b" -> "not (let x = a in x < b)"
    5: 17:59-17:60 "int" "0" -> "1"
    6: 17:52-17:53 "int" "1" -> "0"
    7: 17:32-17:46 "not" "(a < b : bool)" -> "not (a < b : bool)"
    8: 18:49-18:50 "int" "0" -> "1"
    9: 18:42-18:43 "int" "1" -> "0"
    10: 18:25-18:36 "not" "not (a < b)" -> "not (not (a < b))"
    11: 23:55-23:56 "int" "0" -> "1"
    12: 23:48-23:49 "int" "1" -> "0"
    13: 23:34-23:42 "and" "c || d" -> "c && d"
    14: 23:25-23:30 "le" "a < b" -> "a <= b"
    15: 29:45-29:46 "int" "0" -> "1"
    16: 29:38-29:39 "int" "1" -> "0"
    17: 29:27-29:32 "not" "a & b" -> "not (a & b)"
    18: 34:61-34:62 "int" "0" -> "1"
    19: 34:54-34:55 "int" "1" -> "0"
    20: 34:33-34:48 "not" "Float.(<) a b" -> "not (Float.(<) a b)"
    21: 40:16-40:21 "sub" "a + b" -> "a - b"
  let either a b c =
    let __windtrap_mut_0_p =
      let (__windtrap_mut_1_l, __windtrap_mut_1_r) =
        ((a, b) : _ Windtrap_mut_________fixture_contexts___ml.operands) in
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 1
      then Stdlib.not (__windtrap_mut_1_r < __windtrap_mut_1_l)
      else __windtrap_mut_1_l < __windtrap_mut_1_r in
    if
      Stdlib.(=) (__windtrap_mut_0_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 0)
    then c
    else __windtrap_mut_0_p
  let through_let a b =
    if
      let __windtrap_mut_4_p = let x = a in x < b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 4
       then Stdlib.not __windtrap_mut_4_p
       else __windtrap_mut_4_p)
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 3
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 2
      then 1
      else 0
  let through_constraint a b =
    if
      let __windtrap_mut_7_p = (a < b : bool) in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 7
       then Stdlib.not __windtrap_mut_7_p
       else __windtrap_mut_7_p)
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 6
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 5
      then 1
      else 0
  let through_not a b =
    if
      let __windtrap_mut_10_p = not (a < b) in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 10
       then Stdlib.not __windtrap_mut_10_p
       else __windtrap_mut_10_p)
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 9
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 8
      then 1
      else 0
  let skipped a b c d =
    if
      (let (__windtrap_mut_14_l, __windtrap_mut_14_r) =
         ((a, b) : _ Windtrap_mut_________fixture_contexts___ml.operands) in
       if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 14
       then Stdlib.not (__windtrap_mut_14_r < __windtrap_mut_14_l)
       else __windtrap_mut_14_l < __windtrap_mut_14_r) &&
        (let __windtrap_mut_13_p = c in
         (if
            Stdlib.(=) (__windtrap_mut_13_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___
                 13)
          then d
          else __windtrap_mut_13_p))
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 12
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 11
      then 1
      else 0
  let old_and a b = a & b
  let old_or a b = a or b
  let old_condition a b =
    if
      let __windtrap_mut_17_p = a & b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 17
       then Stdlib.not __windtrap_mut_17_p
       else __windtrap_mut_17_p)
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 16
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 15
      then 1
      else 0
  let qualified a b = Stdlib.(+) a b
  let qualified_condition a b =
    if
      let __windtrap_mut_20_p = Float.(<) a b in
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 20
       then Stdlib.not __windtrap_mut_20_p
       else __windtrap_mut_20_p)
    then
      (if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 19
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 18
      then 1
      else 0
  let partial = (+) 1
  let labelled a b = (+) ~a b
  let total a b =
    let (__windtrap_mut_21_l, __windtrap_mut_21_r) = (a, b) in
    if Windtrap_mut_________fixture_contexts___ml.___windtrap_armed___ 21
    then __windtrap_mut_21_l - __windtrap_mut_21_r
    else __windtrap_mut_21_l + __windtrap_mut_21_r

  $ mut --impl ./fixture_empty.ml | ../elide.exe
  type t =
    | A 
    | B 
  let x = 1
  let s = "hello"
  let pair a b = (a, b)

  $ mut --impl ./fixture_exclude.ml | ../elide.exe
  [@@@mutate exclude_file]
  let sum a b = a + b
  let ordered a b = if a < b then 1 else 0

  $ mut --impl ./fixture_inline_tests.ml | ../elide.exe
  mutation sites of "./fixture_inline_tests.ml" in Windtrap_mut_________fixture_inline_tests___ml, with type 'a operands:
    0: 6:14-6:19 "sub" "a + b" -> "a - b"
    1: 7:39-7:40 "int" "0" -> "1"
    2: 7:32-7:33 "int" "1" -> "0"
    3: 7:21-7:26 "le" "a < b" -> "a <= b"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let ordered a b =
    if
      let (__windtrap_mut_3_l, __windtrap_mut_3_r) =
        ((a, b) : _ Windtrap_mut_________fixture_inline_tests___ml.operands) in
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 3
       then Stdlib.not (__windtrap_mut_3_r < __windtrap_mut_3_l)
       else __windtrap_mut_3_l < __windtrap_mut_3_r)
    then
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 2
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 1
      then 1
      else 0
  [%%test let "sums" = if (sum 1 2) = 3 then () else failwith "sum"]
  [%%test
    module Grouped =
      struct
        let twice x = x + x
        [%%test
          let "orders" =
            if (ordered 1 (twice 1)) > 0 then () else failwith "order"]
      end]
  [%%expect_test let "prints" = print_int ((sum 1 2) - 1); [%expect {| 2 |}]]

  $ ../pp.exe -apply ppx_windtrap,windtrap_mutate --impl ./fixture_inline_tests.ml | ../elide.exe
  mutation sites of "./fixture_inline_tests.ml" in Windtrap_mut_________fixture_inline_tests___ml, with type 'a operands:
    0: 6:14-6:19 "sub" "a + b" -> "a - b"
    1: 7:39-7:40 "int" "0" -> "1"
    2: 7:32-7:33 "int" "1" -> "0"
    3: 7:21-7:26 "le" "a < b" -> "a <= b"
  let sum a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  let ordered a b =
    if
      let (__windtrap_mut_3_l, __windtrap_mut_3_r) =
        ((a, b) : _ Windtrap_mut_________fixture_inline_tests___ml.operands) in
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 3
       then Stdlib.not (__windtrap_mut_3_r < __windtrap_mut_3_l)
       else __windtrap_mut_3_l < __windtrap_mut_3_r)
    then
      (if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 2
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_inline_tests___ml.___windtrap_armed___ 1
      then 1
      else 0
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./fixture_inline_tests.ml"
      ~pos:("./fixture_inline_tests.ml", 8, 0, 60) ~tags:[] "sums"
      (fun () ->
         match (if (sum 1 2) = 3 then () else failwith "sum" : unit) with
         | () -> ()
         | exception __windtrap_e -> Stdlib.raise_notrace __windtrap_e)
    [@@windtrap.test ]
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.enter_group
      ~file:"./fixture_inline_tests.ml" ~tags:[] "Grouped"[@@windtrap.test ]
  module Grouped =
    struct
      let twice x = x + x
      let () =
        Ppx_windtrap_runtime.Ppx_runtime.add_test
          ~file:"./fixture_inline_tests.ml"
          ~pos:("./fixture_inline_tests.ml", 12, 2, 78) ~tags:[] "orders"
          (fun () ->
             match (if (ordered 1 (twice 1)) > 0 then () else failwith "order" : 
               unit)
             with
             | () -> ()
             | exception __windtrap_e -> Stdlib.raise_notrace __windtrap_e)
        [@@windtrap.test ]
    end[@@windtrap.test ]
  let () = Ppx_windtrap_runtime.Ppx_runtime.leave_group ()[@@windtrap.test ]
  let () =
    Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"./fixture_inline_tests.ml"
      ~pos:("./fixture_inline_tests.ml", 15, 0, 73) ~tags:[] "prints"
      (fun () ->
         Ppx_windtrap_runtime.Ppx_runtime.expect_test
           ~pos:("./fixture_inline_tests.ml", 15, 0, 73)
           ~body_end:("./fixture_inline_tests.ml", 17, 19, 19)
           ~nodes:[("./fixture_inline_tests.ml", 17, 2, 19)]
           (fun () ->
              (Expect_test_config.run : (unit -> unit) -> unit)
                (fun () ->
                   match (print_int ((sum 1 2) - 1);
                          Ppx_windtrap_runtime.Ppx_runtime.reach
                            ("./fixture_inline_tests.ml", 17, 2, 19);
                          Windtrap.expect
                            (Expect_test_config.sanitize (Windtrap.output ()))
                            (("./fixture_inline_tests.ml", 17, 2, 19), {| 2 |}) : 
                     unit)
                   with
                   | () -> ()
                   | exception __windtrap_e ->
                       Stdlib.raise_notrace __windtrap_e))
           (fun () -> Expect_test_config.sanitize (Windtrap.output ())))
    [@@windtrap.test ]

  $ mut --impl ./fixture_lazy.ml | ../elide.exe
  mutation sites of "./fixture_lazy.ml" in Windtrap_mut_________fixture_lazy___ml:
    0: 10:22-10:29 "sub" "a + b" -> "a - b"
    1: 17:25-17:30 "sub" "a + b" -> "a - b"
  let thunk a b = lazy (fun () -> a + b)
  let forced a b =
    lazy
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
       if Windtrap_mut_________fixture_lazy___ml.___windtrap_armed___ 0
       then __windtrap_mut_0_l - __windtrap_mut_0_r
       else __windtrap_mut_0_l + __windtrap_mut_0_r)
  let plain x = lazy x
  let annotated a b = lazy (fun () -> a + b : unit -> int)
  let computed a b =
    lazy
      (let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
       if Windtrap_mut_________fixture_lazy___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l - __windtrap_mut_1_r
       else __windtrap_mut_1_l + __windtrap_mut_1_r : int)

  $ mut --impl ./fixture_lazy_condition.ml | ../elide.exe
  mutation sites of "./fixture_lazy_condition.ml" in Windtrap_mut_________fixture_lazy_condition___ml:
    0: 3:39-3:40 "int" "0" -> "1"
    1: 3:32-3:33 "int" "1" -> "0"
    2: 4:31-4:32 "int" "0" -> "1"
    3: 4:24-4:25 "int" "1" -> "0"
    4: 4:17-4:18 "not" "x" -> "not x"
  let deferred x =
    if lazy x
    then
      (if
         Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___
           1
       then 0
       else 1)
    else
      if
        Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___ 0
      then 1
      else 0
  let plain x =
    if
      let __windtrap_mut_4_p = x in
      (if
         Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___
           4
       then Stdlib.not __windtrap_mut_4_p
       else __windtrap_mut_4_p)
    then
      (if
         Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___
           3
       then 0
       else 1)
    else
      if
        Windtrap_mut_________fixture_lazy_condition___ml.___windtrap_armed___ 2
      then 1
      else 0

  $ mut --impl ./fixture_lost_cmp.ml | ../elide.exe
  mutation sites of "./fixture_lost_cmp.ml" in Windtrap_mut_________fixture_lost_cmp___ml:
    0: 8:32-8:33 "int" "0" -> "1"
    1: 10:43-10:48 "add" "a - b" -> "a + b"
    2: 10:32-10:37 "sub" "a + b" -> "a - b"
    3: 10:21-10:26 "not" "a < b" -> "not (a < b)"
    4: 11:37-11:38 "int" "0" -> "1"
    5: 11:30-11:31 "int" "1" -> "0"
    6: 11:18-11:24 "or" "a && b" -> "a || b"
  let (<=) a b =
    (compare a b) <=
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 0
       then 1
       else 0)
  let not b = b
  let ordered a b =
    if
      let __windtrap_mut_3_p = a < b in
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 3
       then Stdlib.not __windtrap_mut_3_p
       else __windtrap_mut_3_p)
    then
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 2
       then __windtrap_mut_2_l - __windtrap_mut_2_r
       else __windtrap_mut_2_l + __windtrap_mut_2_r)
    else
      (let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
       if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l + __windtrap_mut_1_r
       else __windtrap_mut_1_l - __windtrap_mut_1_r)
  let both a b =
    if
      let __windtrap_mut_6_p = a in
      (if
         Stdlib.(<>) (__windtrap_mut_6_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 6)
       then b
       else __windtrap_mut_6_p)
    then
      (if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 5
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_lost_cmp___ml.___windtrap_armed___ 4
      then 1
      else 0

  $ mut --impl ./fixture_lost_con.ml | ../elide.exe
  mutation sites of "./fixture_lost_con.ml" in Windtrap_mut_________fixture_lost_con___ml, with type 'a operands:
    0: 11:37-11:38 "int" "0" -> "1"
    1: 11:30-11:31 "int" "1" -> "0"
    2: 11:18-11:24 "not" "a && b" -> "not (a && b)"
    3: 12:39-12:40 "int" "0" -> "1"
    4: 12:32-12:33 "int" "1" -> "0"
    5: 12:20-12:26 "not" "a || b" -> "not (a || b)"
    6: 13:47-13:48 "int" "0" -> "1"
    7: 13:40-13:41 "int" "1" -> "0"
    8: 13:24-13:34 "not" "(a < b) && c" -> "not ((a < b) && c)"
    9: 14:43-14:48 "add" "a - b" -> "a + b"
    10: 14:32-14:37 "sub" "a + b" -> "a - b"
    11: 14:21-14:26 "le" "a < b" -> "a <= b"
  external (&&) : bool -> bool -> bool = "%sequand"
  let both a b =
    if
      let __windtrap_mut_2_p = a && b in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 1
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 0
      then 1
      else 0
  let either a b =
    if
      let __windtrap_mut_5_p = a || b in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 5
       then Stdlib.not __windtrap_mut_5_p
       else __windtrap_mut_5_p)
    then
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 4
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 3
      then 1
      else 0
  let compared a b c =
    if
      let __windtrap_mut_8_p = (a < b) && c in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 8
       then Stdlib.not __windtrap_mut_8_p
       else __windtrap_mut_8_p)
    then
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 7
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 6
      then 1
      else 0
  let ordered a b =
    if
      let (__windtrap_mut_11_l, __windtrap_mut_11_r) =
        ((a, b) : _ Windtrap_mut_________fixture_lost_con___ml.operands) in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 11
       then Stdlib.not (__windtrap_mut_11_r < __windtrap_mut_11_l)
       else __windtrap_mut_11_l < __windtrap_mut_11_r)
    then
      let (__windtrap_mut_10_l, __windtrap_mut_10_r) = (a, b) in
      (if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 10
       then __windtrap_mut_10_l - __windtrap_mut_10_r
       else __windtrap_mut_10_l + __windtrap_mut_10_r)
    else
      (let (__windtrap_mut_9_l, __windtrap_mut_9_r) = (a, b) in
       if Windtrap_mut_________fixture_lost_con___ml.___windtrap_armed___ 9
       then __windtrap_mut_9_l + __windtrap_mut_9_r
       else __windtrap_mut_9_l - __windtrap_mut_9_r)

  $ mut --impl ./fixture_neg.ml | ../elide.exe
  mutation sites of "./fixture_neg.ml" in Windtrap_mut_________fixture_neg___ml:
    0: 8:23-8:27 "not" "flag" -> "not flag"
    1: 9:23-9:27 "not" "flag" -> "not flag"
    2: 12:8-12:16 "not" "ready ()" -> "not (ready ())"
    3: 16:39-16:42 "not" "p y" -> "not (p y)"
    4: 20:55-20:56 "int" "0" -> "1"
    5: 20:48-20:49 "int" "1" -> "0"
    6: 20:20-20:42 "not" "if a then b else false" -> "not (if a then b else false)"
    7: 20:37-20:42 "bool" "false" -> "true"
    8: 20:23-20:24 "not" "a" -> "not a"
  let pick flag x y =
    if
      let __windtrap_mut_0_p = flag in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 0
       then Stdlib.not __windtrap_mut_0_p
       else __windtrap_mut_0_p)
    then x
    else y
  let announce flag =
    if
      let __windtrap_mut_1_p = flag in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 1
       then Stdlib.not __windtrap_mut_1_p
       else __windtrap_mut_1_p)
    then print_string "on"
  let drain ready step =
    while
      let __windtrap_mut_2_p = ready () in
      if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 2
      then Stdlib.not __windtrap_mut_2_p
      else __windtrap_mut_2_p do step () done
  let classify p x =
    match x with
    | y when
        let __windtrap_mut_3_p = p y in
        if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 3
        then Stdlib.not __windtrap_mut_3_p
        else __windtrap_mut_3_p -> "yes"
    | _ -> "no"
  let nested a b =
    if
      let __windtrap_mut_6_p =
        if
          let __windtrap_mut_8_p = a in
          (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 8
           then Stdlib.not __windtrap_mut_8_p
           else __windtrap_mut_8_p)
        then b
        else
          if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 7
          then true
          else false in
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 6
       then Stdlib.not __windtrap_mut_6_p
       else __windtrap_mut_6_p)
    then
      (if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 5
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_neg___ml.___windtrap_armed___ 4
      then 1
      else 0

  $ mut --impl ./fixture_nesting.ml | ../elide.exe
  mutation sites of "./fixture_nesting.ml" in Windtrap_mut_________fixture_nesting___ml, with type 'a operands:
    0: 6:23-6:29 "or" "b && c" -> "b || c"
    1: 7:18-7:26 "or" "a && b" -> "a || b"
    2: 8:30-8:38 "and" "c || d" -> "c && d"
    3: 11:38-11:39 "int" "0" -> "1"
    4: 11:31-11:32 "int" "1" -> "0"
    5: 11:20-11:25 "le" "a < b" -> "a <= b"
    6: 12:39-12:40 "int" "0" -> "1"
    7: 12:32-12:33 "int" "1" -> "0"
    8: 12:20-12:26 "or" "a && b" -> "a || b"
    9: 13:32-13:33 "int" "0" -> "1"
    10: 13:25-13:26 "int" "1" -> "0"
    11: 13:18-13:19 "not" "a" -> "not a"
  let chain a b c =
    a &&
      (let __windtrap_mut_0_p = b in
       if
         Stdlib.(<>) (__windtrap_mut_0_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 0)
       then c
       else __windtrap_mut_0_p)
  let mixed a b c =
    (let __windtrap_mut_1_p = a in
     if
       Stdlib.(<>) (__windtrap_mut_1_p : Stdlib.Bool.t)
         (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 1)
     then b
     else __windtrap_mut_1_p) || c
  let deep a b c d =
    a ||
      (b &&
         (let __windtrap_mut_2_p = c in
          if
            Stdlib.(=) (__windtrap_mut_2_p : Stdlib.Bool.t)
              (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 2)
          then d
          else __windtrap_mut_2_p))
  let by_cmp a b =
    if
      let (__windtrap_mut_5_l, __windtrap_mut_5_r) =
        ((a, b) : _ Windtrap_mut_________fixture_nesting___ml.operands) in
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 5
       then Stdlib.not (__windtrap_mut_5_r < __windtrap_mut_5_l)
       else __windtrap_mut_5_l < __windtrap_mut_5_r)
    then
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 4
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 3
      then 1
      else 0
  let by_con a b =
    if
      let __windtrap_mut_8_p = a in
      (if
         Stdlib.(<>) (__windtrap_mut_8_p : Stdlib.Bool.t)
           (Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 8)
       then b
       else __windtrap_mut_8_p)
    then
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 7
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 6
      then 1
      else 0
  let by_neg a =
    if
      let __windtrap_mut_11_p = a in
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 11
       then Stdlib.not __windtrap_mut_11_p
       else __windtrap_mut_11_p)
    then
      (if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 10
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_nesting___ml.___windtrap_armed___ 9
      then 1
      else 0

  $ mut --impl ./fixture_off.ml | ../elide.exe
  mutation sites of "./fixture_off.ml" in Windtrap_mut_________fixture_off___ml:
    0: 10:7-10:9 "int" "16" -> "17"
    1: 9:5-9:16 "ge" "want > 16" -> "want >= 16", dismissed "both arms yield 16 at the boundary"
    2: 12:54-12:55 "int" "0" -> "1"
    3: 12:47-12:48 "int" "1" -> "0"
    4: 12:19-12:27 "or" "a && b" -> "a || b", dismissed ""
    5: 26:18-26:23 "sub" "a + b" -> "a - b"
  let cap want =
    if ((want > 16)[@mutate off "both arms yield 16 at the boundary"])
    then want
    else
      if Windtrap_mut_________fixture_off___ml.___windtrap_armed___ 0
      then 17
      else 16
  let plain a b =
    if ((a && b)[@mutate off])
    then
      (if Windtrap_mut_________fixture_off___ml.___windtrap_armed___ 3
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_off___ml.___windtrap_armed___ 2
      then 1
      else 0
  let sum a b = a + b[@@mutate off]
  module Hidden = struct let diff a b = a - b end[@@mutate off]
  [@@@mutate off]
  let suppressed a b = a + b
  [@@@mutate on]
  let visible a b =
    let (__windtrap_mut_5_l, __windtrap_mut_5_r) = (a, b) in
    if Windtrap_mut_________fixture_off___ml.___windtrap_armed___ 5
    then __windtrap_mut_5_l - __windtrap_mut_5_r
    else __windtrap_mut_5_l + __windtrap_mut_5_r

  $ mut --impl ./fixture_off_edges.ml | ../elide.exe
  mutation sites of "./fixture_off_edges.ml" in Windtrap_mut_________fixture_off_edges___ml:
    0: 19:7-19:9 "int" "16" -> "17"
    1: 15:4-15:15 "ge" "want > 16" -> "want >= 16", dismissed "a \"quoted\" reason,\nspanning lines \\ containing a backslash"
    2: 30:10-30:15 "sub" "a + b" -> "a - b"
    3: 39:16-39:21 "sub" "a + b" -> "a - b"
  let quoted want =
    if ((want > 16)
      [@mutate
        off "a \"quoted\" reason,\nspanning lines \\ containing a backslash"])
    then want
    else
      if Windtrap_mut_________fixture_off_edges___ml.___windtrap_armed___ 0
      then 17
      else 16
  let coarse a b = ((match a with | 0 -> b | n -> n + b)
    [@mutate off "not a site"])
  let local a b =
    let s =
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
      if Windtrap_mut_________fixture_off_edges___ml.___windtrap_armed___ 2
      then __windtrap_mut_2_l - __windtrap_mut_2_r
      else __windtrap_mut_2_l + __windtrap_mut_2_r[@@mutate off "ignored here"] in
    s
  type t = int[@@mutate off]
  let after a b =
    let (__windtrap_mut_3_l, __windtrap_mut_3_r) = (a, b) in
    if Windtrap_mut_________fixture_off_edges___ml.___windtrap_armed___ 3
    then __windtrap_mut_3_l - __windtrap_mut_3_r
    else __windtrap_mut_3_l + __windtrap_mut_3_r

  $ mut --impl ./fixture_off_structure.ml | ../elide.exe
  mutation sites of "./fixture_off_structure.ml" in Windtrap_mut_________fixture_off_structure___ml:
    0: 16:12-16:17 "add" "x - 1" -> "x + 1"
    1: 41:23-41:28 "sub" "a + b" -> "a - b"
    2: 48:14-48:19 "sub" "a + b" -> "a - b"
  module rec Dark:sig val f : int -> int end = struct let f n = n + 1 end
  [@@mutate off]
  
  let local n =
    let h x =
      let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (x, 1) in
      if Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___ 0
      then __windtrap_mut_0_l + __windtrap_mut_0_r
      else __windtrap_mut_0_l - __windtrap_mut_0_r[@@mutate bogus] in
    h n
  type t = int[@@mutate bogus]
  let reasoned a b = a + b[@@mutate off "binding reason"]
  [@@@mutate off "region reason"]
  let in_region a b = a - b
  [@@@mutate on]
  [@@@mutate off]
  module Inherits =
    struct
      let dark a b = a + b
      [@@@mutate on]
      let lit_inside a b =
        let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
        if
          Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___
            1
        then __windtrap_mut_1_l - __windtrap_mut_1_r
        else __windtrap_mut_1_l + __windtrap_mut_1_r
    end
  let still_dark a b = a + b
  [@@@mutate on]
  let lit a b =
    let (__windtrap_mut_2_l, __windtrap_mut_2_r) = (a, b) in
    if Windtrap_mut_________fixture_off_structure___ml.___windtrap_armed___ 2
    then __windtrap_mut_2_l - __windtrap_mut_2_r
    else __windtrap_mut_2_l + __windtrap_mut_2_r

  $ mut --impl ./fixture_off_unclosed.ml | ../elide.exe
  mutation sites of "./fixture_off_unclosed.ml" in Windtrap_mut_________fixture_off_unclosed___ml:
    0: 17:18-17:23 "sub" "a + b" -> "a - b"
  module Quiet = struct [@@@mutate off]
                        let hidden a b = a + b end
  let visible a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_off_unclosed___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r
  [@@@mutate off]
  let suppressed a b = a + b
  let also_suppressed a b = if a && b then 1 else 0

  $ mut --impl ./fixture_payloads.ml | ../elide.exe
  mutation sites of "./fixture_payloads.ml" in Windtrap_mut_________fixture_payloads___ml:
    0: 6:18-6:23 "sub" "a + b" -> "a - b"
  let extended = [%ext a + b]
  let attributed = ((0)[@attr a + b])
  let written a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_payloads___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l - __windtrap_mut_0_r
    else __windtrap_mut_0_l + __windtrap_mut_0_r

  $ mut --impl ./fixture_shadow.ml | ../elide.exe
  mutation sites of "./fixture_shadow.ml" in Windtrap_mut_________fixture_shadow___ml, with type 'a operands:
    0: 16:39-16:40 "int" "0" -> "1"
    1: 16:32-16:33 "int" "1" -> "0"
    2: 16:21-16:26 "le" "a < b" -> "a <= b"
    3: 17:15-17:21 "or" "a && b" -> "a || b"
  module Vec =
    struct
      type t = {
        x: int ;
        y: int }
      let (+) a b = { x = (a.x + b.x); y = (a.y + b.y) }
    end
  let move a b = Vec.(+) a b
  let ordered a b =
    if
      let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
        ((a, b) : _ Windtrap_mut_________fixture_shadow___ml.operands) in
      (if Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
       else __windtrap_mut_2_l < __windtrap_mut_2_r)
    then
      (if Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 1
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 0
      then 1
      else 0
  let both a b =
    let __windtrap_mut_3_p = a in
    if
      Stdlib.(<>) (__windtrap_mut_3_p : Stdlib.Bool.t)
        (Windtrap_mut_________fixture_shadow___ml.___windtrap_armed___ 3)
    then b
    else __windtrap_mut_3_p
  let untouched a b = a - b

  $ mut --impl ./fixture_texts.ml | ../elide.exe
  mutation sites of "./fixture_texts.ml" in Windtrap_mut_________fixture_texts___ml, with type 'a operands:
    0: 6:42-6:43 "int" "0" -> "1"
    1: 6:35-6:36 "int" "1" -> "0"
    2: 6:18-6:29 "neq" "s = \"a b\"" -> "s <> \"a b\""
    3: 10:15-10:22 "sub" "a + b" -> "a - b"
    4: 11:50-11:51 "int" "0" -> "1"
    5: 11:43-11:44 "int" "1" -> "0"
    6: 11:22-11:29 "le" "a < b" -> "a <= b"
  let spaced s =
    if
      let __windtrap_mut_2_p = s = "a   b" in
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 2
       then Stdlib.not __windtrap_mut_2_p
       else __windtrap_mut_2_p)
    then
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 1
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 0
      then 1
      else 0
  let kept a b =
    let (__windtrap_mut_3_l, __windtrap_mut_3_r) = (a, b) in
    if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 3
    then __windtrap_mut_3_l - __windtrap_mut_3_r
    else ((__windtrap_mut_3_l + __windtrap_mut_3_r)[@kept ])
  let kept_cmp a b =
    if
      let (__windtrap_mut_6_l, __windtrap_mut_6_r) =
        ((a, b) : _ Windtrap_mut_________fixture_texts___ml.operands) in
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 6
       then Stdlib.not (__windtrap_mut_6_r < __windtrap_mut_6_l)
       else ((__windtrap_mut_6_l < __windtrap_mut_6_r)[@kept ]))
    then
      (if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 5
       then 0
       else 1)
    else
      if Windtrap_mut_________fixture_texts___ml.___windtrap_armed___ 4
      then 1
      else 0

Over code a deriver generated, for which generated.ml stands in:

  $ ../pp.exe -apply windtrap_test_generated,windtrap_mutate --impl ./fixture_generated.ml | ../elide.exe
  mutation sites of "./fixture_generated.ml" in Windtrap_mut_________fixture_generated___ml:
    0: 6:18-6:23 "add" "a - b" -> "a + b"
    1: 11:21-11:28 "sub" "a + b" -> "a - b"
  let generated a b = ((a + b)[@generated ])
  let written a b =
    let (__windtrap_mut_0_l, __windtrap_mut_0_r) = (a, b) in
    if Windtrap_mut_________fixture_generated___ml.___windtrap_armed___ 0
    then __windtrap_mut_0_l + __windtrap_mut_0_r
    else __windtrap_mut_0_l - __windtrap_mut_0_r
  let copied a b =
    a *
      (let (__windtrap_mut_1_l, __windtrap_mut_1_r) = (a, b) in
       if Windtrap_mut_________fixture_generated___ml.___windtrap_armed___ 1
       then __windtrap_mut_1_l - __windtrap_mut_1_r
       else __windtrap_mut_1_l + __windtrap_mut_1_r)[@@duplicate ]
  let copied a b = a * (a + b)[@@duplicate ]

  $ ../pp.exe -apply windtrap_test_generated,windtrap_mutate --impl ./fixture_generated_chain.ml | ../elide.exe
  mutation sites of "./fixture_generated_chain.ml" in Windtrap_mut_________fixture_generated_chain___ml:
    0: 6:23-6:34 "sub" "(a + b) + c" -> "(a + b) - c"
  let generated a b c = (((a + b) + c)[@generated ])
  let copied f a b c =
    f
      (let (__windtrap_mut_0_l, __windtrap_mut_0_r) = ((a + b), c) in
       if
         Windtrap_mut_________fixture_generated_chain___ml.___windtrap_armed___
           0
       then __windtrap_mut_0_l - __windtrap_mut_0_r
       else __windtrap_mut_0_l + __windtrap_mut_0_r)[@@duplicate ]
  let copied f a b c = f ((a + b) + c)[@@duplicate ]

Over a file the coverage rewriter ran on first. pp.exe runs the mutation
rewriter first whatever order -apply names, so this order has its own
driver, coverage_first.exe:

  $ ../coverage_first.exe -apply windtrap_coverage,windtrap_mutate --impl ./fixture_visits.ml | ../elide.exe
  mutation sites of "./fixture_visits.ml" in Windtrap_mut_________fixture_visits___ml, with type 'a operands:
    0: 7:22-7:23 "not" "b" -> "not b"
    1: 7:17-7:18 "not" "a" -> "not a"
    2: 10:18-10:27 "le" "(___windtrap_post_visit___ 2 (f x)) < (___windtrap_post_visit___ 3 (g x))" -> "(___windtrap_post_visit___ 2 (f x)) <= (___windtrap_post_visit___ 3 (g x))"
  coverage points of "./fixture_visits.ml" in Windtrap_cov_________fixture_visits___ml, with ___windtrap_post_visit___:
    0: 262-263
    1: 267-268
    2: 362-365
    3: 368-371
    4: 384-385
    5: 377-378
    6: 359-385
  let either a b =
    ___windtrap_visit___ 0;
    if
      (let __windtrap_mut_1_p = a in
       if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 1
       then Stdlib.not __windtrap_mut_1_p
       else __windtrap_mut_1_p)
    then (___windtrap_visit___ 0; true)
    else
      if
        (let __windtrap_mut_0_p = b in
         if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 0
         then Stdlib.not __windtrap_mut_0_p
         else __windtrap_mut_0_p)
      then (___windtrap_visit___ 1; true)
      else false
  let lt f g x =
    ___windtrap_visit___ 6;
    if
      (let (__windtrap_mut_2_l, __windtrap_mut_2_r) =
         (((___windtrap_post_visit___ 2 (f x)),
            (___windtrap_post_visit___ 3 (g x))) : _
                                                     Windtrap_mut_________fixture_visits___ml.operands) in
       if Windtrap_mut_________fixture_visits___ml.___windtrap_armed___ 2
       then Stdlib.not (__windtrap_mut_2_r < __windtrap_mut_2_l)
       else __windtrap_mut_2_l < __windtrap_mut_2_r)
    then (___windtrap_visit___ 5; 1)
    else (___windtrap_visit___ 4; 0)

A file under an input name that is not a source file's is returned as
parsed:

  $ for name in //toplevel// '(stdin)' lib/.ocamlinit lib/topfind; do
  >   mut -loc-filename "$name" --impl ./fixture_input_name.ml
  > done
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0
  let sign n = if n > 0 then 1 else 0

A refusal is an error located at the attribute, and the driver exits 1:

  $ mut --impl ./reject_bad_payload.ml
  File "./reject_bad_payload.ml", line 1, characters 18-33:
  1 | let f n = (n + 1) [@mutate bogus]
                        ^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_double_off.ml
  File "./reject_double_off.ml", line 5, characters 0-15:
  5 | [@@@mutate off]
      ^^^^^^^^^^^^^^^
  Error: Mutation is already off.
  [1]

  $ mut --impl ./reject_empty_payload.ml
  File "./reject_empty_payload.ml", line 1, characters 18-27:
  1 | let f n = (n + 1) [@mutate]
                        ^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_exclude_file_binding.ml
  File "./reject_exclude_file_binding.ml", line 1, characters 16-39:
  1 | let f n = n + 1 [@@mutate exclude_file]
                      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_exclude_file_expr.ml
  File "./reject_exclude_file_expr.ml", line 1, characters 18-40:
  1 | let f n = (n + 1) [@mutate exclude_file]
                        ^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_misplaced_exclude_file.ml
  File "./reject_misplaced_exclude_file.ml", line 2, characters 2-26:
  2 |   [@@@mutate exclude_file]
        ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: mutate exclude_file is not allowed here.
  [1]

  $ mut --impl ./reject_misplaced_on.ml
  File "./reject_misplaced_on.ml", line 1, characters 18-30:
  1 | let f n = (n + 1) [@mutate on]
                        ^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_off_number.ml
  File "./reject_off_number.ml", line 1, characters 18-34:
  1 | let f n = (n + 1) [@mutate off 42]
                        ^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_off_rewrite.ml
  File "./reject_off_rewrite.ml", line 3, characters 18-51:
  3 | let f n = (n + 1) [@mutate off sub "equal at zero"]
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_off_two_reasons.ml
  File "./reject_off_two_reasons.ml", line 1, characters 18-39:
  1 | let f n = (n + 1) [@mutate off "a" "b"]
                        ^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in mutate attribute.
  [1]

  $ mut --impl ./reject_on_assert.ml
  File "./reject_on_assert.ml", line 1, characters 19-31:
  1 | let f x = assert x [@mutate on]
                         ^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_on_binding.ml
  File "./reject_on_binding.ml", line 1, characters 16-29:
  1 | let f n = n + 1 [@@mutate on]
                      ^^^^^^^^^^^^^
  Error: mutate on is not allowed here.
  [1]

  $ mut --impl ./reject_on_outside.ml
  File "./reject_on_outside.ml", line 3, characters 0-14:
  3 | [@@@mutate on]
      ^^^^^^^^^^^^^^
  Error: Mutation is already on.
  [1]
