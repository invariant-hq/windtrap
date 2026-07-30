(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Pp = Windtrap.Private.Pp

let s = Pp.to_string

let tests =
  [
    test "basic printers" (fun () ->
        equal ~msg:"string prints verbatim" string "hello" (s Pp.string "hello");
        equal ~msg:"int" string "42" (s Pp.int 42);
        equal ~msg:"negative int" string "-7" (s Pp.int (-7));
        equal ~msg:"int32" string "5" (s Pp.int32 5l);
        equal ~msg:"int64" string "9007199254740993"
          (s Pp.int64 9007199254740993L);
        equal ~msg:"float keeps trailing dot" string "1." (s Pp.float 1.);
        equal ~msg:"bool" string "true" (s Pp.bool true);
        equal ~msg:"char" string "x" (s Pp.char 'x'));
    test "str and pf agree with to_string" (fun () ->
        equal ~msg:"str formats like sprintf" string "a=1 b=two"
          (Pp.str "a=%d b=%s" 1 "two");
        equal ~msg:"pf into a buffer formatter" string "[7]"
          (let b = Buffer.create 8 in
           let ppf = Format.formatter_of_buffer b in
           Pp.pf ppf "[%d]" 7;
           Pp.flush ppf ();
           Buffer.contents b));
    test "combinators" (fun () ->
        equal ~msg:"list with default semi separator" string "1; 2; 3"
          (s (Pp.list Pp.int) [ 1; 2; 3 ]);
        equal ~msg:"list with comma separator" string "1, 2"
          (s (Pp.list ~sep:Pp.comma Pp.int) [ 1; 2 ]);
        equal ~msg:"singleton list has no separator" string "9"
          (s (Pp.list Pp.int) [ 9 ]);
        equal ~msg:"empty list is empty" string "" (s (Pp.list Pp.int) []);
        equal ~msg:"array matches list" string "1; 2"
          (s (Pp.array Pp.int) [| 1; 2 |]);
        equal ~msg:"option none" string "None" (s (Pp.option Pp.int) None);
        equal ~msg:"option some" string "Some 3" (s (Pp.option Pp.int) (Some 3));
        equal ~msg:"result ok" string "Ok 1"
          (s (Pp.result ~ok:Pp.int ~error:Pp.string) (Ok 1));
        equal ~msg:"result error" string "Error boom"
          (s (Pp.result ~ok:Pp.int ~error:Pp.string) (Error "boom"));
        equal ~msg:"pair" string "(1, x)"
          (s (Pp.pair Pp.int Pp.string) (1, "x"));
        equal ~msg:"brackets" string "[1; 2]"
          (s (Pp.brackets (Pp.list Pp.int)) [ 1; 2 ]));
    test "styled is the identity without ansi and wraps with it" (fun () ->
        equal ~msg:"styled ~ansi:false is the identity" string "hi"
          (s (Pp.styled ~ansi:false `Red Pp.string) "hi");
        equal ~msg:"styled ~ansi:true wraps in escape codes" string
          "\027[31mhi\027[0m"
          (s (Pp.styled ~ansi:true `Red Pp.string) "hi");
        equal ~msg:"styled bold code" string "\027[1mb\027[0m"
          (s (Pp.styled ~ansi:true `Bold Pp.string) "b");
        equal ~msg:"styled_string ~ansi:false is the identity" string "plain"
          (Pp.styled_string ~ansi:false `Green "plain");
        equal ~msg:"styled_string ~ansi:true wraps" string "\027[32mok\027[0m"
          (Pp.styled_string ~ansi:true `Green "ok"));
    test "styling does not change line breaking" (fun () ->
        (* Zero-width escapes: styling must not perturb Format's line breaking.
           With a margin of 10, "aaaa bbbb" breaks identically styled or not
           (escape codes are printed at width 0). *)
        let render ~ansi =
          let b = Buffer.create 32 in
          let ppf = Format.formatter_of_buffer b in
          Format.pp_set_margin ppf 10;
          Format.fprintf ppf "@[<hv>%a@ %a@]"
            (Pp.styled ~ansi `Red Pp.string)
            "aaaa"
            (Pp.styled ~ansi `Green Pp.string)
            "bbbb";
          Format.pp_print_flush ppf ();
          Buffer.contents b
        in
        let strip = Windtrap.Private.Text.strip_ansi in
        equal string (render ~ansi:false) (strip (render ~ansi:true)));
  ]
