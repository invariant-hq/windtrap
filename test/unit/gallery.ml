(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Renderings kept as baselines. A gallery is a file of titled entries. A
   styled rendering is written with each escape sequence as its mark, [«r|]
   for red and [»] for the reset, so that the file reads and diffs as text,
   and the marks spell the bytes back. *)

open Windtrap

let marks =
  [
    ("\027[31m", "\u{ab}r|");
    ("\027[32m", "\u{ab}g|");
    ("\027[33m", "\u{ab}y|");
    ("\027[2m", "\u{ab}d|");
    ("\027[1m", "\u{ab}b|");
    ("\027[1;31m", "\u{ab}R|");
    ("\027[1;32m", "\u{ab}G|");
    ("\027[0m", "\u{bb}");
    ("\r\027[2K", "\u{ab}erase\u{bb}");
  ]

(* [s] with each [from] of [table] replaced by its [into], left to right. *)
let replace table s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then
      match
        List.find_opt
          (fun (from, _) ->
            i + String.length from <= String.length s
            && String.equal (String.sub s i (String.length from)) from)
          table
      with
      | Some (from, into) ->
          Buffer.add_string b into;
          go (i + String.length from)
      | None ->
          Buffer.add_char b s.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* [s] without its CSI sequences, from ESC [\[] to the final byte, the one
   kind a report writes. *)
let unstyled s =
  let b = Buffer.create (String.length s) in
  let rec final i =
    if i >= String.length s || ('\x40' <= s.[i] && s.[i] <= '\x7e') then i + 1
    else final (i + 1)
  in
  let rec go i =
    if i < String.length s then
      if s.[i] = '\027' && i + 1 < String.length s && s.[i + 1] = '[' then
        go (final (i + 2))
      else begin
        Buffer.add_char b s.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b

let marked s =
  let m = replace marks s in
  equal ~msg:"the marks spell the bytes back" text s
    (replace (List.map (fun (escape, mark) -> (mark, escape)) marks) m);
  not_contains ~msg:"every escape has a mark" ~sub:"\027" m;
  m

let check path entries =
  let entry (title, rendering) =
    let rendering =
      if rendering = "" || String.ends_with ~suffix:"\n" rendering then
        rendering
      else rendering ^ "\n"
    in
    "=== " ^ title ^ "\n" ^ rendering
  in
  expect_file (marked (String.concat "\n" (List.map entry entries))) path
