(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = Format.formatter -> 'a -> unit
type style = [ `Bold | `Faint | `Green | `Red | `Yellow | `Cyan | `White ]

(* ───── Style Configuration ───── *)

let use_ansi_stdout = ref false
let use_ansi_stderr = ref false

let ansi_of_style = function
  | `Bold -> "\027[1m"
  | `Faint -> "\027[2m"
  | `Green -> "\027[32m"
  | `Red -> "\027[31m"
  | `Yellow -> "\027[33m"
  | `Cyan -> "\027[36m"
  | `White -> "\027[37m"

let ansi_reset = "\027[0m"

(* ───── Basic Output ───── *)

let str = Format.asprintf
let pf = Format.fprintf
let pr fmt = Format.fprintf Format.std_formatter fmt
let epr fmt = Format.fprintf Format.err_formatter fmt
let flush ppf () = Format.pp_print_flush ppf ()

(* ───── Styled Output ───── *)

let styled style pp ppf v =
  (* Physical equality (==) is intentional: we can only detect stdout/stderr
     by identity since Format provides no other way to query a formatter's
     underlying channel. Other formatters (e.g. Buffer) never get ANSI. *)
  let use_ansi =
    if ppf == Format.std_formatter then !use_ansi_stdout
    else if ppf == Format.err_formatter then !use_ansi_stderr
    else false
  in
  if use_ansi then begin
    Format.pp_print_string ppf (ansi_of_style style);
    pp ppf v;
    Format.pp_print_string ppf ansi_reset
  end
  else pp ppf v

(* ───── Basic Formatters ───── *)

let string = Format.pp_print_string
let int = Format.pp_print_int
let int32 ppf n = Format.fprintf ppf "%ld" n
let int64 ppf n = Format.fprintf ppf "%Ld" n
let bool = Format.pp_print_bool
let char = Format.pp_print_char

(* ───── Combinators ───── *)

let semi ppf () = Format.fprintf ppf ";@ "
let comma ppf () = Format.fprintf ppf ",@ "

let list ?(sep = semi) pp ppf l =
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: xs ->
        pp ppf x;
        sep ppf ();
        loop xs
  in
  loop l

let array ?(sep = semi) pp ppf arr = list ~sep pp ppf (Array.to_list arr)

let option pp ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some %a" pp v

let result ~ok ~error ppf = function
  | Ok v -> Format.fprintf ppf "Ok %a" ok v
  | Error e -> Format.fprintf ppf "Error %a" error e

let pair pp_a pp_b ppf (a, b) = Format.fprintf ppf "(@[%a,@ %a@])" pp_a a pp_b b
let brackets pp ppf v = Format.fprintf ppf "[@[%a@]]" pp v

(* ───── Utilities ───── *)

let to_string pp v = Format.asprintf "%a" pp v

let styled_string style s =
  if not !use_ansi_stdout then s else ansi_of_style style ^ s ^ ansi_reset
