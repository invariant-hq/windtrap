(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = Format.formatter -> 'a -> unit
type style = [ `Bold | `Faint | `Red | `Green | `Yellow | `Cyan | `White ]

(* ───── Output ───── *)

let str = Format.asprintf
let pf = Format.fprintf
let pr fmt = Format.fprintf Format.std_formatter fmt
let epr fmt = Format.fprintf Format.err_formatter fmt
let flush ppf () = Format.pp_print_flush ppf ()
let to_string pp v = Format.asprintf "%a" pp v

(* ───── Printers ───── *)

let string = Format.pp_print_string
let int = Format.pp_print_int
let int32 ppf n = Format.fprintf ppf "%ld" n
let int64 ppf n = Format.fprintf ppf "%Ld" n
let float = Format.pp_print_float
let bool = Format.pp_print_bool
let char = Format.pp_print_char

(* ───── Combinators ───── *)

let semi ppf () = Format.fprintf ppf ";@ "
let comma ppf () = Format.fprintf ppf ",@ "

let list ?(sep = semi) pp ppf l =
  (* The box gives the separators' break hints a known size; without it a
     trailing hint is still unsized at flush time and Format renders it
     as a newline. *)
  Format.pp_open_box ppf 0;
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp ppf x
    | x :: xs ->
        pp ppf x;
        sep ppf ();
        loop xs
  in
  loop l;
  Format.pp_close_box ppf ()

let array ?(sep = semi) pp ppf arr = list ~sep pp ppf (Array.to_list arr)

let option pp ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "Some %a" pp v

let result ~ok ~error ppf = function
  | Ok v -> Format.fprintf ppf "Ok %a" ok v
  | Error e -> Format.fprintf ppf "Error %a" error e

let pair pp_a pp_b ppf (a, b) = Format.fprintf ppf "(@[%a,@ %a@])" pp_a a pp_b b
let brackets pp ppf v = Format.fprintf ppf "[@[%a@]]" pp v

(* ───── Styling ───── *)

let code_of_style = function
  | `Bold -> "\027[1m"
  | `Faint -> "\027[2m"
  | `Red -> "\027[31m"
  | `Green -> "\027[32m"
  | `Yellow -> "\027[33m"
  | `Cyan -> "\027[36m"
  | `White -> "\027[37m"

let reset = "\027[0m"

let styled ~ansi style pp ppf v =
  if not ansi then pp ppf v
  else begin
    (* Escape codes print at width 0 so Format's line breaking is not
       perturbed by the invisible bytes. *)
    Format.pp_print_as ppf 0 (code_of_style style);
    pp ppf v;
    Format.pp_print_as ppf 0 reset
  end

let styled_string ~ansi style s =
  if not ansi then s else code_of_style style ^ s ^ reset
