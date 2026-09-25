(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = Format.formatter -> 'a -> unit

type style =
  [ `Bold | `Faint | `Red | `Green | `Yellow | `Bold_red | `Bold_green ]

let str = Format.asprintf
let pf = Format.fprintf
let flush ppf () = Format.pp_print_flush ppf ()
let to_string pp v = str "%a" pp v
let abstract = "<abstract>"
let string = Format.pp_print_string
let int = Format.pp_print_int
let int32 ppf n = pf ppf "%ld" n
let int64 ppf n = pf ppf "%Ld" n

(* [shortest fmt ~from f] renders [f] under [fmt] at the fewest digits, from
   [from] on, that read back to the bits of [f]. Seventeen significant digits
   read back any double, so the cap never binds under [%g]; under [%f] it
   binds below 0.1, where [decimal] prints [f] rounded. *)
let shortest fmt ~from f =
  let reads_back s =
    Int64.equal
      (Int64.bits_of_float (float_of_string s))
      (Int64.bits_of_float f)
  in
  let rec render digits =
    let s = Printf.sprintf fmt digits f in
    if digits >= 17 || reads_back s then s else render (digits + 1)
  in
  render from

(* No exponent, so a configured [0.00001] prints as typed. *)
let decimal ppf f = string ppf (shortest "%.*f" ~from:0 f)

(* [%g] drops the point on a whole value, and [1] is an int literal where a
   reader pastes a float back, into [~examples] or a [let]. *)
let float_exact ppf f =
  let s = shortest "%.*g" ~from:15 f in
  let is_int_literal =
    Float.is_finite f && not (String.exists (fun c -> c = '.' || c = 'e') s)
  in
  string ppf (if is_int_literal then s ^ "." else s)

let bool = Format.pp_print_bool
let semi ppf () = pf ppf ";@ "

(* The box gives the separators' break hints a known size; without it a
   trailing hint is still unsized at flush time and Format renders it as a
   newline. *)
let list ?(sep = semi) pp ppf l =
  pf ppf "@[%a@]" (Format.pp_print_list ~pp_sep:sep pp) l

let array ?(sep = semi) pp ppf arr = list ~sep pp ppf (Array.to_list arr)

let option pp ppf = function
  | None -> string ppf "None"
  | Some v -> pf ppf "Some %a" pp v

let result ~ok ~error ppf = function
  | Ok v -> pf ppf "Ok %a" ok v
  | Error e -> pf ppf "Error %a" error e

let pair pp_a pp_b ppf (a, b) = pf ppf "(@[%a,@ %a@])" pp_a a pp_b b
let brackets pp ppf v = pf ppf "[@[%a@]]" pp v
