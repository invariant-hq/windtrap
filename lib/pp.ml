(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = Format.formatter -> 'a -> unit
type style = [ `Bold | `Faint | `Red | `Green | `Yellow | `Cyan | `White ]

(* Output *)

let str = Format.asprintf
let pf = Format.fprintf
let flush ppf () = Format.pp_print_flush ppf ()
let to_string pp v = Format.asprintf "%a" pp v

(* Printers *)

let abstract = "<abstract>"
let string = Format.pp_print_string
let int = Format.pp_print_int
let int32 ppf n = Format.fprintf ppf "%ld" n
let int64 ppf n = Format.fprintf ppf "%Ld" n

(* Shortest decimal rendering that round-trips to the exact bits: 15
   significant digits when they suffice, else 16, else 17 (always enough for
   a double). This is the module's only float printer, deliberately: a value
   printed at a fixed precision is not the value that was there, and
   everything this library prints a float into is something a reader is
   expected to copy back (a property counterexample pasted into [~examples],
   a bit-exact witness). A caller that wants a compact, lossy rendering asks
   for it at the call site, as [Testable]'s [%g] instances do. Sign of zero
   survives; non-finite values render as [nan], [inf], [-inf]. *)
let float_exact ppf f =
  if Float.is_nan f || not (Float.is_finite f) then
    Format.pp_print_string ppf (Printf.sprintf "%g" f)
  else
    let round_trips s =
      Int64.equal
        (Int64.bits_of_float (float_of_string s))
        (Int64.bits_of_float f)
    in
    let s15 = Printf.sprintf "%.15g" f in
    let s =
      if round_trips s15 then s15
      else
        let s16 = Printf.sprintf "%.16g" f in
        if round_trips s16 then s16 else Printf.sprintf "%.17g" f
    in
    (* [%g] drops the point on a whole value: [1.] renders as ["1"], which is
       an int literal, not a float one. The whole reason to round-trip is
       that a reader can paste the value back — into [~examples], into a
       [let] — so it has to stay syntactically a float. *)
    let is_float_syntax =
      String.exists (fun c -> c = '.' || c = 'e' || c = 'E') s
    in
    Format.pp_print_string ppf (if is_float_syntax then s else s ^ ".")

let bool = Format.pp_print_bool

(* Combinators *)

let semi ppf () = Format.fprintf ppf ";@ "

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

(* Styling *)

let code_of_style = function
  | `Bold -> "\027[1m"
  | `Faint -> "\027[2m"
  | `Red -> "\027[31m"
  | `Green -> "\027[32m"
  | `Yellow -> "\027[33m"
  | `Cyan -> "\027[36m"
  | `White -> "\027[37m"

let reset = "\027[0m"

(* An empty payload is returned bare: wrapping it would emit an open code
   and its reset with nothing between them — invisible, but real bytes on
   lines that are assembled from optional fragments (the attempt suffix on
   a FAIL header is empty on all but a retried test). *)
let styled_string ~ansi style s =
  if (not ansi) || String.length s = 0 then s
  else code_of_style style ^ s ^ reset
