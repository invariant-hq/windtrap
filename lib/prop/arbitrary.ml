(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = { gen : 'a Gen.t; print : 'a -> string }

let make ~gen ~print = { gen; print }
let gen t = t.gen
let print t = t.print

(* ───── Printers ───── *)

let pp_list pp_elem xs = "[" ^ String.concat "; " (List.map pp_elem xs) ^ "]"

let pp_array pp_elem xs =
  "[|" ^ String.concat "; " (Array.to_list (Array.map pp_elem xs)) ^ "|]"

let pp_option pp_elem = function
  | None -> "None"
  | Some x -> "Some (" ^ pp_elem x ^ ")"

let pp_result pp_ok pp_err = function
  | Ok x -> "Ok (" ^ pp_ok x ^ ")"
  | Error e -> "Error (" ^ pp_err e ^ ")"

let pp_pair pp_a pp_b (a, b) = "(" ^ pp_a a ^ ", " ^ pp_b b ^ ")"

let pp_triple pp_a pp_b pp_c (a, b, c) =
  "(" ^ pp_a a ^ ", " ^ pp_b b ^ ", " ^ pp_c c ^ ")"

let pp_quad pp_a pp_b pp_c pp_d (a, b, c, d) =
  "(" ^ pp_a a ^ ", " ^ pp_b b ^ ", " ^ pp_c c ^ ", " ^ pp_d d ^ ")"

(* Windows string_of_float may include leading exponent zero (e.g. e-010). *)
let cut_exp_zero s =
  match String.split_on_char 'e' s with
  | [ significand; exponent ] -> (
      match exponent.[0] with
      | '+' -> Printf.sprintf "%se+%i" significand (int_of_string exponent)
      | _ -> Printf.sprintf "%se%i" significand (int_of_string exponent))
  | _ -> s

let pp_float f =
  if Float.is_nan f && Float.sign_bit f then "-nan"
  else if Sys.win32 then cut_exp_zero (string_of_float f)
  else string_of_float f

(* ───── Primitives ───── *)

let unit = make ~gen:Gen.unit ~print:(fun () -> "()")
let bool = make ~gen:Gen.bool ~print:string_of_bool
let int = make ~gen:Gen.int ~print:string_of_int
let int_range low high = make ~gen:(Gen.int_range low high) ~print:string_of_int
let int32 = make ~gen:Gen.int32 ~print:(fun i -> Int32.to_string i ^ "l")

let int32_range low high =
  make ~gen:(Gen.int32_range low high) ~print:(fun i -> Int32.to_string i ^ "l")

let int64 = make ~gen:Gen.int64 ~print:(fun i -> Int64.to_string i ^ "L")

let int64_range low high =
  make ~gen:(Gen.int64_range low high) ~print:(fun i -> Int64.to_string i ^ "L")

let float = make ~gen:Gen.float ~print:pp_float
let char = make ~gen:Gen.char ~print:(fun c -> Printf.sprintf "%C" c)
let string = make ~gen:Gen.string ~print:(fun s -> Printf.sprintf "%S" s)

let bytes =
  make ~gen:Gen.bytes ~print:(fun b -> Printf.sprintf "%S" (Bytes.to_string b))

(* ───── Containers ───── *)

let option arb = make ~gen:(Gen.option arb.gen) ~print:(pp_option arb.print)

let result ok_arb err_arb =
  make
    ~gen:(Gen.result ok_arb.gen err_arb.gen)
    ~print:(pp_result ok_arb.print err_arb.print)

let list arb = make ~gen:(Gen.list arb.gen) ~print:(pp_list arb.print)
let array arb = make ~gen:(Gen.array arb.gen) ~print:(pp_array arb.print)
let pair a b = make ~gen:(Gen.pair a.gen b.gen) ~print:(pp_pair a.print b.print)

let triple a b c =
  make
    ~gen:(Gen.triple a.gen b.gen c.gen)
    ~print:(pp_triple a.print b.print c.print)

let quad a b c d =
  make
    ~gen:(Gen.quad a.gen b.gen c.gen d.gen)
    ~print:(pp_quad a.print b.print c.print d.print)

(* ───── Choice ───── *)

let oneof arbs =
  match arbs with
  | [] -> invalid_arg "Arbitrary.oneof: empty list"
  | first :: _ ->
      make ~gen:(Gen.oneof (List.map (fun a -> a.gen) arbs)) ~print:first.print

let oneofl ~print xs = make ~gen:(Gen.oneofl xs) ~print

(* ───── Transformers ───── *)

let map ~print f arb = make ~gen:(Gen.map f arb.gen) ~print

let filter p arb =
  make ~gen:(Gen.add_shrink_invariant p arb.gen) ~print:arb.print
