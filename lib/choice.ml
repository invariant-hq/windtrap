(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* After Hypothesis (MacIver and Donaldson, "Test-Case Reduction via Test-Case
   Generation", ECOOP 2020): a generator reads bounded choices from a source,
   and a case is the sequence it read. *)

type span = { start : int; stop : int }
type key = Unique of int | Range of int * int64 * int64

(* The pools of a case, by generator key (see [Gen.parse]). *)
module Keyed = Hashtbl.Make (struct
  type t = key

  let equal a b =
    match (a, b) with
    | Unique a, Unique b -> Int.equal a b
    | Range (t, l, h), Range (t', l', h') ->
        Int.equal t t' && Int64.equal l l' && Int64.equal h h'
    | Unique _, Range _ | Range _, Unique _ -> false

  let hash = Hashtbl.hash
end)

type dependent = { at : int; over : int list }

type t = {
  prefix : int64 array;
  mutable state : Seed.state option; (* [None] when replaying *)
  mutable read : int64 list; (* newest first *)
  mutable bounds : int64 list;
  mutable length : int;
  mutable spans : span list;
  mutable dependents : dependent list;
  mutable redundant : bool;
  (* A copy reads [buffer] from [copy_at] up to [copy_stop]; [pools] holds
     the spans each generator read, and [buffer] the choices in order, from
     the first pool on. *)
  mutable copy_at : int; (* the next copied position, below [copy_stop] *)
  mutable copy_stop : int; (* [-1] outside a copy *)
  mutable pools : pool Keyed.t option;
  mutable buffer : int64 array; (* the choices, in order, once pools exist *)
}

and pool = { mutable spans : span array; mutable count : int }

let make prefix state =
  {
    prefix;
    state;
    read = [];
    bounds = [];
    length = 0;
    spans = [];
    dependents = [];
    redundant = false;
    copy_at = 0;
    copy_stop = -1;
    pools = None;
    buffer = [||];
  }

let generating state = make [||] (Some state)
let replaying choices = make choices None
let state src = src.state
let position src = src.length
let choices src = Array.of_list (List.rev src.read)
let bounds src = Array.of_list (List.rev src.bounds)
let spans src = src.spans
let dependents src = src.dependents
let redundant src = src.redundant
let respelled src = src.redundant <- true

let sample src draw =
  match src.state with
  | None -> None
  | Some state ->
      let v, state = draw state in
      src.state <- Some state;
      Some v

(* As 0.2 re-generated: [f] draws from a stream split off, and the source
   goes on from the stream that the split continues. *)
let split src f =
  match src.state with
  | None -> f ()
  | Some state ->
      let fresh, continued = Seed.split state in
      src.state <- Some fresh;
      let v = f () in
      src.state <- Some continued;
      v

let umin a b = if Int64.unsigned_compare a b <= 0 then a else b

(* A choice whose bound is [0] is recorded too: a deletion that brings a
   bound to [0] then leaves the choices after it where they were. *)
let choice src ~bound drawn =
  let v =
    if src.copy_at < src.copy_stop then begin
      let c = src.buffer.(src.copy_at) in
      src.copy_at <- src.copy_at + 1;
      umin c bound
    end
    else
      match drawn with
      | Some v -> umin v bound
      | None ->
          if src.length < Array.length src.prefix then
            umin src.prefix.(src.length) bound
          else 0L
  in
  if Option.is_some src.pools then begin
    if src.length = Array.length src.buffer then begin
      let grown = Array.make (Int.max 64 (2 * src.length)) 0L in
      Array.blit src.buffer 0 grown 0 src.length;
      src.buffer <- grown
    end;
    src.buffer.(src.length) <- v
  end;
  src.read <- v :: src.read;
  src.bounds <- bound :: src.bounds;
  src.length <- src.length + 1;
  v

let span src f =
  let start = src.length in
  let v = f () in
  if src.length - start > 0 then
    src.spans <- { start; stop = src.length } :: src.spans;
  v

(* Copies *)

let copying src = src.copy_stop >= 0

let copy src { start; stop } f =
  src.copy_at <- start;
  src.copy_stop <- stop;
  Fun.protect
    ~finally:(fun () ->
      src.copy_at <- 0;
      src.copy_stop <- -1)
    f

let pools src =
  match src.pools with
  | Some t -> t
  | None ->
      let t = Keyed.create 8 in
      src.buffer <- Array.of_list (List.rev src.read);
      src.pools <- Some t;
      t

let pool src key =
  let pools = pools src in
  match Keyed.find_opt pools key with
  | Some p -> p
  | None ->
      let p = { spans = [||]; count = 0 } in
      Keyed.replace pools key p;
      p

let pooled p span =
  if p.count = Array.length p.spans then begin
    let grown = Array.make (Int.max 8 (2 * p.count)) span in
    Array.blit p.spans 0 grown 0 p.count;
    p.spans <- grown
  end;
  p.spans.(p.count) <- span;
  p.count <- p.count + 1

let counts_past src ~at over =
  if over <> [] then src.dependents <- { at; over } :: src.dependents

(* Tokens: the choices as unsigned LEB128 in unpadded base64url after [r1:],
   then [:] and the digest of the value's printing. *)

let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let base64 bytes =
  let n = String.length bytes in
  let out = Buffer.create (((n * 4) + 2) / 3) in
  let byte i = if i < n then Char.code bytes.[i] else 0 in
  let rec group i =
    if i < n then begin
      let word = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      for k = 0 to Int.min 3 (n - i) do
        Buffer.add_char out alphabet.[(word lsr (18 - (6 * k))) land 63]
      done;
      group (i + 3)
    end
  in
  group 0;
  Buffer.contents out

let of_base64 text =
  let out = Buffer.create (String.length text) in
  let rec go i bits count =
    if i = String.length text then
      if bits land ((1 lsl count) - 1) = 0 then Some (Buffer.contents out)
      else None
    else
      match String.index_opt alphabet text.[i] with
      | None -> None
      | Some sextet ->
          let bits = (bits lsl 6) lor sextet and count = count + 6 in
          if count >= 8 then begin
            Buffer.add_char out (Char.chr ((bits lsr (count - 8)) land 0xff));
            go (i + 1) (bits land ((1 lsl (count - 8)) - 1)) (count - 8)
          end
          else go (i + 1) bits count
  in
  go 0 0 0

let leb128 choices =
  let out = Buffer.create (Array.length choices) in
  let rec put v =
    let low = Int64.to_int (Int64.logand v 0x7fL) in
    let v = Int64.shift_right_logical v 7 in
    if Int64.equal v 0L then Buffer.add_char out (Char.chr low)
    else begin
      Buffer.add_char out (Char.chr (low lor 0x80));
      put v
    end
  in
  Array.iter put choices;
  Buffer.contents out

(* A varint longer than ten bytes, or whose tenth byte carries more than the
   64th bit, is no choice. *)
let of_leb128 bytes =
  let n = String.length bytes in
  let rec varint i shift acc =
    if i = n || shift > 63 then None
    else
      let b = Char.code bytes.[i] in
      if shift = 63 && b > 1 then None
      else
        let acc =
          Int64.logor acc (Int64.shift_left (Int64.of_int (b land 0x7f)) shift)
        in
        if b < 0x80 then Some (acc, i + 1) else varint (i + 1) (shift + 7) acc
  in
  let rec all i choices =
    if i = n then Some (Array.of_list (List.rev choices))
    else
      match varint i 0 0L with
      | None -> None
      | Some (v, i) -> all i (v :: choices)
  in
  all 0 []

(* A case file is read totally, so that every byte string is a case: a
   varint ends at its first byte below [0x80] or at the end of the file, and
   the bits past the 64th are dropped. *)
let of_bytes bytes =
  let n = String.length bytes in
  let rec varint i shift acc =
    if i = n then (acc, i)
    else
      let b = Char.code bytes.[i] in
      let acc =
        if shift > 63 then acc
        else
          Int64.logor acc (Int64.shift_left (Int64.of_int (b land 0x7f)) shift)
      in
      if b < 0x80 then (acc, i + 1) else varint (i + 1) (shift + 7) acc
  in
  let rec all i choices =
    if i = n then Array.of_list (List.rev choices)
    else
      let v, i = varint i 0 0L in
      all i (v :: choices)
  in
  all 0 []

let to_bytes = leb128
let token choices ~digest = "r1:" ^ base64 (leb128 choices) ^ ":" ^ digest

let of_token text =
  let refused =
    Error "a replay token is r1:, base64url choices, : and a digest"
  in
  match String.split_on_char ':' text with
  | [ "r1"; choices; digest ] when digest <> "" -> (
      match Option.bind (of_base64 choices) of_leb128 with
      | Some choices -> Ok (choices, digest)
      | None -> refused)
  | _ -> refused
