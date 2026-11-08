(* [none] replaces an application of [Some] by [None]. The guard is
   [if armed then Stdlib.Option.None else Some e], whose [None] comes first
   so that [Some e] is typed against what the context expects. A [Some]
   inside the argument is a site of its own, and so is a literal. *)

let find p x = if p x then Some x else None
let nested x = Some (Some x)
let pair x = Some (x, 1)

(* No site: a qualified [Some], a pattern, [None], module initialization,
   and the right side of a [let rec], where an [if] cannot stand. A [lazy]
   and a function in that right side are mutated. *)
let qualified x = Option.Some x
let get = function Some x -> x | None -> 0
let default = Some 1

type node = Node of node option | Later of node option Lazy.t

let loop () =
  let rec n = Some (Node n) in
  n

let later () =
  let rec n = Later (lazy (Some n)) in
  n

let last l =
  let rec last = function [] -> None | [ x ] -> Some x | _ :: rest -> last rest in
  last l
