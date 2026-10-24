(* Planted: [store] logs the last digit of the pin. *)

(* A store of a pin that keeps a public log of what was done. *)

type t = { mutable pin : int option; mutable log : string list }

let create () = { pin = None; log = [] }

let store v pin =
  v.pin <- Some pin;
  v.log <- v.log @ [ Printf.sprintf "stored ...%d" (pin mod 10) ]

let check v pin =
  v.log <- v.log @ [ "checked" ];
  v.pin = Some pin

let log v = v.log
