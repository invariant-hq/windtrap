(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let random_state = lazy (Random.State.make_self_init ())

(* 8-character base-36 string (0-9, A-Z) giving ~2.8 trillion possible IDs *)
let generate () =
  let state = Lazy.force random_state in
  String.init 8 (fun _ ->
      let n = Random.State.int state 36 in
      if n < 10 then Char.chr (n + Char.code '0')
      else Char.chr (n - 10 + Char.code 'A'))
