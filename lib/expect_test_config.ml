(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module IO = struct
  type 'a t = 'a

  let return x = x
end

let run f = f ()
let sanitize s = s
let upon_unreleasable_issue = `CR
