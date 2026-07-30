(* Windtrap-authored shim (not upstream source): a genuinely monadic
   Deferred, so the ambient Expect_test_config shim has Async's shape
   ([IO.t <> unit]) and generated code must fail to compile (RFC compat
   mechanism (b)). *)

module Deferred = struct
  type 'a t = Deferred of 'a
end

let return x = Deferred.Deferred x
