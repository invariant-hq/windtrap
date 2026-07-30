(* Windtrap-authored shim (not upstream source): the monadic
   Expect_test_config an Async suite would put in scope. Generated
   [let%expect_test] code references the ambient [Expect_test_config.run]
   with type [(unit -> unit) -> unit]; this one's [run] is
   [(unit -> unit IO.t) -> unit] with [IO.t = 'a Async.Deferred.t], so
   the reference must fail to compile, loudly (RFC compat mechanism (b)). *)

module IO = struct
  type 'a t = 'a Async.Deferred.t
end

let run (f : unit -> unit IO.t) : unit = ignore (f ())
let sanitize s = s
