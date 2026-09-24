(* The same exclusion, in the spelling a real build sees: instrumentation
   runs after every other rewriter, so by the time this pass looks at the
   file [let%test] has already become a call into the inline runtime.
   Mentioning that runtime at all is what marks the file as test code. *)

let sum a b = a + b

let () =
  Ppx_windtrap_runtime.Ppx_runtime.add_test ~file:"fixture_inline_expanded.ml"
    ~tags:[] "sums" (fun () -> assert (sum 1 2 = 3))

(* Rules pinned here, by id in RULES.md and interface line: M28, mut:106-108. *)
