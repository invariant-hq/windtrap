(* Windtrap-authored build-layer shim (not upstream ppx_expect source).

   Upstream builds this directory against Core/Async/ppx_jane; the
   conformance corpus builds with zero dependencies. Fixtures that said
   [open Core] say [open Corpus_shim] instead — a single-line,
   line-count-preserving substitution listed as a finding in
   ../../TRIAGE.md. This module supplies the few Core values the
   fixture bodies actually use (control_chars.ml). *)

module List = struct
  let range a b = Stdlib.List.init (b - a) (fun i -> a + i)
  let map l ~f = Stdlib.List.map f l
end

module Char = struct
  let of_int_exn = Stdlib.Char.chr
end

module String = struct
  let of_char = Stdlib.String.make 1
  let concat l ~sep = Stdlib.String.concat sep l
end
