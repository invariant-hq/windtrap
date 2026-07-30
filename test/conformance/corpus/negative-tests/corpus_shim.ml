(* Windtrap-authored build-layer shim (not upstream ppx_expect source).

   Upstream builds this directory against Core/ppx_jane; the conformance
   corpus builds with zero dependencies (stdlib only). Fixtures that said
   [open! Core] / [open Core] say [open! Corpus_shim] / [open Corpus_shim]
   instead — a single-line, line-count-preserving substitution applied
   identically to the fixture and its vendored golden; every such tweak
   is listed as a finding in ../../TRIAGE.md. This module supplies the
   few Core values those fixture bodies actually use. *)

let printf = Printf.printf
