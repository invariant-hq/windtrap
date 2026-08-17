(* The typing-context corpus. OCaml types an application's arguments left
   to right, and type-directed record disambiguation leans on that order:
   one qualified access ([rect.Layout.x]) teaches the checker the
   record's type, and every later unqualified field of the same record
   ([rect.height]) resolves from what was learned. A guard that lifts its
   operands must therefore keep typing them in source order - an encoding
   that type-checks the right operand first asks about [rect.height]
   before anything has said what [rect] is, and the whole file stops
   compiling with "Unbound record field".

   Each shape below is a real site the mutate backend broke, copied from
   the repositories that hit it; none of the field names is in scope
   unqualified, so every function here compiles only while the guards
   preserve the user's left-to-right typing context. That makes this
   module the regression test: it is preprocessed with the instrumenter
   directly (see dune), so an encoding that reorders type-checking is a
   build failure on every ordinary [dune build], not a red test.

   Unlike [forced], this library keeps dune's default warning set:
   type-directed disambiguation is warning 40's subject, so the [-w +a]
   battery would reject the UNinstrumented source and there would be
   nothing left to protect. *)

(* matrix_charts.ml: an [ari] chain of [+] across applications. The
   chain rule guards only the outermost [+], so its right operand - the
   last [max], whose [rect.height] no scope resolves - must still be
   type-checked after the first, qualified [rect.Layout.x]. The inner
   [-] and [+] sites nest inside the outer guard's operands. *)
module Layout = struct
  type t = { x : int; y : int; width : int; height : int }
end

let clip rect x0 y0 box_w box_h =
  max 0 (rect.Layout.x - x0)
  + max 0 (x0 + box_w - (rect.x + rect.width))
  + max 0 (rect.y - y0)
  + max 0 (y0 + box_h - (rect.y + rect.height))

(* coordinates.ml: one [-] site whose two operands are the teaching and
   the taught access - the smallest expression that can break. *)
module Line = struct
  type t = { start : int; end_ : int }
end

let span line = max (line.Line.end_ - line.start) 0

(* The same one-site shape under a comparison, in the condition position
   [cmp] fires in: the swapping encoding lifts its operands exactly as
   [ari] does, so it must preserve the same order. *)
let inverted line = if line.Line.end_ < line.start then 1 else 0

(* toffee_compute_flexbox.ml: a [+.] chain over nested projections, with
   a second record type sharing a field name. Here the first access
   pins [child]'s type through the uniquely-named [padding], and
   [child.border] then disambiguates against [overlay.border] - but only
   if it is type-checked second. An encoding that starts from the right
   operand resolves [border] by scope instead, pins [child] to
   [overlay], and fails on a field that was never ambiguous in the
   source. *)
module Sides = struct
  type t = { left : float; right : float; top : float; bottom : float }
end

type item = { padding : Sides.t; border : Sides.t }
type overlay = { border : int }

let thickness overlay = overlay.border

let horizontal child =
  child.padding.left +. child.padding.right +. child.border.left
  +. child.border.right
