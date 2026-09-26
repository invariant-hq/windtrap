(* The edges of the dismissal grammar: the positions where a well-formed
   [mutate] attribute is silently ignored, the expression dismissal that
   catalogues nothing, and a reason string that has to survive being
   re-emitted as an OCaml literal.

   These are pinned because they are silent. A user who writes one of the
   ignored spellings gets no diagnostic and no dismissal, and would learn
   about it from a survivor they thought they had dismissed. *)

(* Read: an expression dismissal on an expression that IS a site. The
   reason is re-emitted into the site table as a string literal, so a
   quote, a backslash and a newline in it must come back out escaped. *)
let quoted want =
  if
    (want > 16)
    [@mutate
      off "a \"quoted\" reason,\nspanning lines \\ containing a backslash"]
  then want
  else 16

(* Read: an expression dismissal on an expression that is NOT a site.
   Nothing is catalogued - there is no mutant to dismiss - and the whole
   subtree is suppressed, so the [+] inside carries none either. *)
let coarse a b = (match a with 0 -> b | n -> n + b) [@mutate off "not a site"]

(* IGNORED: [[@@mutate off]] on a [let ... in] binding. The pass reads
   that attribute on structure-level bindings only, exactly where the
   coverage instrumenter reads its counterpart, so this site survives. *)
let local a b =
  let s = a + b [@@mutate off "ignored here"] in
  s

(* IGNORED: [[@@mutate off]] on a structure item that is not a value or
   module binding. This one suppresses nothing at all, since a type
   declaration carries no site to begin with - but the [+] below is
   proof that the attribute did not turn anything off either. *)
type t = int [@@mutate off]

let after a b = a + b

(* Rules pinned here, by id in RULES.md and interface line: M38, mut:126-129;
   M40, mut:129-131; M42, mut:133-135. *)
