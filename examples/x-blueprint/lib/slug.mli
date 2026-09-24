(** URL-friendly slugs from arbitrary strings. *)

val slugify : string -> string
(** [slugify s] lowercases ASCII letters, replaces every run of other bytes with
    a single ['-'], and never starts or ends with a separator. Idempotent. Known
    limitation: UTF-8 letters are treated as separators (issue #1, reproduced in
    [test/failures/]). *)
