/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* C-stdio flushing for Windtrap.Capture.

   fd-level capture sees only bytes that reach the file descriptors. C
   stubs writing through C stdio (printf, fprintf(stderr, ...)) buffer in
   libc, invisibly to OCaml's flush; ppx_expect flushes those streams at
   every consumption point and windtrap must too, or output printed from
   a stub without an fflush is missed at the [%expect] node boundary and
   leaks to the console when the process exits. */

#include <caml/mlvalues.h>

#include <stdio.h>

CAMLprim value ocaml_windtrap_capture_flush_c_stdio (value unit)
{
  fflush (stdout);
  fflush (stderr);
  return unit;
}
