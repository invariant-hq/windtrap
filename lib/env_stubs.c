/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Unbinding an environment variable.

   [Unix] binds (putenv) and has no inverse, and binding to the empty
   string is not unbinding: Sys.getenv_opt then answers Some "", which
   reads as set to every program that asks. The runner's test-scoped
   setenv must restore a variable that was originally unset, so the
   missing half comes from here: POSIX unsetenv(3), and on Windows the
   empty assignment _putenv documents as deletion.

   The caller (Env.set) has already rejected the names POSIX rejects — an
   empty one, or one containing '=' — so a failure here is the allocation
   failure neither platform can rule out. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#include <stdlib.h>
#include <string.h>

CAMLprim value ocaml_windtrap_unsetenv (value name)
{
  CAMLparam1 (name);
  const char *n = String_val (name);

#if defined(_WIN32)
  size_t len = strlen (n);
  char *entry = (char *) malloc (len + 2);
  int failed;
  if (entry == NULL)
    caml_raise_sys_error (caml_copy_string ("Windtrap.Env: out of memory"));
  memcpy (entry, n, len);
  entry[len] = '=';
  entry[len + 1] = '\0';
  /* _putenv copies its argument, unlike POSIX putenv. */
  failed = (_putenv (entry) != 0);
  free (entry);
#else
  int failed = (unsetenv (n) != 0);
#endif

  if (failed)
    caml_raise_sys_error (caml_copy_string ("Windtrap.Env: unsetenv () failed"));

  CAMLreturn (Val_unit);
}
