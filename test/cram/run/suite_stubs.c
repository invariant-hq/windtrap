/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Code under test that does not return to OCaml for a while, in ISO C so
   that the fixture builds wherever OCaml does. */

#include <stdio.h>
#include <time.h>

#include <caml/mlvalues.h>

static void create (const char *name)
{
  FILE *f = fopen (name, "w");
  if (f) fclose (f);
}

/* Creates [ready], spins at least [seconds] without returning to OCaml,
   so that no handler of the runtime's runs meanwhile, then creates
   [spun]. */
value suite_spin (value seconds)
{
  time_t end = time (NULL) + Long_val (seconds) + 1;
  create ("ready");
  while (time (NULL) < end)
    ;
  create ("spun");
  return Val_unit;
}
