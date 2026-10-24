/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Code under test that never returns to OCaml for a while, and code that
   crashes the process. */

#include <stdint.h>
#include <time.h>
#include <unistd.h>

#include <caml/mlvalues.h>

/* Spins for [ms] milliseconds without returning to OCaml, so no signal
   handler of the runtime's runs meanwhile, then says it ran to its end. */
value verdict_spin (value ms)
{
  static const char done[] = "the C loop ran to its end\n";
  struct timespec start, now;
  long elapsed;

  clock_gettime (CLOCK_MONOTONIC, &start);
  do
  {
    clock_gettime (CLOCK_MONOTONIC, &now);
    elapsed = (now.tv_sec - start.tv_sec) * 1000
      + (now.tv_nsec - start.tv_nsec) / 1000000;
  }
  while (elapsed < Long_val (ms));
  if (write (1, done, sizeof done - 1) < 0) return Val_unit;
  return Val_unit;
}

/* Writes at [address], which the caller makes 0. */
value verdict_poke (value address)
{
  *(volatile int *) (intptr_t) Long_val (address) = 0;
  return Val_unit;
}
