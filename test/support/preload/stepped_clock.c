/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Preloaded into a child, the system clock steps forward at every reading,
   as a clock set ahead does: reading n is 2^(n+10) seconds past the true
   time, so the interval from any reading to the next is longer than the one
   between any two earlier readings. The monotonic clocks run true. */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <sys/time.h>
#include <time.h>

static int readings;

/* Past 40 readings the clock stops stepping, before time_t overflows. */
static time_t ahead (void)
{
  int n = readings < 40 ? readings++ : 40;
  return (time_t) 1 << (n + 10);
}

int gettimeofday (struct timeval *restrict tv, void *restrict tz)
{
  static int (*next) (struct timeval *restrict, void *restrict);
  int r;

  if (next == NULL)
    next = (int (*) (struct timeval *restrict, void *restrict))
      dlsym (RTLD_NEXT, "gettimeofday");
  r = next (tv, tz);
  if (r == 0) tv->tv_sec += ahead ();
  return r;
}

int clock_gettime (clockid_t clock, struct timespec *ts)
{
  static int (*next) (clockid_t, struct timespec *);
  int r;

  if (next == NULL)
    next = (int (*) (clockid_t, struct timespec *))
      dlsym (RTLD_NEXT, "clock_gettime");
  r = next (clock, ts);
  if (r == 0 && clock == CLOCK_REALTIME) ts->tv_sec += ahead ();
  return r;
}
