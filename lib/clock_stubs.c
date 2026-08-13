/*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Minimal monotonic clock for Windtrap, derived from mtime.
   Only elapsed_ns is exposed; now_ns and period_ns are removed. */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>

#define WINDTRAP_CLOCK_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Windtrap.Clock: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define WINDTRAP_CLOCK_DARWIN

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(__linux__)
   #define WINDTRAP_CLOCK_LINUX
 #endif
 #if defined(_POSIX_VERSION)
   #define WINDTRAP_CLOCK_POSIX
 #endif
#elif defined(_WIN32)
#define WINDTRAP_CLOCK_WINDOWS
#endif

/* Darwin */

#if defined(WINDTRAP_CLOCK_DARWIN)

#include <mach/mach_time.h>

static mach_timebase_info_data_t scale = {0};

void ocaml_windtrap_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    WINDTRAP_CLOCK_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    WINDTRAP_CLOCK_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_windtrap_clock_elapsed_ns (value unit)
{
  static uint64_t start = 0L;
  if (start == 0L) { start = mach_continuous_time (); }
  if (scale.denom == 0) { ocaml_windtrap_clock_init_scale (); }
  uint64_t now = mach_continuous_time ();
  return caml_copy_int64 (((now - start) * scale.numer) / scale.denom);
}

/* POSIX */

#elif defined(WINDTRAP_CLOCK_POSIX)

#include <time.h>

CAMLprim value ocaml_windtrap_clock_elapsed_ns (value unit)
{
  static struct timespec start = {0};
  struct timespec now;
  clockid_t clockid;

#if defined(WINDTRAP_CLOCK_LINUX)
  clockid = CLOCK_BOOTTIME;
#else
  clockid = CLOCK_MONOTONIC;
#endif

  if (start.tv_sec == 0)
  {
    if (clock_gettime (clockid, &start))
      WINDTRAP_CLOCK_RAISE_SYS_ERROR ("clock_gettime () failed");
  }

  if (clock_gettime (clockid, &now))
    WINDTRAP_CLOCK_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec - start.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec - start.tv_nsec));
}

/* Windows */

#elif defined(WINDTRAP_CLOCK_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static double performance_frequency;
static void set_performance_frequency(void)
{
  LARGE_INTEGER t_freq;
  if (!QueryPerformanceFrequency(&t_freq)) {
    WINDTRAP_CLOCK_RAISE_SYS_ERROR ("QueryPerformanceFrequency () failed");
  }
  performance_frequency = (1000000000.0 / t_freq.QuadPart);
}

CAMLprim value ocaml_windtrap_clock_elapsed_ns (value unit)
{
  (void) unit;
  static LARGE_INTEGER start;
  if (performance_frequency == 0.0) {
    set_performance_frequency();
  }
  if ( start.QuadPart == 0 )
  {
    if (!QueryPerformanceCounter(&start)) {
      WINDTRAP_CLOCK_RAISE_SYS_ERROR ("QueryPerformanceCounter () failed");
    }
  }
  static LARGE_INTEGER now;
  if ( !QueryPerformanceCounter(&now)) {
    WINDTRAP_CLOCK_RAISE_SYS_ERROR ("QueryPerformanceCounter () failed");
  }
  uint64_t ret = (now.QuadPart - start.QuadPart) * performance_frequency;
  return caml_copy_int64(ret);
}

/* Unsupported */

#else

#warning Windtrap.Clock: unsupported platform

CAMLprim value ocaml_windtrap_clock_elapsed_ns (value unit)
{ WINDTRAP_CLOCK_RAISE_SYS_ERROR ("unsupported platform"); }

#endif
