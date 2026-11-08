/*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* The primitives Os needs from C: a monotonic clock (derived from mtime;
   only elapsed_ns and the spin clock are exposed), unsetenv, the
   unbinding half Unix lacks (binding to "" is not unbinding:
   Sys.getenv_opt then answers Some ""), and two readings of resident
   memory. On Windows unsetenv is the empty assignment _putenv documents
   as deletion. Os.setenv has already rejected the names POSIX rejects, so
   a failure here is an allocation failure.

   On Darwin and Linux the clock is the one that goes on while the machine
   is suspended (mach_continuous_time, CLOCK_BOOTTIME). */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Monotonic clock */

#define WINDTRAP_CLOCK_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Windtrap.Os: " ERR)); }    \
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

/* The scale is set by the first call of elapsed_ns, as Os initializes. */
static intnat ocaml_windtrap_clock_raw_ns (void)
{
  if (scale.denom == 0) return 0;
  return (intnat) ((mach_continuous_time () * scale.numer) / scale.denom);
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

static intnat ocaml_windtrap_clock_raw_ns (void)
{
  struct timespec now;
#if defined(WINDTRAP_CLOCK_LINUX)
  if (clock_gettime (CLOCK_BOOTTIME, &now)) return 0;
#else
  if (clock_gettime (CLOCK_MONOTONIC, &now)) return 0;
#endif
  return (intnat) now.tv_sec * 1000000000 + (intnat) now.tv_nsec;
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

/* The frequency is set by the first call of elapsed_ns, as Os
   initializes. */
static intnat ocaml_windtrap_clock_raw_ns (void)
{
  LARGE_INTEGER now;
  if (performance_frequency == 0.0 || !QueryPerformanceCounter(&now))
    return 0;
  return (intnat) (now.QuadPart * performance_frequency);
}

/* Unsupported */

#else

#warning Windtrap.Os: unsupported platform

CAMLprim value ocaml_windtrap_clock_elapsed_ns (value unit)
{ WINDTRAP_CLOCK_RAISE_SYS_ERROR ("unsupported platform"); }

static intnat ocaml_windtrap_clock_raw_ns (void) { return 0; }

#endif

/* The clock of a bounded spin, from an arbitrary origin. It is noalloc,
   so it neither allocates nor raises: it is 0 where the clock fails, which
   a spin takes as its end. */

CAMLprim intnat ocaml_windtrap_clock_spin_ns (value unit)
{
  (void) unit;
  return ocaml_windtrap_clock_raw_ns ();
}

CAMLprim value ocaml_windtrap_clock_spin_ns_byte (value unit)
{
  return Val_long (ocaml_windtrap_clock_spin_ns (unit));
}

/* Environment */

CAMLprim value ocaml_windtrap_unsetenv (value name)
{
  CAMLparam1 (name);
  const char *n = String_val (name);

#if defined(_WIN32)
  size_t len = strlen (n);
  char *entry = (char *) malloc (len + 2);
  int failed;
  if (entry == NULL)
    caml_raise_sys_error (caml_copy_string ("Windtrap.Os: out of memory"));
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
    caml_raise_sys_error (caml_copy_string ("Windtrap.Os: unsetenv () failed"));

  CAMLreturn (Val_unit);
}

/* Resident memory, in bytes, 0 where it cannot be read. A process's own
   resident size comes from /proc/<pid>/statm on Linux (its second field,
   in pages) and from proc_pid_rusage on macOS. getrusage's ru_maxrss is in
   KiB on Linux and the BSDs, and in bytes on macOS. */

#if defined(__APPLE__) && defined(__MACH__)
#include <libproc.h>
#include <sys/resource.h>
#elif !defined(_WIN32)
#include <stdio.h>
#include <sys/resource.h>
#include <unistd.h>
#endif

CAMLprim value ocaml_windtrap_resident (value pid)
{
#if defined(__APPLE__) && defined(__MACH__)
  struct rusage_info_v0 info;
  if (proc_pid_rusage (Int_val (pid), RUSAGE_INFO_V0,
                       (rusage_info_t *) &info) != 0)
    return Val_long (0);
  return Val_long ((intnat) info.ri_resident_size);
#elif defined(__linux__)
  char path[64];
  long size, resident;
  FILE *statm;
  snprintf (path, sizeof path, "/proc/%d/statm", Int_val (pid));
  statm = fopen (path, "r");
  if (statm == NULL) return Val_long (0);
  if (fscanf (statm, "%ld %ld", &size, &resident) != 2) resident = 0;
  fclose (statm);
  return Val_long ((intnat) resident * (intnat) sysconf (_SC_PAGESIZE));
#else
  (void) pid;
  return Val_long (0);
#endif
}

CAMLprim value ocaml_windtrap_resident_peak (value unit)
{
  (void) unit;
#if defined(_WIN32)
  return Val_long (0);
#else
  struct rusage usage;
  if (getrusage (RUSAGE_SELF, &usage) != 0) return Val_long (0);
#if defined(__APPLE__) && defined(__MACH__)
  return Val_long ((intnat) usage.ru_maxrss);
#else
  return Val_long ((intnat) usage.ru_maxrss * 1024);
#endif
#endif
}
