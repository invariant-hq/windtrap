/*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Preloaded into a child, /proc/self/exe cannot be read. The OCaml runtime
   then takes Sys.executable_name from argv[0] as given, relative to the
   directory the process started in, as it does outside Linux and macOS. */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

ssize_t readlink (const char *restrict path, char *restrict buf, size_t size)
{
  static ssize_t (*next) (const char *restrict, char *restrict, size_t);

  if (strcmp (path, "/proc/self/exe") == 0)
  {
    errno = ENOENT;
    return -1;
  }
  if (next == NULL)
    next = (ssize_t (*) (const char *restrict, char *restrict, size_t))
      dlsym (RTLD_NEXT, "readlink");
  return next (path, buf, size);
}
