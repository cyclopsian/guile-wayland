/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <fcntl.h>
#include <libguile.h>
#include <stdatomic.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#define FUNC_NAME s_scm_monotonic_time
SCM_DEFINE_PUBLIC(scm_monotonic_time, "monotonic-time", 0, 0, 0,
    (void),
    "") {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) < 0)
    scm_syserror(FUNC_NAME);
  return scm_sum(scm_from_long(ts.tv_sec),
      scm_divide(scm_from_long(ts.tv_nsec), scm_from_long(1000000000)));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_mmap
SCM_DEFINE_PUBLIC(scm_mmap, "mmap", 4, 1, 0,
    (SCM length, SCM prot, SCM flags, SCM fd, SCM offset),
    "") {
  size_t c_length;
  int c_prot, c_flags, c_fd;
  off_t c_offset = 0;
  SCM_VALIDATE_ULONG_COPY(SCM_ARG1, length, c_length);
  SCM_VALIDATE_INT_COPY(SCM_ARG2, prot, c_prot);
  SCM_VALIDATE_INT_COPY(SCM_ARG3, flags, c_flags);
  SCM_VALIDATE_INT_COPY(SCM_ARG4, fd, c_fd);
  if (!scm_is_false(offset))
    SCM_VALIDATE_LONG_COPY(SCM_ARG5, offset, c_offset);

  void *addr = mmap(NULL, c_length, c_prot, c_flags, c_fd, c_offset);

  if (addr == MAP_FAILED)
    scm_syserror(FUNC_NAME);

  return scm_pointer_to_bytevector(scm_from_pointer(addr, NULL),
      length, SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_munmap
SCM_DEFINE_PUBLIC(scm_munmap, "munmap", 1, 0, 0,
    (SCM bytevector),
    "") {
  SCM_VALIDATE_BYTEVECTOR(SCM_ARG1, bytevector);
  void *addr = SCM_BYTEVECTOR_CONTENTS(bytevector);
  size_t length = SCM_BYTEVECTOR_LENGTH(bytevector);
  if (munmap(addr, length) < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static atomic_ulong shm_counter = 0;

#define FUNC_NAME s_scm_create_shm_fdes
SCM_DEFINE_PUBLIC(scm_create_shm_fdes, "create-shm-fdes", 1, 0, 0,
    (SCM length),
    "") {
  size_t c_length;
  SCM_VALIDATE_ULONG_COPY(SCM_ARG1, length, c_length);

  char path[64];
  snprintf(path, sizeof(path), "/shm-%lu", atomic_fetch_add(&shm_counter, 1));

  int fd = shm_open(path, O_RDWR | O_CREAT | O_EXCL, S_IWUSR | S_IRUSR);
  if (fd < 0)
    scm_syserror(FUNC_NAME);
  if (shm_unlink(FUNC_NAME) < 0 || ftruncate(fd, c_length) < 0) {
    close(fd);
    scm_syserror(FUNC_NAME);
  }
  return scm_from_int(fd);
}
#undef FUNC_NAME

#define SCM_PUBLIC_ENUM(name) \
  do { \
    scm_c_define(#name, scm_from_uint(name)); \
    scm_c_export(#name, NULL); \
  } while (0)

static void register_wayland_utils(void *data) {
  SCM_PUBLIC_ENUM(PROT_EXEC);
  SCM_PUBLIC_ENUM(PROT_READ);
  SCM_PUBLIC_ENUM(PROT_WRITE);
  SCM_PUBLIC_ENUM(PROT_NONE);

  SCM_PUBLIC_ENUM(MAP_SHARED);
  SCM_PUBLIC_ENUM(MAP_PRIVATE);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-utils.x"
#endif
}

void scm_i_init_wayland_utils(void) {
  scm_c_define_module("wayland utils",
      register_wayland_utils, NULL);
}
