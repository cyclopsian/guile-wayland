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
#include <wayland-util.h>

#include "guile-wayland-util.h"

SCM scm_wl_interface_type;

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

static long signature_arg_count(const char *signature) {
  long count = 0;
  for ( ; *signature; signature++) {
    if (*signature != '?')
      count++;
  }
  return count;
}

#define FUNC_NAME subr
static void unpack_message(SCM message, struct wl_message *out,
    const char *subr) {
  long message_len;
  SCM_VALIDATE_LIST_COPYLEN(SCM_ARGn, message, message_len);
  SCM_ASSERT_TYPE(message_len == 3, message, SCM_ARGn, FUNC_NAME,
      "list of length 3");
  SCM name      = SCM_CAR(message);
  SCM signature = SCM_CADR(message);
  SCM types     = SCM_CADDR(message);
  long type_count;

  SCM_VALIDATE_STRING(SCM_ARGn, name);
  SCM_VALIDATE_STRING(SCM_ARGn, signature);
  SCM_VALIDATE_LIST_COPYLEN(SCM_ARGn, types, type_count);

  out->name      = scm_to_utf8_string(name);
  out->signature = scm_to_utf8_string(signature);
  out->types     = scm_calloc(type_count * sizeof(struct wl_interface *));

  SCM_ASSERT_TYPE(type_count == signature_arg_count(out->signature),
      types, SCM_ARGn, FUNC_NAME, "type list with same arg count as signature");

  long i = 0;
  for (const char *signature = out->signature; *signature; signature++) {
    if (*signature == '?')
      continue;
    SCM type = SCM_CAR(types);
    if ((*signature == 'o' || *signature == 'n') && !scm_is_false(type)) {
      scm_assert_foreign_object_type(scm_wl_interface_type, type);
      out->types[i] = scm_foreign_object_ref(type, 0);
    } else {
      SCM_ASSERT_TYPE(scm_is_false(type), type, SCM_ARGn, FUNC_NAME, "#f");
    }
    i++;
    types = SCM_CDR(types);
  }
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_make_wl_interface
SCM_DEFINE_PUBLIC(scm_make_wl_interface, "make-wl-interface", 0, 0, 0,
    (void),
    "") {
  struct wl_interface *interface = scm_calloc(sizeof(struct wl_interface));
  return scm_make_foreign_object_1(scm_wl_interface_type, interface);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_interface_set
SCM_DEFINE_PUBLIC(scm_wl_interface_set, "wl-interface-set", 5, 0, 0,
    (SCM interface, SCM name, SCM version, SCM methods, SCM events),
    "") {

  struct wl_interface *i_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG1, interface, i_interface);
  if (i_interface->name != NULL)
    scm_misc_error(FUNC_NAME, "interface already set: ~a",
        scm_list_1(interface));

  SCM_VALIDATE_STRING(SCM_ARG2, name);
  SCM_VALIDATE_INT_COPY(SCM_ARG3, version, i_interface->version);
  if (!scm_is_false(methods))
    SCM_VALIDATE_LIST_COPYLEN(SCM_ARG4, methods, i_interface->method_count);
  if (!scm_is_false(events))
    SCM_VALIDATE_LIST_COPYLEN(SCM_ARG5, events, i_interface->event_count);

  struct wl_message *i_methods = NULL;
  if (i_interface->method_count > 0)
    i_methods = scm_calloc(i_interface->method_count * sizeof(struct wl_message));
  for (int i = 0; i < i_interface->method_count; i++) {
    unpack_message(SCM_CAR(methods), &i_methods[i], FUNC_NAME);
    methods = SCM_CDR(methods);
  }

  struct wl_message *i_events = NULL;
  if (i_interface->event_count > 0)
    i_events = scm_calloc(i_interface->event_count * sizeof(struct wl_message));
  for (int i = 0; i < i_interface->event_count; i++) {
    unpack_message(SCM_CAR(events), &i_events[i], FUNC_NAME);
    events = SCM_CDR(events);
  }

  i_interface->name = scm_to_utf8_string(name);
  i_interface->methods = i_methods;
  i_interface->events = i_events;

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_interface_name
SCM_DEFINE_PUBLIC(scm_wl_interface_name, "wl-interface-name", 1, 0, 0,
    (SCM interface),
    "") {
  struct wl_interface *i_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG1, interface, i_interface);
  return scm_from_utf8_string(i_interface->name);
}
#undef FUNC_NAME

#define SCM_PUBLIC_ENUM(name) \
  do { \
    scm_c_define(#name, scm_from_uint(name)); \
    scm_c_export(#name, NULL); \
  } while (0)

static void register_wayland_util(void *data) {
  static const char s_scm_wl_interface[] = "<wl-interface>";
  scm_wl_interface_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_interface),
      scm_list_1(scm_from_utf8_symbol("interface")),
      NULL);
  scm_c_define(s_scm_wl_interface, scm_wl_interface_type);
  scm_c_export(s_scm_wl_interface, NULL);

  SCM_PUBLIC_ENUM(PROT_EXEC);
  SCM_PUBLIC_ENUM(PROT_READ);
  SCM_PUBLIC_ENUM(PROT_WRITE);
  SCM_PUBLIC_ENUM(PROT_NONE);

  SCM_PUBLIC_ENUM(MAP_SHARED);
  SCM_PUBLIC_ENUM(MAP_PRIVATE);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-util.x"
#endif
}

void scm_i_init_wayland_util(void) {
  scm_c_define_module("wayland util",
      register_wayland_util, NULL);
}
