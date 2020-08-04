/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <fcntl.h>
#include <libguile.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <wayland-util.h>

#include "guile-wayland-client.h"
#include "guile-wayland-server.h"
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
  if (!SCM_UNBNDP(offset) && !scm_is_false(offset))
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

#define FUNC_NAME s_scm_create_shm_fdes
SCM_DEFINE_PUBLIC(scm_create_shm_fdes, "create-shm-fdes", 1, 0, 0,
    (SCM length),
    "") {
  size_t c_length;
  SCM_VALIDATE_ULONG_COPY(SCM_ARG1, length, c_length);

  int fd = memfd_create("guile-wayland", MFD_CLOEXEC | MFD_ALLOW_SEALING);

  if (fd < 0)
    scm_syserror(FUNC_NAME);

  if (ftruncate(fd, c_length) < 0) {
    close(fd);
    scm_syserror(FUNC_NAME);
  }
  return scm_from_int(fd);
}
#undef FUNC_NAME

long scm_i_signature_arg_count(const char *signature) {
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

  SCM_ASSERT_TYPE(type_count == scm_i_signature_arg_count(out->signature),
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

#define FUNC_NAME subr
union wl_argument *scm_i_pack_wl_arguments(long pos, const char *subr,
    const struct wl_message *request, bool server, SCM rest) {
  long length = scm_ilength(rest);
  union wl_argument *args = scm_malloc(length * sizeof(union wl_argument));
  scm_dynwind_free(args);

  const char *signature = request->signature;
  bool nullable = false;
  long i = 0;
  for (; *signature; signature++) {
    if (SCM_UNLIKELY(i >= length || scm_is_eq(rest, SCM_EOL))) {
      scm_error(scm_args_number_key, FUNC_NAME,
          "Wrong number of arguments to ~A (expecting ~A): ~A",
          scm_list_3(
            scm_from_utf8_string(request->name),
            scm_from_size_t(
              scm_i_signature_arg_count(request->signature)),
            scm_from_long(length)),
          SCM_BOOL_F);
    }
    SCM arg = SCM_CAR(rest);
    switch (*signature) {
    case '?':
      nullable = true;
      continue;
    case 'i':
      SCM_VALIDATE_INT_COPY(pos + i, arg, args[i].i);
      break;
    case 'u':
      SCM_VALIDATE_UINT_COPY(pos + i, arg, args[i].u);
      break;
    case 'f':
      {
        double d;
        SCM_VALIDATE_DOUBLE_COPY(pos + i, arg, d);
        args[i].f = wl_fixed_from_double(d);
      }
      break;
    case 's':
      SCM_ASSERT_TYPE(nullable || !scm_is_false(arg), arg, pos + i,
          FUNC_NAME, "string");
      if (scm_is_false(arg)) {
        args[i].s = NULL;
      } else {
        SCM_VALIDATE_STRING(pos + i, arg);
        args[i].s = scm_to_utf8_string(arg);
        scm_dynwind_free((char *) args[i].s);
      }
      break;
    case 'o':
      SCM_ASSERT_TYPE(nullable || !scm_is_false(arg), arg, pos + i,
          FUNC_NAME, "wl-proxy");
      if (scm_is_false(arg)) {
        args[i].o = NULL;
      } else if (server) {
        SCM_VALIDATE_WL_RESOURCE_COPY(pos + i, arg, args[i].o);
      } else {
        SCM_VALIDATE_WL_PROXY_COPY(pos + i, arg, args[i].o);
      }
      break;
    case 'n':
      args[i].o = NULL;
      break;
    case 'a':
      SCM_ASSERT_TYPE(nullable || !scm_is_false(arg), arg, pos + i,
          FUNC_NAME, "array");
      if (scm_is_false(arg)) {
        args[i].a = NULL;
      } else {
        SCM vec = scm_any_to_u32vector(arg);
        scm_t_array_handle *handle = scm_malloc(sizeof(scm_t_array_handle));
        scm_dynwind_unwind_handler(
            (void (*)(void *)) scm_array_handle_release, handle,
            SCM_F_WIND_EXPLICITLY);
        struct wl_array *array = scm_malloc(sizeof(struct wl_array));
        scm_dynwind_free(array);
        array->data = (void *) scm_u32vector_elements(
            vec, handle, &array->size, NULL);
        array->alloc = array->size;
        args[i].a = array;
      }
      break;
    case 'h':
      SCM_VALIDATE_INT_COPY(pos + i, arg, args[i].h);
      break;
    default:
      break;
    }
    nullable = false;
    i++;
    rest = SCM_CDR(rest);
  }
  return args;
}
#undef FUNC_NAME

SCM *scm_i_unpack_wl_arguments(const struct wl_message *message,
    long argc, const union wl_argument *args, bool server) {
  if (!argc)
    return NULL;
  SCM *argv = scm_calloc(argc * sizeof(SCM));
  scm_dynwind_free(argv);
  long i = 0;
  for (const char *signature = message->signature; *signature; signature++) {
    if (i > argc)
      break;
    switch (*signature) {
    case '?':
      continue;
    case 'i':
      argv[i] = scm_from_int32(args[i].i);
      break;
    case 'u':
      argv[i] = scm_from_uint32(args[i].u);
      break;
    case 'f':
      argv[i] = scm_from_double(wl_fixed_to_double(args[i].f));
      break;
    case 's':
      if (args[i].s) {
        argv[i] = scm_from_utf8_string(args[i].s);
      } else {
        argv[i] = SCM_BOOL_F;
      }
      break;
    case 'o':
      if (args[i].o) {
        if (server) {
          argv[i] = scm_from_wl_resource((struct wl_resource *) args[i].o);
        } else {
          argv[i] = scm_from_wl_proxy((struct wl_proxy *) args[i].o);
        }
      } else {
        argv[i] = SCM_BOOL_F;
      }
      break;
    case 'n':
      if (server) {
        argv[i] = scm_from_uint32(args[i].n);
      } else if (args[i].o) {
        argv[i] = scm_from_wl_proxy((struct wl_proxy *) args[i].o);
      } else {
        argv[i] = SCM_BOOL_F;
      }
      break;
    case 'a':
      {
        if (args[i].a) {
          size_t size = args[i].a->size;
          if (size) {
            void *array = scm_malloc(size);
            memcpy(array, args[i].a->data, size);
            argv[1] = scm_take_u32vector(array, size);
          } else {
            argv[1] = scm_list_to_u32vector(SCM_EOL);
          }
        } else {
        argv[i] = SCM_BOOL_F;
        }
      }
      break;
    case 'h':
      argv[i] = scm_from_int(args[i].h);
      break;
    default:
      break;
    }
    i++;
  }
  for (; i < argc; i++) {
    argv[i] = SCM_UNDEFINED;
  }
  return argv;
}

#define FUNC_NAME subr
void scm_i_validate_dispatch_list(long pos, const char *subr,
    const char *iface_name, int message_count,
    const struct wl_message *messages, int extra, SCM rest) {
  long count = scm_ilength(rest);
  if (SCM_UNLIKELY(count != message_count + extra)) {
    scm_error(scm_args_number_key, subr,
        "Wrong number of arguments to ~A (expecting ~A): ~A",
        scm_list_3(
          scm_from_utf8_string(iface_name),
          scm_from_int(message_count),
          scm_from_long(count)),
        SCM_BOOL_F);
  }
  int i = 0;
  SCM iter = rest;
  for (long i = 0; i < message_count; i++) {
    SCM proc = SCM_CAR(iter);
    if (!scm_is_false(proc)) {
      SCM_VALIDATE_PROC(pos + i, proc);
      long argc = scm_i_signature_arg_count(messages[i].signature);
      SCM arities = scm_procedure_minimum_arity(proc);
      if (!scm_is_false(arities)) {
        int req = scm_to_int(SCM_CAR(arities));
        int opt = scm_to_int(SCM_CADR(arities));
        bool restc = scm_to_bool(SCM_CADDR(arities));
        if (SCM_UNLIKELY(req > argc || (!restc && req + opt < argc))) {
          scm_error(scm_arg_type_key,
              subr,
              "Expected procedure of arity ~A in position ~A "
              "(~A.~A, opcode ~A)",
              scm_list_5(
                scm_from_long(argc),
                scm_from_int(pos + i),
                scm_from_utf8_string(iface_name),
                scm_from_utf8_string(messages[i].name),
                scm_from_uint32(i)),
              scm_list_1(proc));
        }
      }
    }
    iter = SCM_CDR(iter);
  }
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

void scm_init_wayland_util(void) {
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

