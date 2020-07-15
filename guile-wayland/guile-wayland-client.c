/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#define _GNU_SOURCE
#include <errno.h>
#include <libguile.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <wayland-client-core.h>

#include "guile-wayland-client.h"
#include "guile-wayland-cursor.h"
#include "guile-wayland-egl.h"
#include "guile-wayland-utils.h"

SCM scm_wl_proxy_class_type;
SCM scm_wl_event_queue_type;
SCM scm_wl_proxy_type;
SCM scm_wl_interface_type;

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

#define SCM_VALIDATE_WL_EVENT_QUEUE_COPY(pos, q, queue) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(q, scm_wl_event_queue_type), \
        q, pos, FUNC_NAME, "wl-event-queue"); \
    queue = scm_foreign_object_ref(q, 0); \
    SCM_ASSERT_TYPE(queue, q, pos, FUNC_NAME, "non-null wl-event-queue"); \
  } while (0)

#define SCM_VALIDATE_WL_INTERFACE_COPY(pos, i, interface) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(i, scm_wl_interface_type), \
        i, pos, FUNC_NAME, "wl-interface"); \
    interface = scm_foreign_object_ref(i, 0); \
    SCM_ASSERT_TYPE(interface, i, pos, FUNC_NAME, "non-null wl-interface"); \
  } while (0)

#define INTERFACE_EQ(_a, _b) \
  ((_a) == (_b) || strcmp((_a)->name, (_b)->name) == 0)

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

SCM scm_c_make_wl_proxy(struct wl_proxy *proxy,
    const struct wl_interface *interface) {
  return scm_make_foreign_object_2(
      scm_wl_proxy_type, proxy, (void *) interface);
}

#define FUNC_NAME s_scm_wl_event_queue_destroy
SCM_DEFINE_PUBLIC(scm_wl_event_queue_destroy, "wl-event-queue-destroy", 1, 0, 0,
    (SCM queue),
    "") {
  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG1, queue, i_queue);
  wl_event_queue_destroy(i_queue);
  scm_foreign_object_set_x(queue, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME subr
static union wl_argument *unpack_marshal_args(long pos, const char *subr,
    const struct wl_message *request, SCM rest) {
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
              signature_arg_count(request->signature)),
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
      } else {
        struct wl_interface *arg_interface;
        SCM_VALIDATE_WL_PROXY_COPY(pos + i, arg, args[i].o, arg_interface);
        SCM_ASSERT_TYPE(INTERFACE_EQ(request->types[i], arg_interface),
            arg, pos + i, FUNC_NAME, arg_interface->name);
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

#define VALIDATE_MARSHAL_ARGS(proxy, i_proxy, i_interface, opcode, i_opcode) \
  do { \
    SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, i_interface); \
    SCM_VALIDATE_UINT_COPY(SCM_ARG2, opcode, i_opcode); \
    if (SCM_UNLIKELY(i_opcode >= (uint32_t) i_interface->method_count)) { \
      scm_error(scm_arg_type_key, \
          FUNC_NAME, \
          "Invalid opcode ~A on ~A", \
          scm_list_2( \
            scm_from_uint32(i_opcode), \
            scm_from_utf8_string(i_interface->name)), \
            scm_list_1(opcode)); \
    } \
  } while (0)

#define FUNC_NAME s_scm_wl_proxy_marshal
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal, "wl-proxy-marshal", 2, 0, 1,
    (SCM proxy, SCM opcode, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t             i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy, i_interface, opcode, i_opcode);

  scm_dynwind_begin(0);
  union wl_argument *args = unpack_marshal_args(SCM_ARG3,
      FUNC_NAME, &i_interface->methods[i_opcode], rest);
  wl_proxy_marshal_array(i_proxy, i_opcode, args);
  scm_dynwind_end();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_create_wrapper
SCM_DEFINE_PUBLIC(scm_wl_proxy_create_wrapper,
    "wl-proxy-create-wrapper", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  void *wrapper = wl_proxy_create_wrapper(i_proxy);
  if (!wrapper)
    scm_report_out_of_memory();
  return scm_c_make_wl_proxy(wrapper, interface);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_wrapper_destroy
SCM_DEFINE_PUBLIC(scm_wl_proxy_wrapper_destroy,
    "wl-proxy-wrapper-destroy", 1, 0, 0,
    (SCM wrapper),
    "") {
  void                *i_wrapper;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, wrapper, i_wrapper, interface);
  wl_proxy_wrapper_destroy(i_wrapper);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_marshal_constructor
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal_constructor,
    "wl-proxy-marshal-constructor", 3, 0, 1,
    (SCM proxy, SCM opcode, SCM interface, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t             i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy, i_interface, opcode, i_opcode);

  struct wl_interface *ret_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG3, interface, ret_interface);

  scm_dynwind_begin(0);
  union wl_argument *args = unpack_marshal_args(SCM_ARG4,
      FUNC_NAME, &i_interface->methods[i_opcode], rest);
  struct wl_proxy *ret
   = wl_proxy_marshal_array_constructor(i_proxy, i_opcode, args, ret_interface);
  scm_dynwind_end();
  return scm_c_make_wl_proxy(ret, ret_interface);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_marshal_constructor_versioned
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal_constructor_versioned,
    "wl-proxy-marshal-constructor-versioned", 4, 0, 1,
    (SCM proxy, SCM opcode, SCM interface, SCM version, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy, i_interface, opcode, i_opcode);

  struct wl_interface *ret_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG3, interface, ret_interface);

  uint32_t ret_version;
  SCM_VALIDATE_UINT_COPY(SCM_ARG4, version, ret_version);

  scm_dynwind_begin(0);
  union wl_argument *args = unpack_marshal_args(SCM_ARG5,
      FUNC_NAME, &i_interface->methods[i_opcode], rest);
  struct wl_proxy *ret
   = wl_proxy_marshal_array_constructor_versioned(
       i_proxy, i_opcode, args, ret_interface, ret_version);
  scm_dynwind_end();
  return scm_c_make_wl_proxy(ret, ret_interface);
}
#undef FUNC_NAME

struct dispatch_data {
  const void *impl;
  struct wl_proxy *proxy;
  uint32_t opcode;
  const struct wl_message *message;
  union wl_argument *args;
  int ret;
};

static SCM proxy_dispatch_body(void *data) {
  struct dispatch_data *d = data;
  SCM procs = scm_pointer_to_scm(scm_from_pointer((void *) d->impl, NULL));
  SCM proc = scm_c_array_ref_1(procs, d->opcode);
  if (scm_is_false(proc))
    return SCM_UNDEFINED;

  scm_dynwind_begin(0);
  long argc = signature_arg_count(d->message->signature);
  SCM *argv = scm_malloc(argc * sizeof(SCM));
  scm_dynwind_free(argv);
  long i = 0;
  for (const char *signature = d->message->signature; *signature; signature++) {
    switch (*signature) {
    case 'i':
      argv[i] = scm_from_int32(d->args[i].i);
      break;
    case 'u':
      argv[i] = scm_from_uint32(d->args[i].u);
      break;
    case 'f':
      argv[i] = scm_from_double(wl_fixed_to_double(d->args[i].f));
      break;
    case 's':
      if (d->args[i].s) {
        argv[i] = scm_from_utf8_string(d->args[i].s);
      } else {
        argv[i] = SCM_BOOL_F;
      }
      break;
    case 'o':
    case 'n':
      if (d->args[i].o) {
        argv[i] = scm_c_make_wl_proxy(
            (struct wl_proxy *) d->args[i].o, d->message->types[i]);
      } else {
        argv[i] = SCM_BOOL_F;
      }
      break;
    case 'a':
      {
        if (d->args[i].a) {
          size_t size = d->args[i].a->size;
          if (size) {
            void *array = scm_malloc(size);
            memcpy(array, d->args[i].a->data, size);
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
      argv[i] = scm_from_int(d->args[i].h);
      break;
    default:
      break;
    }
    i++;
  }
  scm_call_n(proc, argv, argc);
  scm_dynwind_end();
  return SCM_UNDEFINED;
}

static SCM proxy_dispatch_handler(void *data, SCM tag, SCM throw_args) {
  struct dispatch_data *d = data;
  d->ret = -1;
  scm_throw(tag, throw_args);
  return SCM_UNDEFINED;
}

static void *do_proxy_dispatch(void *data) {
  scm_internal_catch(SCM_BOOL_T, proxy_dispatch_body, data,
      proxy_dispatch_handler, NULL);
  return NULL;
}

static int proxy_dispatch(const void *impl, void *proxy, uint32_t opcode, const
    struct wl_message *message, union wl_argument *args) {
  struct dispatch_data data = { impl, proxy, opcode, message, args, 0 };
  scm_with_guile(do_proxy_dispatch, &data);
  return data.ret;
}

#define FUNC_NAME s_scm_wl_proxy_add_listener
SCM_DEFINE_PUBLIC(scm_wl_proxy_add_listener, "wl-proxy-add-listener", 1, 0, 1,
    (SCM proxy, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);

  long count = scm_ilength(rest);
  if (SCM_UNLIKELY(count != interface->event_count)) {
    scm_error(scm_args_number_key, FUNC_NAME,
        "Wrong number of arguments to ~A (expecting ~A): ~A",
        scm_list_3(
          scm_from_utf8_string(interface->name),
          scm_from_int(interface->event_count),
          scm_from_long(count)),
        SCM_BOOL_F);
  }
  long i = 0;
  for (SCM iter = rest; !scm_is_eq(iter, SCM_EOL); iter = SCM_CDR(iter)) {
    SCM proc = SCM_CAR(iter);
    if (!scm_is_false(proc)) {
      SCM_VALIDATE_PROC(SCM_ARG2 + i, proc);
      long argc = signature_arg_count(interface->events[i].signature);
      SCM arities = scm_procedure_minimum_arity(proc);
      if (!scm_is_false(arities)) {
        int req = scm_to_int(SCM_CAR(arities));
        int opt = scm_to_int(SCM_CADR(arities));
        int restc = scm_to_int(SCM_CADDR(arities));
        if (SCM_UNLIKELY(req > argc || (!restc && req + opt < argc))) {
          scm_error(scm_arg_type_key,
              FUNC_NAME,
              "Expected procedure of arity ~A in position ~A "
              "(~A.~A, opcode ~A)",
              scm_list_5(
                scm_from_long(argc),
                scm_from_int(SCM_ARG2 + i),
                scm_from_utf8_string(interface->name),
                scm_from_utf8_string(interface->events[i].name),
                scm_from_uint32(i)),
              scm_list_1(proc));
        }
      }
    }
    i++;
  }
  SCM array = scm_list_to_array(scm_from_int(1), rest);
  void *impl = scm_to_pointer(scm_scm_to_pointer(array));
  wl_proxy_add_dispatcher(i_proxy, proxy_dispatch, impl, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_destroy
SCM_DEFINE_PUBLIC(scm_wl_proxy_destroy, "wl-proxy-destroy", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  wl_proxy_destroy(i_proxy);
  scm_foreign_object_set_x(proxy, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_version
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_version, "wl-proxy-get-version", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  return scm_from_uint32(wl_proxy_get_version(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_id
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_id, "wl-proxy-get-id", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  return scm_from_uint32(wl_proxy_get_id(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_class
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_class, "wl-proxy-get-class", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  return scm_from_utf8_string(wl_proxy_get_class(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_symbol
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_symbol, "wl-proxy-get-symbol", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);
  char *symbol = strdup(wl_proxy_get_class(i_proxy));
  if (symbol == NULL)
    scm_report_out_of_memory();
  for (char *s = symbol; *s; s++) {
    if (*s == '_')
      *s = '-';
  }
  return scm_from_utf8_symbol(symbol);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_set_queue
SCM_DEFINE_PUBLIC(scm_wl_proxy_set_queue, "wl-proxy-set-queue", 2, 0, 0,
    (SCM proxy, SCM queue),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, interface);

  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  wl_proxy_set_queue(i_proxy, i_queue);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_assert_type
SCM_DEFINE_PUBLIC(scm_wl_proxy_assert_type, "wl-proxy-assert-type", 2, 0, 0,
    (SCM proxy, SCM interface),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy, i_interface);
  struct wl_interface *c_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG2, interface, c_interface);

  SCM_ASSERT_TYPE(INTERFACE_EQ(i_interface, c_interface),
      proxy, SCM_ARG1, FUNC_NAME, c_interface->name);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_move
SCM_DEFINE_PUBLIC(scm_wl_proxy_move, "wl-proxy-move", 2, 0, 0,
    (SCM src, SCM dst),
    "") {
  struct wl_proxy     *src_proxy;
  struct wl_interface *src_interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, src, src_proxy, src_interface);
  SCM_ASSERT_TYPE(SCM_IS_A_P(dst, scm_wl_proxy_type),
      dst, SCM_ARG2, FUNC_NAME, "wl-proxy");

  scm_foreign_object_set_x(dst, 0, src_proxy);
  scm_foreign_object_set_x(dst, 1, src_interface);
  scm_foreign_object_set_x(src, 0, NULL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static inline struct wl_interface *get_wl_display_interface(void) {
  SCM client_proto = scm_c_resolve_module("wayland client protocol");
  SCM interface = scm_c_module_lookup(client_proto, "wl-display-interface");
  return scm_foreign_object_ref(scm_variable_ref(interface), 0);
}

#define FUNC_NAME s_scm_wl_display_connect
SCM_DEFINE_PUBLIC(scm_wl_display_connect, "wl-display-connect", 0, 1, 0,
    (SCM name),
    "") {
  scm_dynwind_begin(0);
  char *i_name = NULL;
  if (!SCM_UNBNDP(name)) {
    SCM_VALIDATE_STRING(SCM_ARG1, name);
    i_name = scm_to_utf8_string(name);
    scm_dynwind_free(i_name);
  }
  struct wl_display *display = wl_display_connect(i_name);
  if (!display)
    scm_syserror(FUNC_NAME);
  scm_dynwind_end();
  return scm_c_make_wl_proxy((struct wl_proxy *) display,
      get_wl_display_interface());
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_connect_to_fd
SCM_DEFINE_PUBLIC(scm_wl_display_connect_to_fd, "wl-display-connect-to-fd",
    1, 0, 0,
    (SCM fd),
    "") {
  int i_fd;
  SCM_VALIDATE_INT_COPY(SCM_ARG1, fd, i_fd);
  struct wl_display *display = wl_display_connect_to_fd(i_fd);
  if (!display)
    scm_syserror(FUNC_NAME);
  return scm_c_make_wl_proxy((struct wl_proxy *) display,
      get_wl_display_interface());
}
#undef FUNC_NAME

#define VALIDATE_WL_DISPLAY_COPY(pos, display, i_display) \
  do { \
    struct wl_interface *interface; \
    SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display, interface); \
    SCM_ASSERT_TYPE(INTERFACE_EQ(interface, get_wl_display_interface()), \
        display, pos, FUNC_NAME, interface->name); \
  } while (0);

#define FUNC_NAME s_scm_wl_display_disconnect
SCM_DEFINE_PUBLIC(scm_wl_display_disconnect, "wl-display-disconnect", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  wl_display_disconnect(i_display);
  scm_foreign_object_set_x(display, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_fd
SCM_DEFINE_PUBLIC(scm_wl_display_get_fd, "wl-display-get-fd", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  return scm_from_int(wl_display_get_fd(i_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_dispatch
SCM_DEFINE_PUBLIC(scm_wl_display_dispatch, "wl-display-dispatch", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int events = wl_display_dispatch(i_display);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_dispatch_queue
SCM_DEFINE_PUBLIC(scm_wl_display_dispatch_queue,
    "wl-display-dispatch-queue", 2, 0, 0,
    (SCM display, SCM queue),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  int events = wl_display_dispatch_queue(i_display, i_queue);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_dispatch_queue_pending
SCM_DEFINE_PUBLIC(scm_wl_display_dispatch_queue_pending,
    "wl-display-dispatch-queue-pending", 2, 0, 0,
    (SCM display, SCM queue),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  int events = wl_display_dispatch_queue_pending(i_display, i_queue);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_dispatch_pending
SCM_DEFINE_PUBLIC(scm_wl_display_dispatch_pending,
    "wl-display-dispatch-pending", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int events = wl_display_dispatch_pending(i_display);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_error
SCM_DEFINE_PUBLIC(scm_wl_display_get_error,
    "wl-display-get-error", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  return scm_from_int(wl_display_get_error(i_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_protocol_error
SCM_DEFINE_PUBLIC(scm_wl_display_get_protocol_error,
    "wl-display-get-protocol-error", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  const struct wl_interface *interface;
  uint32_t id;
  uint32_t code = wl_display_get_protocol_error(i_display, &interface, &id);
  return scm_list_3(
      scm_from_uint32(code),
      scm_make_foreign_object_1(scm_wl_interface_type, (void *) interface),
      scm_from_uint32(id));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_flush
SCM_DEFINE_PUBLIC(scm_wl_display_flush, "wl-display-flush", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int bytes = wl_display_flush(i_display);
  if (bytes == -1 && errno != EAGAIN)
    scm_syserror(FUNC_NAME);
  return scm_from_int(bytes);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_roundtrip_queue
SCM_DEFINE_PUBLIC(scm_wl_display_roundtrip_queue,
    "wl-display-roundtrip-queue", 2, 0, 0,
    (SCM display, SCM queue),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  int events = wl_display_roundtrip_queue(i_display, i_queue);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_roundtrip
SCM_DEFINE_PUBLIC(scm_wl_display_roundtrip, "wl-display-roundtrip", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int events = wl_display_roundtrip(i_display);
  if (events == -1)
    scm_syserror(FUNC_NAME);
  return scm_from_int(events);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_create_queue
SCM_DEFINE_PUBLIC(scm_wl_display_create_queue,
    "wl-display-create-queue", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  struct wl_event_queue *queue = wl_display_create_queue(i_display);
  if (!queue)
    scm_report_out_of_memory();
  return scm_make_foreign_object_1(scm_wl_event_queue_type, queue);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_prepare_read_queue
SCM_DEFINE_PUBLIC(scm_wl_display_prepare_read_queue,
    "wl-display-prepare-read-queue", 2, 0, 0,
    (SCM display, SCM queue),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  int status = wl_display_prepare_read_queue(i_display, i_queue);
  if (status == -1 && errno != EAGAIN)
    scm_syserror(FUNC_NAME);
  return scm_from_bool(status == 0);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_prepare_read
SCM_DEFINE_PUBLIC(scm_wl_display_prepare_read,
    "wl-display-prepare-read", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int status = wl_display_prepare_read(i_display);
  if (status == -1 && errno != EAGAIN)
    scm_syserror(FUNC_NAME);
  return scm_from_bool(status == 0);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_cancel_read
SCM_DEFINE_PUBLIC(scm_wl_display_cancel_read,
    "wl-display-cancel-read", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  wl_display_cancel_read(i_display);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_read_events
SCM_DEFINE_PUBLIC(scm_wl_display_read_events,
    "wl-display-read-events", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, i_display);
  int status = wl_display_read_events(i_display);
  if (status == -1)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM wl_log_port;

struct log_data {
  const char *fmt;
  va_list args;
};

static void *do_log(void *data) {
  struct log_data *d = data;
  char *str;
  int bytes = vasprintf(&str, d->fmt, d->args);
  if (bytes == -1)
    scm_syserror("wl_log_handler_client");
  scm_puts(str, wl_log_port);
  free(str);
  return NULL;
}

static void log_to_port(const char *fmt, va_list args) {
  struct log_data data = { fmt };
  va_copy(data.args, args);
  scm_with_guile(do_log, &data);
}

#define FUNC_NAME s_scm_wl_set_log_port_client
SCM_DEFINE_PUBLIC(scm_wl_set_log_port_client, "wl-set-log-port-client", 1, 0, 0,
    (SCM port),
    "") {
  SCM_VALIDATE_OUTPUT_PORT(SCM_ARG1, port);
  wl_log_port = port;
  wl_log_set_handler_client(log_to_port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void register_wayland_client_core(void *data) {
  SCM class_type
    = scm_variable_ref(scm_c_public_lookup("oop goops", "<class>"));
  SCM make_class
    = scm_variable_ref(scm_c_public_lookup("oop goops", "make-class"));

  static const char s_scm_wl_proxy_class[] = "<wl-proxy-class>";
  scm_wl_proxy_class_type = scm_call_4(make_class,
      scm_list_1(class_type),
      scm_list_1(scm_list_1(scm_from_utf8_symbol("interface"))),
      scm_from_utf8_keyword("name"),
      scm_from_utf8_symbol(s_scm_wl_proxy_class));
  scm_c_define(s_scm_wl_proxy_class, scm_wl_proxy_class_type);
  scm_c_export(s_scm_wl_proxy_class, NULL);

  static const char s_scm_wl_event_queue[] = "<wl-event-queue>";
  scm_wl_event_queue_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_event_queue),
      scm_list_1(scm_from_utf8_symbol("queue")),
      NULL);
  scm_c_define(s_scm_wl_event_queue, scm_wl_event_queue_type);
  scm_c_export(s_scm_wl_event_queue, NULL);

  static const char s_scm_wl_proxy[] = "<wl-proxy>";
  scm_wl_proxy_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_proxy),
      scm_list_2(
        scm_from_utf8_symbol("proxy"),
        scm_from_utf8_symbol("interface")),
      NULL);
  scm_c_define(s_scm_wl_proxy, scm_wl_proxy_type);
  scm_c_export(s_scm_wl_proxy, NULL);

  static const char s_scm_wl_interface[] = "<wl-interface>";
  scm_wl_interface_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_interface),
      scm_list_1(scm_from_utf8_symbol("interface")),
      NULL);
  scm_c_define(s_scm_wl_interface, scm_wl_interface_type);
  scm_c_export(s_scm_wl_interface, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-client.x"
#endif
}

void scm_i_init_wayland_client_core(void) {
  scm_c_define_module("wayland client core",
      register_wayland_client_core, NULL);
}

void scm_init_wayland_client(void) {
  scm_i_init_wayland_client_core();
  scm_i_init_wayland_cursor();
  scm_i_init_wayland_egl();
  scm_i_init_wayland_utils();
}
