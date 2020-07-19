/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <errno.h>
#include <libguile.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <wayland-client-core.h>

#include "guile-wayland-client.h"
#include "guile-wayland-cursor.h"
#include "guile-wayland-egl.h"
#include "guile-wayland-util-private.h"

SCM scm_wl_proxy_class_type;
SCM scm_wl_event_queue_type;
SCM scm_wl_proxy_type;

SCM scm_from_wl_proxy(struct wl_proxy *proxy) {
  return scm_make_foreign_object_2(scm_wl_proxy_type, proxy, NULL);
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

#define VALIDATE_MARSHAL_ARGS(proxy, i_proxy, \
                              interface, i_interface, \
                              opcode, i_opcode) \
  do { \
    SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy); \
    SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG2, interface, i_interface); \
    SCM_VALIDATE_UINT_COPY(SCM_ARG3, opcode, i_opcode); \
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
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal, "wl-proxy-marshal", 3, 0, 1,
    (SCM proxy, SCM interface, SCM opcode, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t             i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy,
                        interface, i_interface,
                        opcode, i_opcode);

  scm_dynwind_begin(0);
  union wl_argument *args = scm_i_pack_wl_arguments(SCM_ARG4,
      FUNC_NAME, &i_interface->methods[i_opcode], false, rest);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  void *wrapper = wl_proxy_create_wrapper(i_proxy);
  if (!wrapper)
    scm_report_out_of_memory();
  return scm_from_wl_proxy(wrapper);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_wrapper_destroy
SCM_DEFINE_PUBLIC(scm_wl_proxy_wrapper_destroy,
    "wl-proxy-wrapper-destroy", 1, 0, 0,
    (SCM wrapper),
    "") {
  void                *i_wrapper;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, wrapper, i_wrapper);
  wl_proxy_wrapper_destroy(i_wrapper);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_marshal_constructor
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal_constructor,
    "wl-proxy-marshal-constructor", 4, 0, 1,
    (SCM proxy, SCM interface, SCM opcode, SCM ret_interface, SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t             i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy,
                        interface, i_interface,
                        opcode, i_opcode);

  struct wl_interface *i_ret_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG4, ret_interface, i_ret_interface);

  scm_dynwind_begin(0);
  union wl_argument *args = scm_i_pack_wl_arguments(SCM_ARG5,
      FUNC_NAME, &i_interface->methods[i_opcode], false, rest);
  struct wl_proxy *ret = wl_proxy_marshal_array_constructor(
      i_proxy, i_opcode, args, i_ret_interface);
  scm_dynwind_end();
  return scm_from_wl_proxy(ret);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_marshal_constructor_versioned
SCM_DEFINE_PUBLIC(scm_wl_proxy_marshal_constructor_versioned,
    "wl-proxy-marshal-constructor-versioned", 5, 0, 1,
    (SCM proxy, SCM interface, SCM opcode, SCM ret_interface, SCM version,
     SCM rest),
    "") {
  struct wl_proxy     *i_proxy;
  struct wl_interface *i_interface;
  uint32_t i_opcode;

  VALIDATE_MARSHAL_ARGS(proxy, i_proxy,
                        interface, i_interface,
                        opcode, i_opcode);

  struct wl_interface *i_ret_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG4, ret_interface, i_ret_interface);

  uint32_t ret_version;
  SCM_VALIDATE_UINT_COPY(SCM_ARG5, version, ret_version);

  scm_dynwind_begin(0);
  union wl_argument *args = scm_i_pack_wl_arguments(SCM_ARG6,
      FUNC_NAME, &i_interface->methods[i_opcode], false, rest);
  struct wl_proxy *ret
   = wl_proxy_marshal_array_constructor_versioned(
       i_proxy, i_opcode, args, i_ret_interface, ret_version);
  scm_dynwind_end();
  return scm_from_wl_proxy(ret);
}
#undef FUNC_NAME

struct dispatch_data {
  const void *impl;
  uint32_t opcode;
  const struct wl_message *message;
  union wl_argument *args;
  int ret;
};

static SCM proxy_dispatch_body(void *data) {
  struct dispatch_data *d = data;
  SCM procs = SCM_PACK_POINTER(d->impl);
  SCM proc = scm_c_array_ref_1(procs, d->opcode);
  if (scm_is_false(proc))
    return SCM_UNDEFINED;

  scm_dynwind_begin(0);
  long argc = scm_i_signature_arg_count(d->message->signature);
  SCM *argv = scm_i_unpack_wl_arguments(d->message, argc, d->args, false);
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
      proxy_dispatch_handler, data);
  return NULL;
}

static int proxy_dispatch(const void *impl, void *proxy, uint32_t opcode,
    const struct wl_message *message, union wl_argument *args) {
  struct dispatch_data data = { impl, opcode, message, args, 0 };
  scm_with_guile(do_proxy_dispatch, &data);
  return data.ret;
}

#define FUNC_NAME s_scm_wl_proxy_add_listener
SCM_DEFINE_PUBLIC(scm_wl_proxy_add_listener, "wl-proxy-add-listener", 2, 0, 1,
    (SCM proxy, SCM interface, SCM rest),
    "") {
  struct wl_proxy *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  if (scm_foreign_object_ref(proxy, 1) != NULL)
    scm_error(scm_misc_error_key, FUNC_NAME, "proxy already has listener",
        SCM_EOL, SCM_EOL);

  struct wl_interface *i_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG2, interface, i_interface);

  scm_i_validate_dispatch_list(SCM_ARG3, FUNC_NAME, i_interface->name,
      i_interface->event_count, i_interface->events, 0, rest);
  SCM array = scm_list_to_array(scm_from_int(1), rest);
  void *impl = SCM_UNPACK_POINTER(array);
  int result = wl_proxy_add_dispatcher(i_proxy, proxy_dispatch, impl, NULL);
  if (result < 0)
    scm_misc_error(FUNC_NAME, "failed to add dispatcher to proxy ~a",
        scm_list_1(proxy));
  scm_foreign_object_set_x(proxy, 1, impl);
  scm_gc_protect_object(array);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_destroy
SCM_DEFINE_PUBLIC(scm_wl_proxy_destroy, "wl-proxy-destroy", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  void *impl = scm_foreign_object_ref(proxy, 1);
  if (impl != NULL)
    scm_gc_unprotect_object(SCM_PACK_POINTER(impl));
  wl_proxy_destroy(i_proxy);
  scm_foreign_object_set_x(proxy, 0, NULL);
  scm_foreign_object_set_x(proxy, 1, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_version
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_version, "wl-proxy-get-version", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  return scm_from_uint32(wl_proxy_get_version(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_id
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_id, "wl-proxy-get-id", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  return scm_from_uint32(wl_proxy_get_id(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_class
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_class, "wl-proxy-get-class", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
  return scm_from_utf8_string(wl_proxy_get_class(i_proxy));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_get_symbol
SCM_DEFINE_PUBLIC(scm_wl_proxy_get_symbol, "wl-proxy-get-symbol", 1, 0, 0,
    (SCM proxy),
    "") {
  struct wl_proxy     *i_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, proxy, i_proxy);

  struct wl_event_queue *i_queue;
  SCM_VALIDATE_WL_EVENT_QUEUE_COPY(SCM_ARG2, queue, i_queue);

  wl_proxy_set_queue(i_proxy, i_queue);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_proxy_move
SCM_DEFINE_PUBLIC(scm_wl_proxy_move, "wl-proxy-move", 2, 0, 0,
    (SCM src, SCM dst),
    "") {
  struct wl_proxy     *src_proxy;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, src, src_proxy);
  SCM_ASSERT_TYPE(SCM_IS_A_P(dst, scm_wl_proxy_type),
      dst, SCM_ARG2, FUNC_NAME, "wl-proxy");

  scm_foreign_object_set_x(dst, 0, src_proxy);
  scm_foreign_object_set_x(dst, 1, scm_foreign_object_ref(src, 1));
  scm_foreign_object_set_x(src, 0, NULL);
  scm_foreign_object_set_x(src, 1, NULL);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

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
  return scm_from_wl_proxy((struct wl_proxy *) display);
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
  return scm_from_wl_proxy((struct wl_proxy *) display);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_disconnect
SCM_DEFINE_PUBLIC(scm_wl_display_disconnect, "wl-display-disconnect", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
  return scm_from_int(wl_display_get_fd(i_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_dispatch
SCM_DEFINE_PUBLIC(scm_wl_display_dispatch, "wl-display-dispatch", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
  return scm_from_int(wl_display_get_error(i_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_protocol_error
SCM_DEFINE_PUBLIC(scm_wl_display_get_protocol_error,
    "wl-display-get-protocol-error", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *i_display;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
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
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, display, i_display);
  int status = wl_display_read_events(i_display);
  if (status == -1)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM wl_client_log_port;

struct log_data {
  const char *fmt;
  va_list args;
};

static void *do_log(void *data) {
  struct log_data *d = data;
  char *str;
  int bytes = vasprintf(&str, d->fmt, d->args);
  if (bytes == -1)
    scm_syserror("wl-log-handler-client");
  scm_puts(str, wl_client_log_port);
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
  wl_client_log_port = port;
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
      scm_list_2(scm_from_utf8_symbol("proxy"),
                 scm_from_utf8_symbol("implementation")),
      NULL);
  scm_c_define(s_scm_wl_proxy, scm_wl_proxy_type);
  scm_c_export(s_scm_wl_proxy, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-client.x"
#endif
}

void scm_init_wayland_client(void) {
  scm_c_define_module("wayland client core",
      register_wayland_client_core, NULL);
  scm_i_init_wayland_cursor();
  scm_i_init_wayland_egl();
  scm_i_init_wayland_util();
}
