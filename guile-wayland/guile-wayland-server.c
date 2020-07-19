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

#include "guile-wayland-server.h"
#include "guile-wayland-util-private.h"

SCM scm_wl_resource_class_type;
SCM scm_wl_event_source_type;
SCM scm_wl_event_loop_type;
SCM scm_wl_display_type;
SCM scm_wl_client_type;
SCM scm_wl_global_type;
SCM scm_wl_resource_type;
SCM scm_wl_listener_type;
SCM scm_wl_shm_buffer_type;
SCM scm_wl_shm_pool_type;
SCM scm_wl_protocol_logger_type;

SCM_SYMBOL(sym_asterisk, "*");
SCM_SYMBOL(sym_out_of_memory, "out-of-memory");
SCM_SYMBOL(sym_request, "request");
SCM_SYMBOL(sym_event, "event");

SCM scm_c_make_wl_listener(SCM proc, wl_notify_func_t func,
    struct wl_listener **link) {
  struct scm_wl_listener *listener = scm_calloc(sizeof(*listener));
  listener->proc = proc;
  listener->listener.notify = func;
  *link = &listener->listener;
  scm_gc_protect_object(listener->proc);
  return scm_make_foreign_object_1(scm_wl_listener_type, listener);
}

static void foreign_set_protected(SCM foreign, size_t n, SCM value) {
  void *c_value = NULL;
  if (!scm_is_false(value)) {
    c_value = SCM_UNPACK_POINTER(value);
    scm_gc_protect_object(value);
  }

  void *old_value = scm_foreign_object_ref(foreign, n);
  if (old_value != NULL) {
    scm_gc_unprotect_object(SCM_PACK_POINTER(old_value));
  }

  scm_foreign_object_set_x(foreign, n, c_value);
}

#define FUNC_NAME s_scm_wl_listener_destroy
SCM_DEFINE_PUBLIC(scm_wl_listener_destroy, "wl-listener-destroy", 1, 0, 0,
    (SCM listener),
    "") {
  struct scm_wl_listener *c_listener;
  SCM_VALIDATE_WL_LISTENER_COPY(SCM_ARG1, listener, c_listener);
  if (!scm_is_false(c_listener->proc))
    scm_gc_unprotect_object(c_listener->proc);
  wl_list_remove(&c_listener->listener.link);
  free(c_listener);
  scm_foreign_object_set_x(listener, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_loop_create
SCM_DEFINE_PUBLIC(scm_wl_event_loop_create, "wl-event-loop-create", 0, 0, 0,
    (void),
    "") {
  struct wl_event_loop *loop = wl_event_loop_create();
  if (!loop)
    scm_syserror(FUNC_NAME);
  return scm_make_foreign_object_1(scm_wl_event_loop_type, loop);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_loop_destroy
SCM_DEFINE_PUBLIC(scm_wl_event_loop_destroy, "wl-event-loop-destroy", 1, 0, 0,
    (SCM loop),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  wl_event_loop_destroy(c_loop);
  scm_foreign_object_set_x(loop, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

struct loop_func_data {
  void *data;
  uint32_t mask;
  int ret;
};

static void *scm_fd_func(void *data) {
  struct loop_func_data *d = data;
  SCM proc = SCM_PACK_POINTER(d->data);
  SCM result;
  if (scm_thunk_p(proc)) {
    result = scm_call_0(proc);
  } else {
    result = scm_call_1(proc, scm_from_uint32(d->mask));
  }
  if (scm_is_true(result)) {
    d->ret = 1;
  }
  return NULL;
}

static int fd_func(int fd, uint32_t mask, void *data) {
  struct loop_func_data d = { data, mask, 0 };
  scm_with_guile(scm_fd_func, &d);
  return d.ret;
}

#define FUNC_NAME s_scm_wl_event_loop_add_fd
SCM_DEFINE_PUBLIC(scm_wl_event_loop_add_fd, "wl-event-loop-add-fd", 4, 0, 0,
    (SCM loop, SCM fd, SCM mask, SCM proc),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  uint32_t c_fd;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, fd, c_fd);
  uint32_t c_mask;
  SCM_VALIDATE_UINT_COPY(SCM_ARG3, mask, c_mask);
  SCM_VALIDATE_PROC(SCM_ARG4, proc);
  SCM arities = scm_procedure_minimum_arity(proc);
  if (!scm_is_false(arities)) {
    int req = scm_to_int(SCM_CAR(arities));
    if (SCM_UNLIKELY(req > 1)) {
      scm_error(scm_arg_type_key,
          FUNC_NAME, "Expected procedure of arity 0 or 1 in position 4",
          SCM_EOL, scm_list_1(proc));
    }
  }
  void *c_proc = SCM_UNPACK_POINTER(proc);
  struct wl_event_source *source = wl_event_loop_add_fd(c_loop, c_fd, c_mask,
      fd_func, c_proc);
  if (!source)
    scm_syserror(FUNC_NAME);
  scm_gc_protect_object(proc);
  return scm_make_foreign_object_2(scm_wl_event_source_type, source, c_proc);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_source_fd_update
SCM_DEFINE_PUBLIC(scm_wl_event_source_fd_update,
    "wl-event-source-fd-update", 2, 0, 0,
    (SCM source, SCM mask),
    "") {
  struct wl_event_source *c_source;
  SCM_VALIDATE_WL_EVENT_SOURCE_COPY(SCM_ARG1, source, c_source);
  uint32_t c_mask;
  SCM_VALIDATE_UINT_COPY(SCM_ARG2, mask, c_mask);
  int result = wl_event_source_fd_update(c_source, c_mask);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return scm_from_int(result);
}
#undef FUNC_NAME

static void *scm_timer_func(void *data) {
  struct loop_func_data *d = data;
  SCM thunk = SCM_PACK_POINTER(d->data);
  if (scm_is_true(scm_call_0(thunk))) {
    d->ret = 1;
  }
  return NULL;
}

static int timer_func(void *data) {
  struct loop_func_data d = { data, 0, 0 };
  scm_with_guile(scm_timer_func, &d);
  return d.ret;
}

#define FUNC_NAME s_scm_wl_event_loop_add_timer
SCM_DEFINE_PUBLIC(scm_wl_event_loop_add_timer,
    "wl-event-loop-add-timer", 2, 0, 0,
    (SCM loop, SCM thunk),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  void *c_thunk = SCM_UNPACK_POINTER(thunk);
  struct wl_event_source *source
    = wl_event_loop_add_timer(c_loop, timer_func, c_thunk);
  if (!source)
    scm_syserror(FUNC_NAME);
  scm_gc_protect_object(thunk);
  return scm_make_foreign_object_2(scm_wl_event_source_type, source, c_thunk);
}
#undef FUNC_NAME

static int signal_func(int signal_number, void *data) {
  struct loop_func_data d = { data, 0, 0 };
  scm_with_guile(scm_timer_func, &d);
  return d.ret;
}

#define FUNC_NAME s_scm_wl_event_loop_add_signal
SCM_DEFINE_PUBLIC(scm_wl_event_loop_add_signal,
    "wl-event-loop-add-signal", 3, 0, 0,
    (SCM loop, SCM signal, SCM thunk),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  uint32_t c_signal;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, signal, c_signal);
  SCM_VALIDATE_THUNK(SCM_ARG3, thunk);
  void *c_thunk = SCM_UNPACK_POINTER(thunk);
  struct wl_event_source *source
    = wl_event_loop_add_signal(c_loop, c_signal, signal_func, c_thunk);
  if (!source)
    scm_syserror(FUNC_NAME);
  scm_gc_protect_object(thunk);
  return scm_make_foreign_object_2(scm_wl_event_source_type, source, c_thunk);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_source_timer_update
SCM_DEFINE_PUBLIC(scm_wl_event_source_timer_update,
    "wl-event-source-timer-update", 2, 0, 0,
    (SCM source, SCM ms_delay),
    "") {
  struct wl_event_source *c_source;
  SCM_VALIDATE_WL_EVENT_SOURCE_COPY(SCM_ARG1, source, c_source);
  int c_delay;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, ms_delay, c_delay);
  int result = wl_event_source_timer_update(c_source, c_delay);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_source_remove
SCM_DEFINE_PUBLIC(scm_wl_event_source_remove, "wl-event-source-remove", 1, 0, 0,
    (SCM source),
    "") {
  struct wl_event_source *c_source;
  SCM_VALIDATE_WL_EVENT_SOURCE_COPY(SCM_ARG1, source, c_source);
  wl_event_source_remove(c_source);
  scm_foreign_object_set_x(source, 0, NULL);
  foreign_set_protected(source, 1, SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_source_check
SCM_DEFINE_PUBLIC(scm_wl_event_source_check, "wl-event-source-check", 1, 0, 0,
    (SCM source),
    "") {
  struct wl_event_source *c_source;
  SCM_VALIDATE_WL_EVENT_SOURCE_COPY(SCM_ARG1, source, c_source);
  wl_event_source_check(c_source);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_loop_dispatch
SCM_DEFINE_PUBLIC(scm_wl_event_loop_dispatch, "wl-event-loop-dispatch", 2, 0, 0,
    (SCM loop, SCM timeout),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  int c_timeout;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, timeout, c_timeout);
  int result = wl_event_loop_dispatch(c_loop, c_timeout);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_loop_dispatch_idle
SCM_DEFINE_PUBLIC(scm_wl_event_loop_dispatch_idle,
    "wl-event-loop-dispatch-idle", 1, 0, 0,
    (SCM loop),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  wl_event_loop_dispatch_idle(c_loop);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void *scm_idle_func(void *data) {
  struct loop_func_data *d = data;
  SCM thunk = SCM_PACK_POINTER(d->data);
  scm_call_0(thunk);
  return NULL;
}

static void idle_func(void *data) {
  struct loop_func_data d = { data, 0, 0 };
  scm_with_guile(scm_idle_func, &d);
}

#define FUNC_NAME s_scm_wl_event_loop_add_idle
SCM_DEFINE_PUBLIC(scm_wl_event_loop_add_idle,
    "wl-event-loop-add-idle", 2, 0, 0,
    (SCM loop, SCM thunk),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  void *c_thunk = SCM_UNPACK_POINTER(thunk);
  struct wl_event_source *source
    = wl_event_loop_add_idle(c_loop, idle_func, c_thunk);
  if (!source)
    scm_syserror(FUNC_NAME);
  scm_gc_protect_object(thunk);
  return scm_make_foreign_object_2(scm_wl_event_source_type, source, c_thunk);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_event_loop_get_fd
SCM_DEFINE_PUBLIC(scm_wl_event_loop_get_fd, "wl-event-loop-get-fd", 1, 0, 0,
    (SCM loop),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  return scm_from_int(wl_event_loop_get_fd(c_loop));
}
#undef FUNC_NAME

static void *scm_noarg_notify(void *data) {
  struct scm_wl_listener *listener = data;
  scm_call_0(listener->proc);
  return NULL;
}

static void noarg_notify(struct wl_listener *listener, void *data) {
  struct scm_wl_listener *scm_listener
    = wl_container_of(listener, scm_listener, listener);
  scm_with_guile(scm_noarg_notify, scm_listener);
}

#define FUNC_NAME s_scm_wl_event_loop_add_destroy_listener
SCM_DEFINE_PUBLIC(scm_wl_event_loop_add_destroy_listener,
    "wl-event-loop-add-destroy-listener", 2, 0, 0,
    (SCM loop, SCM thunk),
    "") {
  struct wl_event_loop *c_loop;
  SCM_VALIDATE_WL_EVENT_LOOP_COPY(SCM_ARG1, loop, c_loop);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(thunk, noarg_notify, &link);
  wl_event_loop_add_destroy_listener(c_loop, link);
  return listener;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_create
SCM_DEFINE_PUBLIC(scm_wl_display_create, "wl-display-create", 0, 0, 0,
    (void),
    "") {
  struct wl_display *display = wl_display_create();
  if (!display)
    scm_syserror(FUNC_NAME);
  return scm_make_foreign_object_2(scm_wl_display_type, display, NULL);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_destroy
SCM_DEFINE_PUBLIC(scm_wl_display_destroy, "wl-display-destroy", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  wl_display_destroy(c_display);
  scm_foreign_object_set_x(display, 0, NULL);
  foreign_set_protected(display, 1, SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_event_loop
SCM_DEFINE_PUBLIC(scm_wl_display_get_event_loop,
    "wl-display-get-event-loop", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  struct wl_event_loop *loop = wl_display_get_event_loop(c_display);
  return scm_make_foreign_object_1(scm_wl_event_loop_type, loop);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_socket
SCM_DEFINE_PUBLIC(scm_wl_display_add_socket,
    "wl-display-add-socket", 2, 0, 0,
    (SCM display, SCM name),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  SCM_VALIDATE_STRING(SCM_ARG2, name);
  char *c_name = scm_to_utf8_string(name);
  int result = wl_display_add_socket(c_display, c_name);
  free(c_name);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_socket_auto
SCM_DEFINE_PUBLIC(scm_wl_display_add_socket_auto,
    "wl-display-add-socket-auto", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  const char *name = wl_display_add_socket_auto(c_display);
  if (name == NULL)
    scm_syserror(FUNC_NAME);
  return scm_from_utf8_string(name);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_socket_fd
SCM_DEFINE_PUBLIC(scm_wl_display_add_socket_fd,
    "wl-display-add-socket-fd", 2, 0, 0,
    (SCM display, SCM fd),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  int c_fd;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, fd, c_fd);
  int result = wl_display_add_socket_fd(c_display, c_fd);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_terminate
SCM_DEFINE_PUBLIC(scm_wl_display_terminate, "wl-display-terminate", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  wl_display_terminate(c_display);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_run
SCM_DEFINE_PUBLIC(scm_wl_display_run, "wl-display-run", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  wl_display_run(c_display);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_flush_clients
SCM_DEFINE_PUBLIC(scm_wl_display_flush_clients,
    "wl-display-flush-clients", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  wl_display_flush_clients(c_display);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_destroy_clients
SCM_DEFINE_PUBLIC(scm_wl_display_destroy_clients,
    "wl-display-destroy-clients", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  wl_display_destroy_clients(c_display);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_serial
SCM_DEFINE_PUBLIC(scm_wl_display_get_serial, "wl-display-get-serial", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  return scm_from_uint32(wl_display_get_serial(c_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_next_serial
SCM_DEFINE_PUBLIC(scm_wl_display_next_serial, "wl-display-next-serial", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  return scm_from_uint32(wl_display_next_serial(c_display));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_destroy_listener
SCM_DEFINE_PUBLIC(scm_wl_display_add_destroy_listener,
    "wl-display-add-destroy-listener", 2, 0, 0,
    (SCM display, SCM thunk),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(thunk, noarg_notify, &link);
  wl_display_add_destroy_listener(c_display, link);
  return listener;
}
#undef FUNC_NAME

struct listener_data {
  struct scm_wl_listener *listener;
  void *data;
};

SCM scm_from_wl_client(struct wl_client *c_client) {
  SCM client = scm_make_foreign_object_1(scm_wl_client_type, client);
  return client;
}

static SCM client_created_body(void *data) {
  struct listener_data *d = data;
  SCM client = scm_from_wl_client(d->data);
  scm_call_1(d->listener->proc, client);
  return SCM_UNDEFINED;
}

static SCM client_error_handler(void *data, SCM key, SCM args) {
  struct listener_data *d = data;
  struct wl_client *client = d->data;
  if (scm_is_eq(key, sym_out_of_memory)) {
    wl_client_post_no_memory(client);
  } else {
    SCM port = scm_open_output_string();
    scm_print_exception(port, SCM_BOOL_F, key, args);
    char *str = scm_to_utf8_string(scm_get_output_string(port));
    wl_client_post_implementation_error(client, "%s", str);
    free(str);
  }
  scm_throw(key, args);
  return SCM_UNDEFINED;
}

static void *scm_client_created_notify(void *data) {
  scm_internal_catch(SCM_BOOL_T, client_created_body, data,
      client_error_handler, data);
  return NULL;
}

static void client_created_notify(struct wl_listener *listener,
    void *data) {
  struct listener_data d = {
    wl_container_of(listener, d.listener, listener),
    data,
  };
  scm_with_guile(scm_client_created_notify, &d);
}

#define SCM_VALIDATE_PROC_ARITY(_pos, _proc, _arity) \
  do { \
    SCM_VALIDATE_PROC((_pos), (_proc)); \
    SCM _arities = scm_procedure_minimum_arity(_proc); \
    if (!scm_is_false(_arities)) { \
      int _req = scm_to_int(SCM_CAR(_arities)); \
      int _opt = scm_to_int(SCM_CADR(_arities)); \
      int _rest = scm_to_int(SCM_CADDR(_arities)); \
      if (SCM_UNLIKELY(_req > (_arity) || (!_rest && _req + _opt < (_arity)))) { \
        scm_error(scm_arg_type_key, \
            FUNC_NAME, "Expected procedure of arity ~a in position ~a", \
            scm_list_2(scm_from_uint(_arity), scm_from_uint(_pos)), \
            scm_list_1(_proc)); \
      } \
    } \
  } while (0)

#define FUNC_NAME s_scm_wl_display_add_client_created_listener
SCM_DEFINE_PUBLIC(scm_wl_display_add_client_created_listener,
    "wl-display-add-client-created-listener", 2, 0, 0,
    (SCM display, SCM proc),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG2, proc, 1);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(proc, client_created_notify, &link);
  wl_display_add_client_created_listener(c_display, link);
  return listener;
}
#undef FUNC_NAME

struct global_bind_data {
  struct wl_client *client;
  void *data;
  uint32_t version;
  uint32_t id;
};

static void *scm_global_bind_func(void *data) {
  struct global_bind_data *d = data;
  SCM proc = SCM_PACK_POINTER(d->data);
  SCM client = scm_from_wl_client(d->client);
  scm_call_3(proc, client, scm_from_uint32(d->version), scm_from_uint32(d->id));
  return NULL;
}

static void global_bind_func(struct wl_client *client, void *data,
    uint32_t version, uint32_t id) {
  struct global_bind_data d = { client, data, version, id };
  scm_with_guile(scm_global_bind_func, &d);
}

SCM scm_from_wl_global(struct wl_global *c_global) {
  void *ptr = wl_global_get_user_data(c_global);
  if (ptr != NULL)
    return SCM_PACK_POINTER(ptr);
  SCM global = scm_make_foreign_object_2(scm_wl_global_type, c_global, NULL);
  wl_global_set_user_data(c_global, SCM_UNPACK_POINTER(global));
  scm_gc_protect_object(global);
  return global;
}

#define FUNC_NAME s_scm_wl_global_create
SCM_DEFINE_PUBLIC(scm_wl_global_create, "wl-global-create", 5, 0, 0,
    (SCM global, SCM display, SCM interface, SCM version, SCM proc),
    "") {
  SCM_ASSERT_TYPE(SCM_IS_A_P(global, scm_wl_global_type),
      global, SCM_ARG1, FUNC_NAME, "<wl-global>");
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG2, display, c_display);
  struct wl_interface *c_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG3, interface, c_interface);
  int c_version;
  SCM_VALIDATE_INT_COPY(SCM_ARG4, version, c_version);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG5, proc, 3);
  void *bind = SCM_UNPACK_POINTER(proc);
  struct wl_global *c_global = wl_global_create(c_display, c_interface,
      c_version, bind, global_bind_func);
  if (!c_global)
    scm_syserror(FUNC_NAME);
  scm_foreign_object_set_x(global, 0, c_global);
  scm_foreign_object_set_x(global, 1, bind);
  wl_global_set_user_data(c_global, SCM_UNPACK_POINTER(global));
  scm_gc_protect_object(global);
  return global;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_global_remove
SCM_DEFINE_PUBLIC(scm_wl_global_remove, "wl-global-remove", 1, 0, 0,
    (SCM global),
    "") {
  struct wl_global *c_global;
  SCM_VALIDATE_WL_GLOBAL_COPY(SCM_ARG1, global, c_global);
  wl_global_remove(c_global);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_global_destroy
SCM_DEFINE_PUBLIC(scm_wl_global_destroy, "wl-global-destroy", 1, 0, 0,
    (SCM global),
    "") {
  struct wl_global *c_global;
  SCM_VALIDATE_WL_GLOBAL_COPY(SCM_ARG1, global, c_global);
  wl_global_set_user_data(c_global, NULL);
  wl_global_destroy(c_global);
  scm_foreign_object_set_x(global, 0, NULL);
  scm_foreign_object_set_x(global, 1, NULL);
  scm_gc_unprotect_object(global);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

struct display_global_filter_data {
  const struct wl_client *client;
  const struct wl_global *global;
  void *data;
  bool ret;
};

static void *scm_display_global_filter(void *data) {
  struct display_global_filter_data *d = data;
  SCM proc = SCM_PACK_POINTER(d->data);
  SCM client = scm_from_wl_client((struct wl_client *) d->client);
  SCM global = scm_from_wl_global((struct wl_global *) d->global);
  d->ret = scm_is_true(scm_call_2(proc, client, global));
  return NULL;
}

static bool display_global_filter(const struct wl_client *client,
    const struct wl_global *global, void *data) {
  struct display_global_filter_data d = { client, global, data, false };
  scm_with_guile(scm_display_global_filter, &d);
  return d.ret;
}

#define FUNC_NAME s_scm_wl_display_set_global_filter
SCM_DEFINE_PUBLIC(scm_wl_display_set_global_filter,
    "wl-display-set-global-filter", 2, 0, 0,
    (SCM display, SCM proc),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG2, proc, 2);
  void *c_proc = SCM_UNPACK_POINTER(proc);
  wl_display_set_global_filter(c_display, display_global_filter, c_proc);
  foreign_set_protected(display, 1, proc);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_global_get_interface
SCM_DEFINE_PUBLIC(scm_wl_global_get_interface,
    "wl-global-get-interface", 1, 0, 0,
    (SCM global),
    "") {
  struct wl_global *c_global;
  SCM_VALIDATE_WL_GLOBAL_COPY(SCM_ARG1, global, c_global);
  const struct wl_interface *interface = wl_global_get_interface(c_global);
  return scm_make_foreign_object_1(scm_wl_interface_type, (void *) interface);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_create
SCM_DEFINE_PUBLIC(scm_wl_client_create, "wl-client-create", 2, 0, 0,
    (SCM display, SCM fd),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  int c_fd;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, fd, c_fd);
  struct wl_client *c_client = wl_client_create(c_display, c_fd);
  if (!c_client)
    scm_syserror(FUNC_NAME);
  return scm_from_wl_client(c_client);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_get_client_list
SCM_DEFINE_PUBLIC(scm_wl_display_get_client_list,
    "wl-display-get-client-list", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  struct wl_list *list = wl_display_get_client_list(c_display);
  SCM clients = SCM_EOL;
  struct wl_client *client;
  for (client = wl_client_from_link((list)->prev);
      wl_client_get_link(client) != (list);
      client = wl_client_from_link(wl_client_get_link(client)->prev)) {
    clients = scm_cons(scm_from_wl_client(client), clients);
  }
  return clients;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_destroy
SCM_DEFINE_PUBLIC(scm_wl_client_destroy, "wl-client-destroy", 1, 0, 0,
    (SCM client),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  wl_client_destroy(c_client);
  scm_foreign_object_set_x(client, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_flush
SCM_DEFINE_PUBLIC(scm_wl_client_flush, "wl-client-flush", 1, 0, 0,
    (SCM client),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  wl_client_flush(c_client);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_get_credentials
SCM_DEFINE_PUBLIC(scm_wl_client_get_credentials,
    "wl-client-get-credentials", 1, 0, 0,
    (SCM client),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  pid_t pid;
  uid_t uid;
  gid_t gid;
  wl_client_get_credentials(c_client, &pid, &uid, &gid);
  return scm_list_3(scm_from_int(pid), scm_from_uint(uid), scm_from_uint(gid));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_get_fd
SCM_DEFINE_PUBLIC(scm_wl_client_get_fd, "wl-client-get-fd", 1, 0, 0,
    (SCM client),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  return scm_from_int(wl_client_get_fd(c_client));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_client_add_destroy_listener
SCM_DEFINE_PUBLIC(scm_wl_client_add_destroy_listener,
    "wl-client-add-destroy-listener", 2, 0, 0,
    (SCM client, SCM thunk),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(thunk, noarg_notify, &link);
  wl_client_add_destroy_listener(c_client, link);
  return listener;
}
#undef FUNC_NAME

SCM scm_from_wl_resource(struct wl_resource *c_resource) {
  void *ptr = wl_resource_get_user_data(c_resource);
  if (ptr != NULL)
    return SCM_PACK_POINTER(ptr);
  SCM resource
    = scm_make_foreign_object_3(scm_wl_resource_type, c_resource, NULL, NULL);
  wl_resource_set_user_data(c_resource, SCM_UNPACK_POINTER(resource));
  scm_gc_protect_object(resource);
  return resource;
}

#define FUNC_NAME s_scm_wl_client_get_object
SCM_DEFINE_PUBLIC(scm_wl_client_get_object, "wl-client-get-object", 2, 0, 0,
    (SCM client, SCM id),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  uint32_t c_id;
  SCM_VALIDATE_UINT_COPY(SCM_ARG2, id, c_id);
  struct wl_resource *resource = wl_client_get_object(c_client, c_id);
  if (resource == NULL)
    return SCM_BOOL_F;
  return scm_from_wl_resource(resource);
}
#undef FUNC_NAME

static void *scm_resource_created_notify(void *data) {
  struct listener_data *d = data;
  SCM resource = scm_from_wl_resource(d->data);
  scm_call_1(d->listener->proc, resource);
  return NULL;
}

static void resource_created_notify(struct wl_listener *listener,
    void *data) {
  struct listener_data d = {
    wl_container_of(listener, d.listener, listener),
    data,
  };
  scm_with_guile(scm_resource_created_notify, &d);
}

#define FUNC_NAME s_scm_wl_client_add_resource_created_listener
SCM_DEFINE_PUBLIC(scm_wl_client_add_resource_created_listener,
    "wl-client-add-resource-created-listener", 2, 0, 0,
    (SCM client, SCM proc),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG2, proc, 1);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(proc, resource_created_notify, &link);
  wl_client_add_resource_created_listener(c_client, link);
  return listener;
}
#undef FUNC_NAME

enum wl_iterator_result resource_iterator(struct wl_resource *resource,
    void *data) {
  SCM proc = SCM_PACK_POINTER(data);
  SCM result = scm_call_1(proc, scm_from_wl_resource(resource));
  return scm_is_false(result) ? WL_ITERATOR_STOP : WL_ITERATOR_CONTINUE;
}

#define FUNC_NAME s_scm_wl_client_for_each_resource
SCM_DEFINE_PUBLIC(scm_wl_client_for_each_resource,
    "wl-client-for-each-resource", 2, 0, 0,
    (SCM client, SCM proc),
    "") {
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG1, client, c_client);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG2, proc, 1);
  wl_client_for_each_resource(c_client, resource_iterator,
      SCM_UNPACK_POINTER(proc));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define VALIDATE_EVENT_ARGS(resource, i_resource, \
                          interface, c_interface, \
                          opcode, i_opcode) \
  do { \
    SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, i_resource); \
    SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG2, interface, c_interface); \
    SCM_VALIDATE_UINT_COPY(SCM_ARG3, opcode, i_opcode); \
    if (SCM_UNLIKELY(i_opcode >= (uint32_t) c_interface->event_count)) { \
      scm_error(scm_arg_type_key, \
          FUNC_NAME, \
          "Invalid opcode ~A on ~A", \
          scm_list_2( \
            scm_from_uint32(i_opcode), \
            scm_from_utf8_string(c_interface->name)), \
            scm_list_1(opcode)); \
    } \
  } while (0)

#define FUNC_NAME s_scm_wl_resource_post_event
SCM_DEFINE_PUBLIC(scm_wl_resource_post_event,
    "wl-resource-post-event", 3, 0, 1,
    (SCM resource, SCM interface, SCM opcode, SCM rest),
    "") {
  struct wl_resource *c_resource;
  struct wl_interface *c_interface;
  uint32_t c_opcode;

  VALIDATE_EVENT_ARGS(resource,  c_resource,
                      interface, c_interface,
                      opcode,    c_opcode);

  scm_dynwind_begin(0);
  union wl_argument *args = scm_i_pack_wl_arguments(SCM_ARG4,
      FUNC_NAME, &c_interface->events[c_opcode], true, rest);
  wl_resource_post_event_array(c_resource, c_opcode, args);
  scm_dynwind_end();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_queue_event
SCM_DEFINE_PUBLIC(scm_wl_resource_queue_event,
    "wl-resource-queue-event", 3, 0, 1,
    (SCM resource, SCM interface, SCM opcode, SCM rest),
    "") {
  struct wl_resource *c_resource;
  struct wl_interface *c_interface;
  uint32_t c_opcode;

  VALIDATE_EVENT_ARGS(resource,  c_resource,
                      interface, c_interface,
                      opcode,    c_opcode);

  scm_dynwind_begin(0);
  union wl_argument *args = scm_i_pack_wl_arguments(SCM_ARG4,
      FUNC_NAME, &c_interface->events[c_opcode], true, rest);
  wl_resource_queue_event_array(c_resource, c_opcode, args);
  scm_dynwind_end();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_create
SCM_DEFINE_PUBLIC(scm_wl_resource_create, "wl-resource-create", 5, 0, 0,
    (SCM resource, SCM client, SCM interface, SCM version, SCM id),
    "") {
  SCM_ASSERT_TYPE(SCM_IS_A_P(resource, scm_wl_resource_type),
      resource, SCM_ARG1, FUNC_NAME, "<wl-resource>");
  struct wl_resource *c_resource = scm_foreign_object_ref(resource, 0);
  SCM_ASSERT_TYPE(c_resource == NULL, resource, SCM_ARG1, FUNC_NAME,
      "null <wl-resource>");
  struct wl_client *c_client;
  SCM_VALIDATE_WL_CLIENT_COPY(SCM_ARG2, client, c_client);
  struct wl_interface *c_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG3, interface, c_interface);
  int c_version;
  SCM_VALIDATE_INT_COPY(SCM_ARG4, version, c_version);
  uint32_t c_id;
  SCM_VALIDATE_UINT_COPY(SCM_ARG5, id, c_id);
  c_resource = wl_resource_create(c_client, c_interface, c_version, c_id);
  scm_foreign_object_set_x(resource, 0, c_resource);
  return resource;
}
#undef FUNC_NAME

struct dispatch_data {
  const void *impl;
  struct wl_resource *resource;
  uint32_t opcode;
  const struct wl_message *message;
  union wl_argument *args;
  int ret;
};

static SCM resource_dispatch_body(void *data) {
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

static SCM resource_dispatch_handler(void *data, SCM key, SCM args) {
  struct dispatch_data *d = data;
  d->ret = -1;
  struct wl_resource *resource = d->resource;
  if (scm_is_eq(key, sym_out_of_memory)) {
    wl_resource_post_no_memory(resource);
  } else {
    SCM port = scm_open_output_string();
    scm_print_exception(port, SCM_BOOL_F, key, args);
    char *str = scm_to_utf8_string(scm_get_output_string(port));
    wl_resource_post_error(resource, -1, "%s", str);
    free(str);
  }
  scm_throw(key, args);
  return SCM_UNDEFINED;
}

static void *scm_resource_dispatch(void *data) {
  scm_internal_catch(SCM_BOOL_T, resource_dispatch_body, data,
      resource_dispatch_handler, data);
  return NULL;
}

static int resource_dispatch(const void *impl, void *resource, uint32_t opcode,
    const struct wl_message *message, union wl_argument *args) {
  struct dispatch_data data = { impl, resource, opcode, message, args, 0 };
  scm_with_guile(scm_resource_dispatch, &data);
  return data.ret;
}

static void *scm_resource_destroy(void *data) {
  SCM resource = SCM_PACK_POINTER(data);
  void *c_thunk = scm_foreign_object_ref(resource, 2);
  SCM thunk = SCM_PACK_POINTER(c_thunk);
  scm_call_0(thunk);
  return NULL;
}

static void resource_destroy(struct wl_resource *resource) {
  void *data = wl_resource_get_user_data(resource);
  scm_with_guile(scm_resource_destroy, data);
}

#define FUNC_NAME s_scm_wl_resource_set_implementation
SCM_DEFINE_PUBLIC(scm_wl_resource_set_implementation,
    "wl-resource-set-implementation", 2, 0, 1,
    (SCM resource, SCM interface, SCM rest),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  struct wl_interface *c_interface;
  SCM_VALIDATE_WL_INTERFACE_COPY(SCM_ARG2, interface, c_interface);

  scm_i_validate_dispatch_list(SCM_ARG3, FUNC_NAME, c_interface->name,
      c_interface->method_count, c_interface->methods, 1, rest);

  SCM last_index = scm_from_int(c_interface->method_count);
  SCM destructor = scm_list_ref(rest, last_index);
  if (!scm_is_false(destructor)) {
    SCM_VALIDATE_THUNK(SCM_ARG3 + c_interface->method_count, destructor);
  }

  SCM array = scm_list_to_array(scm_from_int(1),
                                scm_list_head(rest, last_index));
  void *impl = SCM_UNPACK_POINTER(array);

  wl_resource_set_dispatcher(c_resource, resource_dispatch, impl, NULL,
      resource_destroy);

  scm_foreign_object_set_x(resource, 1, array);
  scm_foreign_object_set_x(resource, 2, destructor);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_destroy
SCM_DEFINE_PUBLIC(scm_wl_resource_destroy, "wl-resource-destroy", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  wl_resource_set_user_data(c_resource, NULL);
  wl_resource_destroy(c_resource);

  scm_foreign_object_set_x(resource, 0, NULL);
  scm_foreign_object_set_x(resource, 1, SCM_BOOL_F);
  scm_foreign_object_set_x(resource, 2, SCM_BOOL_F);

  scm_gc_unprotect_object(resource);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_get_id
SCM_DEFINE_PUBLIC(scm_wl_resource_get_id, "wl-resource-get-id", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  return scm_from_uint32(wl_resource_get_id(c_resource));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_get_client
SCM_DEFINE_PUBLIC(scm_wl_resource_get_client, "wl-resource-get-client", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  return scm_from_wl_client(wl_resource_get_client(c_resource));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_get_version
SCM_DEFINE_PUBLIC(scm_wl_resource_get_version,
    "wl-resource-get-version", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  return scm_from_int(wl_resource_get_version(c_resource));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_set_destructor
SCM_DEFINE_PUBLIC(scm_wl_resource_set_destructor,
    "wl-resource-set-destructor", 2, 0, 0,
    (SCM resource, SCM destructor),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  if (!scm_is_false(destructor))
    SCM_VALIDATE_THUNK(SCM_ARG2, destructor);

  wl_resource_set_destructor(c_resource, resource_destroy);

  scm_foreign_object_set_x(resource, 2, destructor);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_get_class
SCM_DEFINE_PUBLIC(scm_wl_resource_get_class, "wl-resource-get-class", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  return scm_from_utf8_string(wl_resource_get_class(c_resource));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_resource_add_destroy_listener
SCM_DEFINE_PUBLIC(scm_wl_resource_add_destroy_listener,
    "wl-resource-add-destroy-listener", 2, 0, 0,
    (SCM resource, SCM thunk),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  SCM_VALIDATE_THUNK(SCM_ARG2, thunk);
  struct wl_listener *link;
  SCM listener = scm_c_make_wl_listener(thunk, noarg_notify, &link);
  wl_resource_add_destroy_listener(c_resource, link);
  return listener;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get, "wl-shm-buffer-get", 1, 0, 0,
    (SCM resource),
    "") {
  struct wl_resource *c_resource;
  SCM_VALIDATE_WL_RESOURCE_COPY(SCM_ARG1, resource, c_resource);
  struct wl_shm_buffer *buffer = wl_shm_buffer_get(c_resource);
  if (buffer == NULL)
    return SCM_BOOL_F;
  return scm_make_foreign_object_1(scm_wl_shm_buffer_type, buffer);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_begin_access
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_begin_access,
    "wl-shm-buffer-begin-access", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  wl_shm_buffer_begin_access(c_buffer);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_end_access
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_end_access,
    "wl-shm-buffer-end-access", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  wl_shm_buffer_end_access(c_buffer);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get_data
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get_data, "wl-shm-buffer-get-data", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  void *data = wl_shm_buffer_get_data(c_buffer);
  int32_t height = wl_shm_buffer_get_height(c_buffer);
  int32_t stride = wl_shm_buffer_get_stride(c_buffer);
  return scm_pointer_to_bytevector(scm_from_pointer(data, NULL),
      scm_from_int(height * stride), SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get_stride
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get_stride,
    "wl-shm-buffer-get-stride", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  return scm_from_int32(wl_shm_buffer_get_stride(c_buffer));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get_format
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get_format,
    "wl-shm-buffer-get-format", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  return scm_from_uint32(wl_shm_buffer_get_format(c_buffer));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get_width
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get_width,
    "wl-shm-buffer-get-width", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  return scm_from_int32(wl_shm_buffer_get_width(c_buffer));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_get_height
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_get_height,
    "wl-shm-buffer-get-height", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  return scm_from_int32(wl_shm_buffer_get_height(c_buffer));
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_buffer_ref_pool
SCM_DEFINE_PUBLIC(scm_wl_shm_buffer_ref_pool, "wl-shm-buffer-ref-pool", 1, 0, 0,
    (SCM buffer),
    "") {
  struct wl_shm_buffer *c_buffer;
  SCM_VALIDATE_WL_SHM_BUFFER_COPY(SCM_ARG1, buffer, c_buffer);
  struct wl_shm_pool *pool = wl_shm_buffer_ref_pool(c_buffer);
  return scm_make_foreign_object_1(scm_wl_shm_pool_type, pool);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_shm_pool_unref
SCM_DEFINE_PUBLIC(scm_wl_shm_pool_unref, "wl-shm-pool-unref", 1, 0, 0,
    (SCM pool),
    "") {
  struct wl_shm_pool *c_pool;
  SCM_VALIDATE_WL_SHM_POOL_COPY(SCM_ARG1, pool, c_pool);
  wl_shm_pool_unref(c_pool);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_init_shm
SCM_DEFINE_PUBLIC(scm_wl_display_init_shm, "wl-display-init-shm", 1, 0, 0,
    (SCM display),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  int result = wl_display_init_shm(c_display);
  if (result < 0)
    scm_syserror(FUNC_NAME);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_shm_format
SCM_DEFINE_PUBLIC(scm_wl_display_add_shm_format,
    "wl-display-add-shm-format", 2, 0, 0,
    (SCM display, SCM format),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  uint32_t c_format;
  SCM_VALIDATE_UINT_COPY(SCM_ARG2, format, c_format);
  uint32_t *result = wl_display_add_shm_format(c_display, c_format);
  if (result == NULL)
    scm_report_out_of_memory();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

struct protocol_logger_data {
  void *data;
  enum wl_protocol_logger_type direction;
  const struct wl_protocol_logger_message *message;
};

static void *do_protocol_logger(void *data) {
  struct protocol_logger_data *d = data;
  SCM proc = SCM_PACK_POINTER(d->data);
  SCM direction = SCM_BOOL_F;
  switch (d->direction) {
    case WL_PROTOCOL_LOGGER_REQUEST: direction = sym_request; break;
    case WL_PROTOCOL_LOGGER_EVENT: direction = sym_event; break;
  }
  scm_dynwind_begin(0);
  SCM *argv = scm_i_unpack_wl_arguments(d->message->message,
      d->message->arguments_count, d->message->arguments, false);
  SCM args = SCM_EOL;
  for (int i = d->message->arguments_count; i > 0; i--) {
    args = scm_cons(argv[i], args);
  }
  scm_call_5(proc, direction, scm_from_wl_resource(d->message->resource),
      scm_from_int(d->message->message_opcode),
      scm_from_utf8_string(d->message->message->name), args);
  scm_dynwind_end();
  return NULL;
}

static void protocol_logger(void *data,
    enum wl_protocol_logger_type direction,
    const struct wl_protocol_logger_message *message) {
  struct protocol_logger_data d = { data, direction, message };
  scm_with_guile(do_protocol_logger, &d);
}

static SCM wl_server_log_port;

struct log_data {
  const char *fmt;
  va_list args;
};

static void *do_log(void *data) {
  struct log_data *d = data;
  char *str;
  int bytes = vasprintf(&str, d->fmt, d->args);
  if (bytes == -1)
    scm_syserror("wl-log-handler-server");
  scm_puts(str, wl_server_log_port);
  free(str);
  return NULL;
}

static void log_to_port(const char *fmt, va_list args) {
  struct log_data data = { fmt };
  va_copy(data.args, args);
  scm_with_guile(do_log, &data);
}

#define FUNC_NAME s_scm_wl_set_log_port_server
SCM_DEFINE_PUBLIC(scm_wl_set_log_port_server, "wl-set-log-port-server", 1, 0, 0,
    (SCM port),
    "") {
  SCM_VALIDATE_OUTPUT_PORT(SCM_ARG1, port);
  wl_server_log_port = port;
  wl_log_set_handler_server(log_to_port);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_display_add_protocol_logger
SCM_DEFINE_PUBLIC(scm_wl_display_add_protocol_logger,
    "wl-display-add-protocol-logger", 2, 0, 0,
    (SCM display, SCM proc),
    "") {
  struct wl_display *c_display;
  SCM_VALIDATE_WL_DISPLAY_COPY(SCM_ARG1, display, c_display);
  SCM_VALIDATE_PROC_ARITY(SCM_ARG2, proc, 5);
  void *c_proc = SCM_UNPACK_POINTER(proc);
  struct wl_protocol_logger *logger = wl_display_add_protocol_logger(c_display,
      protocol_logger, c_proc);
  if (logger == NULL)
    scm_report_out_of_memory();
  scm_gc_protect_object(proc);
  return scm_make_foreign_object_2(scm_wl_protocol_logger_type, logger, c_proc);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_protocol_logger_destroy
SCM_DEFINE_PUBLIC(scm_wl_protocol_logger_destroy,
    "wl-protocol-logger-destroy", 1, 0, 0,
    (SCM logger),
    "") {
  struct wl_protocol_logger *c_logger;
  SCM_VALIDATE_WL_PROTOCOL_LOGGER_COPY(SCM_ARG1, logger, c_logger);
  wl_protocol_logger_destroy(c_logger);
  scm_foreign_object_set_x(logger, 0, NULL);
  foreign_set_protected(logger, 1, SCM_BOOL_F);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void register_wayland_server_core(void *data) {
  SCM class_type
    = scm_variable_ref(scm_c_public_lookup("oop goops", "<class>"));
  SCM make_class
    = scm_variable_ref(scm_c_public_lookup("oop goops", "make-class"));

  static const char s_scm_wl_resource_class[] = "<wl-resource-class>";
  scm_wl_resource_class_type = scm_call_4(make_class,
      scm_list_1(class_type),
      scm_list_1(scm_list_1(scm_from_utf8_symbol("interface"))),
      scm_from_utf8_keyword("name"),
      scm_from_utf8_symbol(s_scm_wl_resource_class));
  scm_c_define(s_scm_wl_resource_class, scm_wl_resource_class_type);
  scm_c_export(s_scm_wl_resource_class, NULL);

#define DEFINE_FOREIGN_CLASS_N(_sym, _class_name, _nslots, ...) \
  do { \
    static const char s_##_sym[] = _class_name; \
    _sym = scm_make_foreign_object_type( \
        scm_from_utf8_symbol(s_##_sym), \
        scm_list_##_nslots(__VA_ARGS__), \
        NULL); \
    scm_c_define(s_##_sym, _sym); \
    scm_c_export(s_##_sym, NULL); \
  } while (0)

#define DEFINE_FOREIGN_CLASS_1(_sym, _class_name, _slot1) \
  DEFINE_FOREIGN_CLASS_N(_sym, _class_name, 1, \
      scm_from_utf8_symbol(_slot1))
#define DEFINE_FOREIGN_CLASS_2(_sym, _class_name, _slot1, _slot2) \
  DEFINE_FOREIGN_CLASS_N(_sym, _class_name, 2, \
      scm_from_utf8_symbol(_slot1), \
      scm_from_utf8_symbol(_slot2))
#define DEFINE_FOREIGN_CLASS_3(_sym, _class_name, _slot1, _slot2, _slot3) \
  DEFINE_FOREIGN_CLASS_N(_sym, _class_name, 3, \
      scm_from_utf8_symbol(_slot1), \
      scm_from_utf8_symbol(_slot2), \
      scm_from_utf8_symbol(_slot3))

  DEFINE_FOREIGN_CLASS_2(scm_wl_event_source_type,
                         "<wl-event-source>", "source", "proc");
  DEFINE_FOREIGN_CLASS_1(scm_wl_event_loop_type,
                         "<wl-event-loop>", "loop");
  DEFINE_FOREIGN_CLASS_2(scm_wl_display_type,
                         "<wl-display-server>", "display", "filter");
  DEFINE_FOREIGN_CLASS_1(scm_wl_client_type,
                         "<wl-client>", "client");
  DEFINE_FOREIGN_CLASS_2(scm_wl_global_type,
                         "<wl-global>", "global", "bind");
  DEFINE_FOREIGN_CLASS_3(scm_wl_resource_type,
                         "<wl-resource>", "resource", "listener", "destroy");
  DEFINE_FOREIGN_CLASS_1(scm_wl_listener_type,
                         "<wl-listener>", "listener");
  DEFINE_FOREIGN_CLASS_1(scm_wl_shm_buffer_type,
                         "<wl-shm-buffer>", "buffer");
  DEFINE_FOREIGN_CLASS_1(scm_wl_shm_pool_type,
                         "<wl-shm-pool-ref>", "pool");
  DEFINE_FOREIGN_CLASS_2(scm_wl_protocol_logger_type,
                         "<wl-protocol-logger>", "logger", "proc");

#undef DEFINE_FOREIGN_CLASS_3
#undef DEFINE_FOREIGN_CLASS_2
#undef DEFINE_FOREIGN_CLASS_1
#undef DEFINE_FOREIGN_CLASS_N

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-server.x"
#endif
}


void scm_init_wayland_server(void) {
  scm_c_define_module("wayland server core",
      register_wayland_server_core, NULL);
  scm_i_init_wayland_util();
}
