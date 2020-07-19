/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_SERVER_H
#define _GUILE_WAYLAND_SERVER_H

#include <libguile/scm.h>
#include <wayland-server-core.h>

SCM_API SCM scm_wl_resource_class_type;
SCM_API SCM scm_wl_event_source_type;
SCM_API SCM scm_wl_event_loop_type;
SCM_API SCM scm_wl_display_type;
SCM_API SCM scm_wl_client_type;
SCM_API SCM scm_wl_global_type;
SCM_API SCM scm_wl_resource_type;
SCM_API SCM scm_wl_listener_type;
SCM_API SCM scm_wl_shm_buffer_type;
SCM_API SCM scm_wl_shm_pool_type;
SCM_API SCM scm_wl_protocol_logger_type;

struct scm_wl_listener {
  struct wl_listener listener;
  SCM proc;
};

#define SCM_VALIDATE_WL_EVENT_SOURCE_COPY(pos, k, source) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_event_source_type), \
        k, pos, FUNC_NAME, "<wl-event-source>"); \
    source = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(source, k, pos, FUNC_NAME, "non-null <wl-event-source>"); \
  } while (0)

#define SCM_VALIDATE_WL_EVENT_LOOP_COPY(pos, k, loop) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_event_loop_type), \
        k, pos, FUNC_NAME, "<wl-event-loop>"); \
    loop = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(loop, k, pos, FUNC_NAME, "non-null <wl-event-loop>"); \
  } while (0)

#define SCM_VALIDATE_WL_DISPLAY_COPY(pos, k, display) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_display_type), \
        k, pos, FUNC_NAME, "<wl-display-server>"); \
    display = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(display, k, pos, FUNC_NAME, "non-null <wl-display-server>"); \
  } while (0)

#define SCM_VALIDATE_WL_CLIENT_COPY(pos, k, client) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_client_type), \
        k, pos, FUNC_NAME, "<wl-client>"); \
    client = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(client, k, pos, FUNC_NAME, "non-null <wl-client>"); \
  } while (0)

#define SCM_VALIDATE_WL_GLOBAL_COPY(pos, k, global) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_global_type), \
        k, pos, FUNC_NAME, "<wl-global>"); \
    global = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(global, k, pos, FUNC_NAME, "non-null <wl-global>"); \
  } while (0)

#define SCM_VALIDATE_WL_RESOURCE_COPY(pos, k, resource) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_resource_type), \
        k, pos, FUNC_NAME, "<wl-resource>"); \
    resource = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(resource, k, pos, FUNC_NAME, "non-null <wl-resource>"); \
  } while (0)

#define SCM_VALIDATE_WL_LISTENER_COPY(pos, k, listener) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_listener_type), \
        k, pos, FUNC_NAME, "<wl-listener>"); \
    listener = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(listener, k, pos, FUNC_NAME, "non-null <wl-listener>"); \
  } while (0)

#define SCM_VALIDATE_WL_SHM_BUFFER_COPY(pos, k, shm_buffer) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_shm_buffer_type), \
        k, pos, FUNC_NAME, "<wl-shm-buffer>"); \
    shm_buffer = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(shm_buffer, k, pos, FUNC_NAME, "non-null <wl-shm-buffer>"); \
  } while (0)

#define SCM_VALIDATE_WL_SHM_POOL_COPY(pos, k, shm_pool) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_shm_pool_type), \
        k, pos, FUNC_NAME, "<wl-shm-pool-ref>"); \
    shm_pool = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(shm_pool, k, pos, FUNC_NAME, "non-null <wl-shm-pool-ref>"); \
  } while (0)

#define SCM_VALIDATE_WL_PROTOCOL_LOGGER_COPY(pos, k, protocol_logger) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_protocol_logger_type), \
        k, pos, FUNC_NAME, "<wl-protocol-logger>"); \
    protocol_logger = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(protocol_logger, k, pos, FUNC_NAME, \
        "non-null <wl-protocol-logger>"); \
  } while (0)

SCM_API SCM scm_from_wl_client(struct wl_client *c_client);
SCM_API SCM scm_from_wl_global(struct wl_global *c_global);
SCM_API SCM scm_from_wl_resource(struct wl_resource *c_resource);

SCM_API SCM scm_wl_listener_destroy(SCM listener);
SCM_API SCM scm_wl_event_loop_create(void);
SCM_API SCM scm_wl_event_loop_destroy(SCM loop);
SCM_API SCM scm_wl_event_loop_add_fd(SCM loop, SCM fd, SCM mask, SCM proc);
SCM_API SCM scm_wl_event_source_fd_update(SCM source, SCM mask);
SCM_API SCM scm_wl_event_loop_add_timer(SCM loop, SCM thunk);
SCM_API SCM scm_wl_event_loop_add_signal(SCM loop, SCM signal, SCM thunk);
SCM_API SCM scm_wl_event_source_timer_update(SCM source, SCM ms_delay);
SCM_API SCM scm_wl_event_source_remove(SCM source);
SCM_API SCM scm_wl_event_source_check(SCM source);
SCM_API SCM scm_wl_event_loop_dispatch(SCM loop, SCM timeout);
SCM_API SCM scm_wl_event_loop_dispatch_idle(SCM loop);
SCM_API SCM scm_wl_event_loop_add_idle(SCM loop, SCM thunk);
SCM_API SCM scm_wl_event_loop_get_fd(SCM loop);
SCM_API SCM scm_wl_event_loop_add_destroy_listener(SCM loop, SCM thunk);

SCM_API SCM scm_wl_display_create(void);
SCM_API SCM scm_wl_display_destroy(SCM display);
SCM_API SCM scm_wl_display_get_event_loop(SCM display);
SCM_API SCM scm_wl_display_add_socket(SCM display, SCM name);
SCM_API SCM scm_wl_display_add_socket_auto(SCM display);
SCM_API SCM scm_wl_display_add_socket_fd(SCM display, SCM fd);
SCM_API SCM scm_wl_display_terminate(SCM display);
SCM_API SCM scm_wl_display_run(SCM display);
SCM_API SCM scm_wl_display_flush_clients(SCM display);
SCM_API SCM scm_wl_display_destroy_clients(SCM display);
SCM_API SCM scm_wl_display_get_serial(SCM display);
SCM_API SCM scm_wl_display_next_serial(SCM display);
SCM_API SCM scm_wl_display_add_destroy_listener(SCM display, SCM thunk);
SCM_API SCM scm_wl_display_add_client_created_listener(SCM display, SCM proc);

SCM_API SCM scm_wl_global_create(SCM global, SCM display, SCM interface, SCM version, SCM proc);
SCM_API SCM scm_wl_global_remove(SCM global);
SCM_API SCM scm_wl_global_destroy(SCM global);
SCM_API SCM scm_wl_global_get_interface(SCM global);

SCM_API SCM scm_wl_client_create(SCM display, SCM fd);
SCM_API SCM scm_wl_display_get_client_list(SCM display);
SCM_API SCM scm_wl_client_destroy(SCM client);
SCM_API SCM scm_wl_client_flush(SCM client);
SCM_API SCM scm_wl_client_get_credentials(SCM client);
SCM_API SCM scm_wl_client_get_fd(SCM client);
SCM_API SCM scm_wl_client_add_destroy_listener(SCM client, SCM thunk);
SCM_API SCM scm_wl_client_get_object(SCM client, SCM id);
SCM_API SCM scm_wl_client_add_resource_created_listener(SCM client, SCM proc);
SCM_API SCM scm_wl_client_for_each_resource(SCM client, SCM proc);

SCM_API SCM scm_wl_resource_post_event(SCM resource, SCM interface, SCM opcode, SCM rest);
SCM_API SCM scm_wl_resource_queue_event(SCM resource, SCM interface, SCM opcode, SCM rest);
SCM_API SCM scm_wl_resource_create(SCM resource, SCM client, SCM interface, SCM version, SCM id);
SCM_API SCM scm_wl_resource_set_implementation(SCM resource, SCM interface, SCM rest);
SCM_API SCM scm_wl_resource_destroy(SCM resource);
SCM_API SCM scm_wl_resource_get_id(SCM resource);
SCM_API SCM scm_wl_resource_get_client(SCM resource);
SCM_API SCM scm_wl_resource_get_version(SCM resource);
SCM_API SCM scm_wl_resource_set_destructor(SCM resource, SCM destructor);
SCM_API SCM scm_wl_resource_get_class(SCM resource);
SCM_API SCM scm_wl_resource_add_destroy_listener(SCM resource, SCM thunk);

SCM_API SCM scm_wl_shm_buffer_get(SCM resource);
SCM_API SCM scm_wl_shm_buffer_begin_access(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_end_access(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_get_data(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_get_stride(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_get_format(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_get_width(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_get_height(SCM buffer);
SCM_API SCM scm_wl_shm_buffer_ref_pool(SCM buffer);
SCM_API SCM scm_wl_shm_pool_unref(SCM pool);
SCM_API SCM scm_wl_display_init_shm(SCM display);
SCM_API SCM scm_wl_display_add_shm_format(SCM display, SCM format);

SCM_API SCM scm_wl_set_log_port_server(SCM port);
SCM_API SCM scm_wl_display_add_protocol_logger(SCM display, SCM proc);
SCM_API SCM scm_wl_protocol_logger_destroy(SCM logger);

SCM_API void scm_init_wayland_server(void);

#endif
