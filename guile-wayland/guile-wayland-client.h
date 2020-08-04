/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_CLIENT_H
#define _GUILE_WAYLAND_CLIENT_H

#include <libguile.h>
#include <wayland-client-core.h>
#include "guile-wayland-util.h"

SCM_API SCM scm_wl_proxy_class_type;
SCM_API SCM scm_wl_event_queue_type;
SCM_API SCM scm_wl_proxy_type;

#define SCM_VALIDATE_WL_PROXY_CLASS(pos, k) \
  SCM_ASSERT_TYPE(SCM_IS_A_P(k, scm_wl_proxy_class_type), \
      k, pos, FUNC_NAME, "<wl-proxy-class>")

#define SCM_VALIDATE_WL_EVENT_QUEUE_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY( \
      scm_wl_event_queue_type, "<wl-event-queue>", pos, k, var)

#define SCM_VALIDATE_WL_PROXY_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY(scm_wl_proxy_type, "<wl-proxy>", pos, k, var)

SCM_API SCM scm_from_wl_proxy(struct wl_proxy *proxy);

SCM_API SCM scm_wl_event_queue_destroy(SCM queue);
SCM_API SCM scm_wl_proxy_marshal(
    SCM proxy, SCM interface, SCM opcode, SCM rest);
SCM_API SCM scm_wl_proxy_create_wrapper(SCM proxy);
SCM_API SCM scm_wl_proxy_wrapper_destroy(SCM wrapper);
SCM_API SCM scm_wl_proxy_marshal_constructor(
    SCM proxy, SCM interface, SCM opcode, SCM ret_interface, SCM rest);
SCM_API SCM scm_wl_proxy_marshal_constructor_versioned(
    SCM proxy, SCM interface, SCM opcode, SCM ret_interface, SCM version,
    SCM rest);
SCM_API SCM scm_wl_proxy_add_listener(SCM proxy, SCM interface, SCM rest);
SCM_API SCM scm_wl_proxy_destroy(SCM proxy);
SCM_API SCM scm_wl_proxy_get_version(SCM proxy);
SCM_API SCM scm_wl_proxy_get_id(SCM proxy);
SCM_API SCM scm_wl_proxy_get_class(SCM proxy);
SCM_API SCM scm_wl_proxy_set_queue(SCM proxy, SCM queue);
SCM_API SCM scm_wl_proxy_cast(SCM proxy, SCM proxy_class);

SCM_API SCM scm_wl_display_connect(SCM display, SCM name);
SCM_API SCM scm_wl_display_connect_to_fd(SCM display, SCM fd);
SCM_API SCM scm_wl_display_disconnect(SCM display);
SCM_API SCM scm_wl_display_get_fd(SCM display);
SCM_API SCM scm_wl_display_dispatch(SCM display);
SCM_API SCM scm_wl_display_dispatch_queue(SCM display, SCM queue);
SCM_API SCM scm_wl_display_dispatch_queue_pending(SCM display, SCM queue);
SCM_API SCM scm_wl_display_dispatch_pending(SCM display);
SCM_API SCM scm_wl_display_get_error(SCM display);
SCM_API SCM scm_wl_display_get_protocol_error(SCM display);
SCM_API SCM scm_wl_display_flush(SCM display);
SCM_API SCM scm_wl_display_roundtrip_queue(SCM display, SCM queue);
SCM_API SCM scm_wl_display_roundtrip(SCM display);
SCM_API SCM scm_wl_display_create_queue(SCM display);
SCM_API SCM scm_wl_display_prepare_read_queue(SCM display, SCM queue);
SCM_API SCM scm_wl_display_prepare_read(SCM display);
SCM_API SCM scm_wl_display_cancel_read(SCM display);
SCM_API SCM scm_wl_display_read_events(SCM display);

SCM_API SCM scm_wl_set_log_port_client(SCM port);

SCM_API void scm_init_wayland_client_core(void);

#endif
