/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_CLIENT_H
#define _GUILE_WAYLAND_CLIENT_H

#include <libguile/scm.h>
#include <wayland-client-core.h>

SCM_API SCM scm_wl_event_queue_type;
SCM_API SCM scm_wl_proxy_type;
SCM_API SCM scm_wl_interface_type;

#define SCM_VALIDATE_WL_PROXY_COPY(pos, p, proxy, interface) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(p, scm_wl_proxy_type), \
        p, pos, FUNC_NAME, "wl-proxy"); \
    proxy     = scm_foreign_object_ref(p, 0); \
    interface = scm_foreign_object_ref(p, 1); \
    SCM_ASSERT_TYPE(proxy,     p, pos, FUNC_NAME, "non-null wl-proxy"); \
    SCM_ASSERT_TYPE(interface, p, pos, FUNC_NAME, "non-null wl-interface"); \
  } while (0)

SCM_API SCM scm_make_wl_interface(void);
SCM_API SCM scm_wl_interface_set(
    SCM interface, SCM name, SCM version, SCM methods, SCM events);
SCM_API SCM scm_wl_interface_name(SCM interface);
SCM_API SCM scm_c_make_wl_proxy(struct wl_proxy *proxy,
    const struct wl_interface *interface);

SCM_API SCM scm_wl_event_queue_destroy(SCM queue);
SCM_API SCM scm_wl_proxy_marshal(SCM proxy, SCM opcode, SCM rest);
SCM_API SCM scm_wl_proxy_create_wrapper(SCM proxy);
SCM_API SCM scm_wl_proxy_wrapper_destroy(SCM wrapper);
SCM_API SCM scm_wl_proxy_marshal_constructor(
    SCM proxy, SCM opcode, SCM interface, SCM rest);
SCM_API SCM scm_wl_proxy_marshal_constructor_versioned(
    SCM proxy, SCM opcode, SCM interface, SCM version, SCM rest);
SCM_API SCM scm_wl_proxy_add_listener(SCM proxy, SCM rest);
SCM_API SCM scm_wl_proxy_destroy(SCM proxy);
SCM_API SCM scm_wl_proxy_get_version(SCM proxy);
SCM_API SCM scm_wl_proxy_get_id(SCM proxy);
SCM_API SCM scm_wl_proxy_get_class(SCM proxy);
SCM_API SCM scm_wl_proxy_set_queue(SCM proxy, SCM queue);
SCM_API SCM scm_wl_proxy_assert_type(SCM proxy, SCM interface);
SCM_API SCM scm_wl_proxy_move(SCM src, SCM dst);

SCM_API SCM scm_wl_display_connect(SCM name);
SCM_API SCM scm_wl_display_connect_to_fd(SCM fd);
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

void scm_init_wayland_client(void);

#endif
