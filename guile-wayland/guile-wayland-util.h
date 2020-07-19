/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_UTIL_H
#define _GUILE_WAYLAND_UTIL_H

#include <libguile/scm.h>
#include <stdbool.h>
#include <wayland-util.h>

SCM_API SCM scm_wl_interface_type;

#define SCM_VALIDATE_WL_INTERFACE_COPY(pos, i, interface) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(i, scm_wl_interface_type), \
        i, pos, FUNC_NAME, "<wl-interface>"); \
    interface = scm_foreign_object_ref(i, 0); \
    SCM_ASSERT_TYPE(interface, i, pos, FUNC_NAME, "non-null <wl-interface>"); \
  } while (0)

SCM_API SCM scm_make_wl_interface(void);
SCM_API SCM scm_wl_interface_set(
    SCM interface, SCM name, SCM version, SCM methods, SCM events);
SCM_API SCM scm_wl_interface_name(SCM interface);

#endif
