/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_UTIL_H
#define _GUILE_WAYLAND_UTIL_H

#include <libguile.h>
#include <stdbool.h>
#include <wayland-util.h>

SCM_API SCM scm_wl_interface_type;

#define SCM_VALIDATE_TYPE_COPY(type, name, pos, k, var) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, type), \
        k, pos, FUNC_NAME, name); \
    var = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(var, k, pos, FUNC_NAME, "non-null " name); \
  } while (0)

#define SCM_VALIDATE_NULL_TYPE(type, name, pos, k) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(k, type), \
        k, pos, FUNC_NAME, name); \
    void *ptr = scm_foreign_object_ref(k, 0); \
    SCM_ASSERT_TYPE(!ptr, k, pos, FUNC_NAME, "null " name); \
  } while (0)

#define SCM_VALIDATE_WL_INTERFACE_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY(scm_wl_interface_type, "<wl-interface>", pos, k, var)

SCM_API SCM scm_make_wl_interface(void);
SCM_API SCM scm_wl_interface_set(
    SCM interface, SCM name, SCM version, SCM methods, SCM events);
SCM_API SCM scm_wl_interface_name(SCM interface);

SCM_API void scm_init_wayland_util(void);

#endif
