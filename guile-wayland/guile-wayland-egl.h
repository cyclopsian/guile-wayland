/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_EGL_H
#define _GUILE_WAYLAND_EGL_H

#include <libguile.h>
#include "guile-wayland-util.h"

SCM_API SCM scm_wl_egl_window_type;

#define SCM_VALIDATE_WL_EGL_WINDOW_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY(scm_wl_egl_window_type, "<wl-egl-window>", pos, k, var)

SCM_API SCM scm_wl_egl_window_create(SCM egl_window,
    SCM surface, SCM width, SCM height);
SCM_API SCM scm_wl_egl_window_destroy(SCM egl_window);
SCM_API SCM scm_wl_egl_window_resize(SCM egl_window,
    SCM width, SCM height, SCM dx, SCM dy);
SCM_API SCM scm_wl_egl_window_get_attached_size(SCM egl_window);

SCM_API void scm_init_wayland_egl(void);

#endif
