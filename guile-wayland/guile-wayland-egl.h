/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: LGPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_EGL_H
#define _GUILE_WAYLAND_EGL_H

#include <libguile/scm.h>

SCM_API SCM scm_wl_egl_window_type;

SCM_API SCM scm_wl_egl_window_create(SCM surface, SCM width, SCM height);
SCM_API SCM scm_wl_egl_window_destroy(SCM egl_window);
SCM_API SCM scm_wl_egl_window_resize(SCM egl_window,
    SCM width, SCM height, SCM dx, SCM dy);
SCM_API SCM scm_wl_egl_window_get_attached_size(SCM egl_window);

void scm_init_wayland_egl(void);

#endif
