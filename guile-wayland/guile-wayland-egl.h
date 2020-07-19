/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_EGL_H
#define _GUILE_WAYLAND_EGL_H

#include <libguile/scm.h>

SCM_API SCM scm_wl_egl_window_type;

#define SCM_VALIDATE_WL_EGL_WINDOW_COPY(pos, w, egl_window) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(w, scm_wl_egl_window_type), \
        w, pos, FUNC_NAME, "<wl-egl-window>"); \
    egl_window = scm_foreign_object_ref(w, 0); \
    SCM_ASSERT_TYPE(egl_window, w, pos, FUNC_NAME, "non-null <wl-egl-window>"); \
  } while (0)

SCM_API SCM scm_wl_egl_window_create(SCM surface, SCM width, SCM height);
SCM_API SCM scm_wl_egl_window_destroy(SCM egl_window);
SCM_API SCM scm_wl_egl_window_resize(SCM egl_window,
    SCM width, SCM height, SCM dx, SCM dy);
SCM_API SCM scm_wl_egl_window_get_attached_size(SCM egl_window);

void scm_i_init_wayland_egl(void);

#endif
