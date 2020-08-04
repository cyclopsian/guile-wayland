/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <libguile.h>
#include <wayland-egl.h>

#include "guile-wayland-client.h"
#include "guile-wayland-egl.h"

SCM scm_wl_egl_window_type;

#define FUNC_NAME s_scm_wl_egl_window_create
SCM_DEFINE_PUBLIC(scm_wl_egl_window_create, "wl-egl-window-create", 4, 0, 0,
    (SCM egl_window, SCM surface, SCM width, SCM height),
    "") {
  SCM_VALIDATE_NULL_TYPE(scm_wl_egl_window_type, "<wl-egl-window>",
      SCM_ARG1, egl_window);
  struct wl_surface *c_surface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG2, surface, c_surface);

  int c_width, c_height;
  SCM_VALIDATE_INT_COPY(SCM_ARG3, width,  c_width);
  SCM_VALIDATE_INT_COPY(SCM_ARG4, height, c_height);

  struct wl_egl_window *c_egl_window
    = wl_egl_window_create(c_surface, c_width, c_height);
  scm_foreign_object_set_x(egl_window, 0, c_egl_window);
  return egl_window;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_egl_window_destroy
SCM_DEFINE_PUBLIC(scm_wl_egl_window_destroy, "wl-egl-window-destroy", 1, 0, 0,
    (SCM egl_window),
    "") {
  struct wl_egl_window *c_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, c_egl_window);
  wl_egl_window_destroy(c_egl_window);
  scm_foreign_object_set_x(egl_window, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_egl_window_resize
SCM_DEFINE_PUBLIC(scm_wl_egl_window_resize, "wl-egl-window-resize", 5, 0, 0,
    (SCM egl_window, SCM width, SCM height, SCM dx, SCM dy),
    "") {
  struct wl_egl_window *c_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, c_egl_window);
  int c_width, c_height, c_dx, c_dy;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, width,  c_width);
  SCM_VALIDATE_INT_COPY(SCM_ARG3, height, c_height);
  SCM_VALIDATE_INT_COPY(SCM_ARG4, dx,     c_dx);
  SCM_VALIDATE_INT_COPY(SCM_ARG5, dy,     c_dy);
  wl_egl_window_resize(c_egl_window, c_width, c_height, c_dx, c_dy);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_egl_window_get_attached_size
SCM_DEFINE_PUBLIC(scm_wl_egl_window_get_attached_size,
    "wl-egl-window-get-attached-size", 1, 0, 0,
    (SCM egl_window),
    "") {
  struct wl_egl_window *c_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, c_egl_window);
  int width, height;
  wl_egl_window_get_attached_size(c_egl_window, &width, &height);
  return scm_cons(scm_from_int(width), scm_from_int(height));
}
#undef FUNC_NAME

void scm_init_wayland_egl(void) {
  static const char s_scm_wl_egl_window[] = "<wl-egl-window>";
  scm_wl_egl_window_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_egl_window),
      scm_list_1(scm_from_utf8_symbol("egl-window")),
      NULL);
  scm_c_define(s_scm_wl_egl_window, scm_wl_egl_window_type);
  scm_c_export(s_scm_wl_egl_window, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-egl.x"
#endif
}
