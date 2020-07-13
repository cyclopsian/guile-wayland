/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <libguile.h>
#include <wayland-egl.h>

#include "guile-wayland-client.h"
#include "guile-wayland-egl.h"

SCM scm_wl_egl_window_type;

#define FUNC_NAME s_scm_wl_egl_window_create
SCM_DEFINE_PUBLIC(scm_wl_egl_window_create, "wl-egl-window-create", 3, 0, 0,
    (SCM surface, SCM width, SCM height),
    "") {
  SCM client_proto = scm_c_resolve_module("wayland client protocol");
  SCM sinterface = scm_c_module_lookup(client_proto, "wl-surface-interface");
  struct wl_interface *surface_interface
    = scm_foreign_object_ref(sinterface, 0);

  struct wl_surface *i_surface;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG1, surface, i_surface, interface); \
  SCM_ASSERT_TYPE(interface == surface_interface,
      surface, SCM_ARG1, FUNC_NAME, interface->name);

  int i_width, i_height;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, width,  i_width);
  SCM_VALIDATE_INT_COPY(SCM_ARG3, height, i_height);

  struct wl_egl_window *egl_window
    = wl_egl_window_create(i_surface, i_width, i_height);
  return scm_make_foreign_object_1(scm_wl_egl_window_type, egl_window);
}
#undef FUNC_NAME

#define SCM_VALIDATE_WL_EGL_WINDOW_COPY(pos, w, egl_window) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(w, scm_wl_egl_window_type), \
        w, pos, FUNC_NAME, "wl-egl-window"); \
    egl_window = scm_foreign_object_ref(w, 0); \
    SCM_ASSERT_TYPE(egl_window, w, pos, FUNC_NAME, "non-null wl_egl_window"); \
  } while (0)

#define FUNC_NAME s_scm_wl_egl_window_destroy
SCM_DEFINE_PUBLIC(scm_wl_egl_window_destroy, "wl-egl-window-destroy", 1, 0, 0,
    (SCM egl_window),
    "") {
  struct wl_egl_window *i_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, i_egl_window);
  wl_egl_window_destroy(i_egl_window);
  scm_foreign_object_set_x(egl_window, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_egl_window_resize
SCM_DEFINE_PUBLIC(scm_wl_egl_window_resize, "wl-egl-window-resize", 5, 0, 0,
    (SCM egl_window, SCM width, SCM height, SCM dx, SCM dy),
    "") {
  struct wl_egl_window *i_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, i_egl_window);
  int i_width, i_height, i_dx, i_dy;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, width,  i_width);
  SCM_VALIDATE_INT_COPY(SCM_ARG3, height, i_height);
  SCM_VALIDATE_INT_COPY(SCM_ARG4, dx,     i_dx);
  SCM_VALIDATE_INT_COPY(SCM_ARG5, dy,     i_dy);
  wl_egl_window_resize(i_egl_window, i_width, i_height, i_dx, i_dy);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_egl_window_get_attached_size
SCM_DEFINE_PUBLIC(scm_wl_egl_window_get_attached_size,
    "wl-egl-window-get-attached-size", 1, 0, 0,
    (SCM egl_window),
    "") {
  struct wl_egl_window *i_egl_window;
  SCM_VALIDATE_WL_EGL_WINDOW_COPY(SCM_ARG1, egl_window, i_egl_window);
  int width, height;
  wl_egl_window_get_attached_size(i_egl_window, &width, &height);
  return scm_cons(scm_from_int(width), scm_from_int(height));
}
#undef FUNC_NAME

static void register_wayland_egl(void *data) {
  scm_wl_egl_window_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol("<wl-egl-window>"),
      scm_list_1(scm_from_utf8_symbol("egl-window")),
      NULL);
  scm_c_export("<wl-egl-window>", scm_wl_egl_window_type, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-egl.x"
#endif
}

void scm_init_wayland_egl(void) {
  scm_c_define_module("wayland egl core", register_wayland_egl, NULL);
}
