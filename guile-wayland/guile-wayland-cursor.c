/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <libguile.h>
#include <wayland-cursor.h>

#include "guile-wayland-client.h"
#include "guile-wayland-cursor.h"

SCM scm_wl_cursor_theme_type;
SCM scm_wl_cursor_type;
SCM scm_wl_cursor_image_type;

#define FUNC_NAME s_scm_wl_cursor_theme_load
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_load, "wl-cursor-theme-load", 4, 0, 0,
    (SCM theme, SCM name, SCM size, SCM shm),
    "") {
  SCM_VALIDATE_NULL_TYPE(
      scm_wl_cursor_theme_type, "<wl-cursor-theme>", SCM_ARG1, theme);
  scm_dynwind_begin(0);
  char *c_name = NULL;
  if (!scm_is_false(name)) {
    SCM_VALIDATE_STRING(SCM_ARG2, name);
    c_name = scm_to_utf8_string(name);
    scm_dynwind_free(c_name);
  }
  int c_size;
  SCM_VALIDATE_INT_COPY(SCM_ARG3, size,  c_size);

  struct wl_shm *c_shm;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG4, shm, c_shm);

  struct wl_cursor_theme *c_theme = wl_cursor_theme_load(c_name, c_size, c_shm);
  if (!theme)
    scm_report_out_of_memory();
  scm_foreign_object_set_x(theme, 0, c_theme);
  scm_dynwind_end();
  return theme;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_theme_destroy
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_destroy,
    "wl-cursor-theme-destroy", 1, 0, 0,
    (SCM theme),
    "") {
  struct wl_cursor_theme *c_theme;
  SCM_VALIDATE_WL_CURSOR_THEME_COPY(SCM_ARG1, theme, c_theme);
  wl_cursor_theme_destroy(c_theme);
  scm_foreign_object_set_x(theme, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_theme_get_cursor
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_get_cursor,
    "wl-cursor-theme-get-cursor", 2, 0, 0,
    (SCM theme, SCM name),
    "") {
  struct wl_cursor_theme *c_theme;
  SCM_VALIDATE_WL_CURSOR_THEME_COPY(SCM_ARG1, theme, c_theme);
  SCM_VALIDATE_STRING(SCM_ARG2, name);
  scm_dynwind_begin(0);
  char *c_name = scm_to_utf8_string(name);
  scm_dynwind_free(c_name);
  struct wl_cursor *cursor = wl_cursor_theme_get_cursor(c_theme, c_name);
  scm_dynwind_end();
  if (!cursor)
    return SCM_BOOL_F;
  return scm_make_foreign_object_1(scm_wl_cursor_type, cursor);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_get_buffer
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_get_buffer,
    "wl-cursor-image-get-buffer", 1, 0, 0,
    (SCM image),
    "") {
  SCM client_proto = scm_c_resolve_module("wayland client protocol");
  SCM binterface = scm_c_module_lookup(client_proto, "wl-buffer-interface");
  struct wl_interface *buffer_interface
    = scm_foreign_object_ref(scm_variable_ref(binterface), 0);

  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  struct wl_buffer *buffer = wl_cursor_image_get_buffer(c_image);
  if (!buffer)
    scm_report_out_of_memory();
  return scm_from_wl_proxy((struct wl_proxy *) buffer);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_width
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_width, "wl-cursor-image-width", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  return scm_from_uint32(c_image->width);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_height
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_height, "wl-cursor-image-height", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  return scm_from_uint32(c_image->height);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_hotspot_x
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_hotspot_x,
    "wl-cursor-image-hotspot-x", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  return scm_from_uint32(c_image->hotspot_x);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_hotspot_y
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_hotspot_y,
    "wl-cursor-image-hotspot-y", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  return scm_from_uint32(c_image->hotspot_y);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_delay
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_delay, "wl-cursor-image-delay", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *c_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, c_image);
  return scm_from_uint32(c_image->delay);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_images
SCM_DEFINE_PUBLIC(scm_wl_cursor_images, "wl-cursor-images", 1, 0, 0,
    (SCM cursor),
    "") {
  struct wl_cursor *c_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, c_cursor);
  SCM images = SCM_EOL;
  for (unsigned int i = c_cursor->image_count; i > 0; i--) {
    SCM image = scm_make_foreign_object_1(scm_wl_cursor_image_type,
        c_cursor->images[i - 1]);
    images = scm_cons(image, images);
  }
  return images;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_name
SCM_DEFINE_PUBLIC(scm_wl_cursor_name, "wl-cursor-name", 1, 0, 0,
    (SCM cursor),
    "") {
  struct wl_cursor *c_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, c_cursor);
  return scm_from_utf8_string(c_cursor->name);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_frame_and_duration
SCM_DEFINE_PUBLIC(scm_wl_cursor_frame_and_duration,
    "wl-cursor-frame-and-duration", 2, 0, 0,
    (SCM cursor, SCM time),
    "") {
  struct wl_cursor *c_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, c_cursor);
  uint32_t c_time;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, time, c_time);

  uint32_t duration;
  int frame = wl_cursor_frame_and_duration(c_cursor, c_time, &duration);
  return scm_cons(scm_from_int(frame), scm_from_uint32(duration));
}
#undef FUNC_NAME

void scm_init_wayland_cursor(void) {
  static const char s_scm_wl_cursor_theme[] = "<wl-cursor-theme>";
  scm_wl_cursor_theme_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_cursor_theme),
      scm_list_1(scm_from_utf8_symbol("cursor-theme")),
      NULL);
  scm_c_define(s_scm_wl_cursor_theme, scm_wl_cursor_theme_type);
  scm_c_export(s_scm_wl_cursor_theme, NULL);

  static const char s_scm_wl_cursor[] = "<wl-cursor>";
  scm_wl_cursor_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_cursor),
      scm_list_1(scm_from_utf8_symbol("cursor")),
      NULL);
  scm_c_define(s_scm_wl_cursor, scm_wl_cursor_type);
  scm_c_export(s_scm_wl_cursor, NULL);

  static const char s_scm_wl_cursor_image[] = "<wl-cursor-image>";
  scm_wl_cursor_image_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol(s_scm_wl_cursor_image),
      scm_list_1(scm_from_utf8_symbol("cursor-image")),
      NULL);
  scm_c_define(s_scm_wl_cursor_image, scm_wl_cursor_image_type);
  scm_c_export(s_scm_wl_cursor_image, NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-cursor.x"
#endif
}
