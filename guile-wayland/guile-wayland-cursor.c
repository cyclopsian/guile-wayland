/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <libguile.h>
#include <wayland-cursor.h>

#include "guile-wayland-client.h"
#include "guile-wayland-cursor.h"

SCM scm_wl_cursor_theme_type;
SCM scm_wl_cursor_type;
SCM scm_wl_cursor_image_type;

#define SCM_VALIDATE_WL_CURSOR_THEME_COPY(pos, c, cursor_theme) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_theme_type), \
        c, pos, FUNC_NAME, "wl-cursor-theme"); \
    cursor_theme = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor_theme, c, pos, FUNC_NAME, "non-null wl_cursor_theme"); \
  } while (0)

#define SCM_VALIDATE_WL_CURSOR_COPY(pos, c, cursor) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_type), \
        c, pos, FUNC_NAME, "wl-cursor"); \
    cursor = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor, c, pos, FUNC_NAME, "non-null wl_cursor"); \
  } while (0)

#define SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(pos, c, cursor_image) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_image_type), \
        c, pos, FUNC_NAME, "wl-cursor-image"); \
    cursor_image = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor_image, c, pos, FUNC_NAME, "non-null wl_cursor_image"); \
  } while (0)

#define FUNC_NAME s_scm_wl_cursor_theme_load
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_load, "wl-cursor-theme-load", 3, 0, 0,
    (SCM name, SCM size, SCM shm),
    "") {
  SCM client_proto = scm_c_resolve_module("wayland client protocol");
  SCM sinterface = scm_c_module_lookup(client_proto, "wl-shm-interface");
  struct wl_interface *shm_interface = scm_foreign_object_ref(sinterface, 0);

  scm_dynwind_begin(0);
  char *i_name = NULL;
  if (!scm_is_false(name)) {
    SCM_VALIDATE_STRING(SCM_ARG1, name);
    i_name = scm_to_utf8_string(name);
    scm_dynwind_free(i_name);
  }
  int i_size;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, size,  i_size);

  struct wl_shm *i_shm;
  struct wl_interface *interface;
  SCM_VALIDATE_WL_PROXY_COPY(SCM_ARG3, shm, i_shm, interface); \
  SCM_ASSERT_TYPE(interface == shm_interface,
      shm, SCM_ARG3, FUNC_NAME, interface->name);

  struct wl_cursor_theme *theme = wl_cursor_theme_load(i_name, i_size, i_shm);
  if (!theme)
    scm_report_out_of_memory();
  SCM ret = scm_make_foreign_object_1(scm_wl_cursor_theme_type, theme);
  scm_dynwind_end();
  return ret;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_theme_destroy
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_destroy,
    "wl-cursor-theme-destroy", 1, 0, 0,
    (SCM theme),
    "") {
  struct wl_cursor_theme *i_theme;
  SCM_VALIDATE_WL_CURSOR_THEME_COPY(SCM_ARG1, theme, i_theme);
  wl_cursor_theme_destroy(i_theme);
  scm_foreign_object_set_x(theme, 0, NULL);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_theme_get_cursor
SCM_DEFINE_PUBLIC(scm_wl_cursor_theme_get_cursor,
    "wl-cursor-theme-get-cursor", 2, 0, 0,
    (SCM theme, SCM name),
    "") {
  struct wl_cursor_theme *i_theme;
  SCM_VALIDATE_WL_CURSOR_THEME_COPY(SCM_ARG1, theme, i_theme);
  SCM_VALIDATE_STRING(SCM_ARG2, name);
  scm_dynwind_begin(0);
  char *i_name = scm_to_utf8_string(name);
  scm_dynwind_free(i_name);
  struct wl_cursor *cursor = wl_cursor_theme_get_cursor(i_theme, i_name);
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
  struct wl_interface *buffer_interface = scm_foreign_object_ref(binterface, 0);

  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  struct wl_buffer *buffer = wl_cursor_image_get_buffer(i_image);
  if (!buffer)
    scm_report_out_of_memory();
  return scm_c_make_wl_proxy((struct wl_proxy *) buffer, buffer_interface);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_width
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_width, "wl-cursor-image-width", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  return scm_from_uint32(i_image->width);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_height
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_height, "wl-cursor-image-height", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  return scm_from_uint32(i_image->height);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_hotspot_x
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_hotspot_x,
    "wl-cursor-image-hotspot-x", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  return scm_from_uint32(i_image->hotspot_x);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_hotspot_y
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_hotspot_y,
    "wl-cursor-image-hotspot-y", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  return scm_from_uint32(i_image->hotspot_y);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_image_delay
SCM_DEFINE_PUBLIC(scm_wl_cursor_image_delay, "wl-cursor-image-delay", 1, 0, 0,
    (SCM image),
    "") {
  struct wl_cursor_image *i_image;
  SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(SCM_ARG1, image, i_image);
  return scm_from_uint32(i_image->delay);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_images
SCM_DEFINE_PUBLIC(scm_wl_cursor_images, "wl-cursor-images", 1, 0, 0,
    (SCM cursor),
    "") {
  struct wl_cursor *i_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, i_cursor);
  SCM images = SCM_EOL;
  for (unsigned int i = i_cursor->image_count; i > 0; i--) {
    SCM image = scm_make_foreign_object_1(scm_wl_cursor_image_type,
        i_cursor->images[i - 1]);
    images = scm_cons(image, images);
  }
  return images;
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_name
SCM_DEFINE_PUBLIC(scm_wl_cursor_name, "wl-cursor-name", 1, 0, 0,
    (SCM cursor),
    "") {
  struct wl_cursor *i_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, i_cursor);
  return scm_from_utf8_string(i_cursor->name);
}
#undef FUNC_NAME

#define FUNC_NAME s_scm_wl_cursor_frame_and_duration
SCM_DEFINE_PUBLIC(scm_wl_cursor_frame_and_duration,
    "wl-cursor-frame-and-duration", 2, 0, 0,
    (SCM cursor, SCM time),
    "") {
  struct wl_cursor *i_cursor;
  SCM_VALIDATE_WL_CURSOR_COPY(SCM_ARG1, cursor, i_cursor);
  uint32_t i_time;
  SCM_VALIDATE_INT_COPY(SCM_ARG2, time, i_time);

  uint32_t duration;
  int frame = wl_cursor_frame_and_duration(i_cursor, i_time, &duration);
  return scm_cons(scm_from_int(frame), scm_from_uint32(duration));
}
#undef FUNC_NAME

static void register_wayland_cursor(void *data) {
  scm_wl_cursor_theme_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol("wl_cursor_theme"),
      scm_list_1(scm_from_utf8_symbol("cursor_theme")),
      NULL);
  scm_wl_cursor_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol("wl_cursor"),
      scm_list_1(scm_from_utf8_symbol("cursor")),
      NULL);
  scm_wl_cursor_image_type = scm_make_foreign_object_type(
      scm_from_utf8_symbol("wl_cursor_image"),
      scm_list_1(scm_from_utf8_symbol("cursor_image")),
      NULL);

#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-cursor.x"
#endif
}

void scm_init_wayland_cursor(void) {
  scm_c_define_module("wayland cursor", register_wayland_cursor, NULL);
}
