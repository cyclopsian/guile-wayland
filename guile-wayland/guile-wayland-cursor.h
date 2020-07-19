/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_CURSOR_H
#define _GUILE_WAYLAND_CURSOR_H

#include <libguile/scm.h>

SCM_API SCM scm_wl_cursor_theme_type;
SCM_API SCM scm_wl_cursor_type;
SCM_API SCM scm_wl_cursor_image_type;

#define SCM_VALIDATE_WL_CURSOR_THEME_COPY(pos, c, cursor_theme) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_theme_type), \
        c, pos, FUNC_NAME, "<wl-cursor-theme>"); \
    cursor_theme = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor_theme, c, pos, FUNC_NAME, "non-null <wl-cursor-theme>"); \
  } while (0)

#define SCM_VALIDATE_WL_CURSOR_COPY(pos, c, cursor) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_type), \
        c, pos, FUNC_NAME, "<wl-cursor>"); \
    cursor = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor, c, pos, FUNC_NAME, "non-null <wl-cursor>"); \
  } while (0)

#define SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(pos, c, cursor_image) \
  do { \
    SCM_ASSERT_TYPE(SCM_IS_A_P(c, scm_wl_cursor_image_type), \
        c, pos, FUNC_NAME, "<wl-cursor-image>"); \
    cursor_image = scm_foreign_object_ref(c, 0); \
    SCM_ASSERT_TYPE(cursor_image, c, pos, FUNC_NAME, "non-null <wl-cursor-image>"); \
  } while (0)

SCM_API SCM scm_wl_cursor_theme_load(SCM name, SCM size, SCM shm);
SCM_API SCM scm_wl_cursor_theme_destroy(SCM theme);
SCM_API SCM scm_wl_cursor_theme_get_cursor(SCM theme, SCM name);
SCM_API SCM scm_wl_cursor_image_get_buffer(SCM image);
SCM_API SCM scm_wl_cursor_image_width(SCM image);
SCM_API SCM scm_wl_cursor_image_height(SCM image);
SCM_API SCM scm_wl_cursor_image_hotspot_x(SCM image);
SCM_API SCM scm_wl_cursor_image_hotspot_y(SCM image);
SCM_API SCM scm_wl_cursor_image_delay(SCM image);
SCM_API SCM scm_wl_cursor_frame_and_duration(SCM cursor, SCM time);
SCM_API SCM scm_wl_cursor_images(SCM cursor);
SCM_API SCM scm_wl_cursor_name(SCM cursor);

void scm_i_init_wayland_cursor(void);

#endif
