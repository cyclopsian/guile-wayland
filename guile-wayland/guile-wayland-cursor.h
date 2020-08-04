/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_CURSOR_H
#define _GUILE_WAYLAND_CURSOR_H

#include <libguile.h>
#include "guile-wayland-util.h"

SCM_API SCM scm_wl_cursor_theme_type;
SCM_API SCM scm_wl_cursor_type;
SCM_API SCM scm_wl_cursor_image_type;

#define SCM_VALIDATE_WL_CURSOR_THEME_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY( \
      scm_wl_cursor_theme_type, "<wl-cursor-theme>", pos, k, var)

#define SCM_VALIDATE_WL_CURSOR_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY(scm_wl_cursor_type, "<wl-cursor>", pos, k, var)

#define SCM_VALIDATE_WL_CURSOR_IMAGE_COPY(pos, k, var) \
  SCM_VALIDATE_TYPE_COPY( \
      scm_wl_cursor_image_type, "<wl-cursor-image>", pos, k, var)

SCM_API SCM scm_wl_cursor_theme_load(SCM theme, SCM name, SCM size, SCM shm);
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

SCM_API void scm_init_wayland_cursor(void);

#endif
