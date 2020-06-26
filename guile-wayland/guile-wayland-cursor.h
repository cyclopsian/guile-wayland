/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_CURSOR_H
#define _GUILE_WAYLAND_CURSOR_H

#include <libguile/scm.h>

SCM_API SCM scm_wl_cursor_theme_type;
SCM_API SCM scm_wl_cursor_type;
SCM_API SCM scm_wl_cursor_image_type;

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

void scm_init_wayland_cursor(void);

#endif
