/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <libguile.h>

#include "guile-wayland-client.h"
#include "guile-wayland-cursor.h"
#include "guile-wayland-egl.h"

void scm_init_wayland_utils(void);

void scm_init_wayland(void) {
  scm_init_wayland_client();
  scm_init_wayland_cursor();
  scm_init_wayland_egl();
  scm_init_wayland_utils();
}
