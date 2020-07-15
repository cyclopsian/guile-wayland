/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#define _GNU_SOURCE
#include <errno.h>
#include <libguile.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <wayland-server-core.h>

#include "guile-wayland-server.h"
#include "guile-wayland-util.h"

#define FUNC_NAME s_scm_wl_test
SCM_DEFINE_PUBLIC(scm_wl_test, "wl-test", 0, 0, 0,
    (void),
    "") {
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static void register_wayland_server_core(void *data) {
#ifndef SCM_MAGIC_SNARFER
#include "guile-wayland-server.x"
#endif
}

void scm_init_wayland_server(void) {
  scm_c_define_module("wayland server core",
      register_wayland_server_core, NULL);
  scm_i_init_wayland_util();
}
