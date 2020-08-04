/* SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef _GUILE_WAYLAND_UTIL_PRIVATE_H
#define _GUILE_WAYLAND_UTIL_PRIVATE_H

#include <stdbool.h>

#include "guile-wayland-util.h"

long scm_i_signature_arg_count(const char *signature);
union wl_argument *scm_i_pack_wl_arguments(long pos, const char *subr,
    const struct wl_message *request, bool server, SCM rest);
SCM *scm_i_unpack_wl_arguments(const struct wl_message *message,
    long argc, const union wl_argument *args, bool server);
void scm_i_validate_dispatch_list(long pos, const char *subr,
    const char *iface_name, int message_count,
    const struct wl_message *messages, int extra, SCM rest);

#endif
