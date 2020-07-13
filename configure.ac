# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

AC_INIT([guile-wayland], [1.0], [jason@cycles.network])
AM_INIT_AUTOMAKE([gnu -Wall -Werror foreign subdir-objects])
AC_CONFIG_MACRO_DIRS([m4])
AM_SILENT_RULES([yes])

PKG_INSTALLDIR

GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_FLAGS
AC_PATH_PROG(GUILE_SNARF,[guile-snarf-$GUILE_EFFECTIVE_VERSION])
AC_SUBST(GUILE_SNARF)

if test "$cross_compiling" != no; then
  GUILE_TARGET="--target=$host_alias"
  AC_SUBST([GUILE_TARGET])
fi

PKG_CHECK_MODULES(WAYLAND_CLIENT, wayland-client)
AC_SUBST(WAYLAND_CLIENT_LIBS)
AC_SUBST(WAYLAND_CLIENT_CFLAGS)
PKG_CHECK_MODULES(WAYLAND_CURSOR, wayland-cursor)
AC_SUBST(WAYLAND_CURSOR_LIBS)
AC_SUBST(WAYLAND_CURSOR_CFLAGS)
PKG_CHECK_MODULES(WAYLAND_EGL, wayland-egl)
AC_SUBST(WAYLAND_EGL_LIBS)
AC_SUBST(WAYLAND_EGL_CFLAGS)
WAYLAND_DATADIR=`$PKG_CONFIG --print-errors --variable=pkgdatadir wayland-client`
AC_SUBST(WAYLAND_DATADIR)

AC_PROG_CC
AM_PROG_AR
LT_INIT([disable-static])
AC_CONFIG_FILES([
Makefile
guile-wayland.pc
wayland-scanner-guile
guile-wayland/guile-wayland-client-init.c
])
AC_OUTPUT
