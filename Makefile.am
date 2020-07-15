# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

guileextension_LTLIBRARIES = libguile-wayland.la
guileextensiondir = $(GUILE_EXTENSION)

moddir = $(GUILE_SITE)
objdir = $(GUILE_SITE_CCACHE)

libguile_wayland_la_includedir = $(includedir)/guile-wayland
libguile_wayland_la_include_HEADERS = \
	guile-wayland/guile-wayland-client.h \
	guile-wayland/guile-wayland-cursor.h \
	guile-wayland/guile-wayland-egl.h
libguile_wayland_la_SOURCES = \
	guile-wayland/guile-wayland-cursor.c \
	guile-wayland/guile-wayland-egl.c \
	guile-wayland/guile-wayland-client.c \
	guile-wayland/guile-wayland-utils.c
libguile_wayland_la_CFLAGS = $(WAYLAND_CLIENT_CFLAGS) $(WAYLAND_CURSOR_CFLAGS) $(WAYLAND_EGL_CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD = $(WAYLAND_CLIENT_LIBS) $(WAYLAND_CURSOR_LIBS) $(WAYLAND_EGL_LIBS) $(GUILE_LIBS)
libguile_wayland_la_LDFLAGS = -export-dynamic

SNARF_FILES = $(libguile_wayland_la_SOURCES)
DOT_X_FILES = $(SNARF_FILES:.c=.x)

$(DOT_X_FILES): %.x : %.c
	$(GUILE_SNARF) $(libguile_wayland_la_CFLAGS) $(CFLAGS) $< > $@ \
		|| { rm $@; false; }

SCM_BUILT = wayland/client/protocol.scm
SOURCES = \
	  wayland/client.scm \
	  wayland/client/utils.scm \
	  wayland/config.scm \
	  wayland/cursor.scm \
	  wayland/egl.scm \
	  wayland/scanner.scm

BUILT_SOURCES = $(SCM_BUILT) $(DOT_X_FILES)

GOBJECTS = $(SCM_BUILT:.scm=.go) $(SOURCES:.scm=.go)

bin_SCRIPTS = wayland-scanner-guile

GUILE_WARNINGS = -Warity-mismatch \
		 -Wbad-case-datum \
		 -Wduplicate-case-datum \
		 -Wformat \
		 -Wmacro-use-before-definition \
		 -Wunbound-variable

$(GOBJECTS): %.go: %.scm $(guileextension_LTLIBRARIES)
	./env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

wayland/client/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland client" client "$<" "$@"

nobase_mod_DATA = $(SCM_BUILT) $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS)

pkgconfig_DATA = guile-wayland.pc

CLEANFILES = $(BUILT_SOURCES) $(GOBJECTS) $(DOT_X_FILES)

EXTRA_DIST = $(SCM_BUILT) $(SOURCES) $(GOBJECTS)
