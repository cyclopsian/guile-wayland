# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

guileextension_LTLIBRARIES = libguile-wayland.la
guileextensiondir = $(GUILE_EXTENSION)

moddir = $(GUILE_SITE)
objdir = $(GUILE_SITE_CCACHE)

libguile_wayland_la_SOURCES = \
	guile-wayland/guile-wayland-client.c \
	guile-wayland/guile-wayland-cursor.c \
	guile-wayland/guile-wayland-egl.c \
	guile-wayland/guile-wayland-server.c \
	guile-wayland/guile-wayland-util.c
libguile_wayland_la_includedir = $(includedir)/guile-wayland
libguile_wayland_la_include_HEADERS = \
	guile-wayland/guile-wayland-client.h \
	guile-wayland/guile-wayland-cursor.h \
	guile-wayland/guile-wayland-egl.h \
	guile-wayland/guile-wayland-util.h \
	guile-wayland/guile-wayland-server.h
libguile_wayland_la_CFLAGS = \
	$(WAYLAND_CLIENT_CFLAGS) $(WAYLAND_CURSOR_CFLAGS) $(WAYLAND_EGL_CFLAGS) \
	$(WAYLAND_SERVER_CFLAGS) \
	$(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD = \
	$(WAYLAND_CLIENT_LIBS) $(WAYLAND_CURSOR_LIBS) $(WAYLAND_EGL_LIBS) \
	$(WAYLAND_SERVER_LIBS) \
	$(GUILE_LIBS)
libguile_wayland_la_LDFLAGS = -export-dynamic

DOT_X = $(libguile_wayland_la_SOURCES:.c=.x)

$(DOT_X): %.x : %.c
	$(GUILE_SNARF) $(AM_CFLAGS) $(GUILE_CFLAGS) $(CFLAGS) $< > $@ \
		|| { rm $@; false; }

SOURCES = \
	  wayland/client.scm \
	  wayland/client/util.scm \
	  wayland/config.scm \
	  wayland/cursor.scm \
	  wayland/egl.scm \
	  wayland/server.scm \
	  wayland/server/util.scm
SCANNER = wayland/scanner.scm
PROTOS = wayland/client/protocol.scm wayland/server/protocol.scm

BUILT_SOURCES = $(PROTOS) $(DOT_X)

GOBJECTS =  $(SOURCES:.scm=.go) $(PROTOS:.scm=.go)
SCANNER_GOBJECTS = $(SCANNER:.scm=.go)

GUILE_WARNINGS = -Warity-mismatch \
		 -Wbad-case-datum \
		 -Wduplicate-case-datum \
		 -Wformat \
		 -Wmacro-use-before-definition \
		 -Wunbound-variable

$(GOBJECTS): %.go: %.scm $(guileextension_LTLIBRARIES)
	./env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

$(SCANNER_GOBJECTS): %.go: %.scm
	./env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

wayland/client/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland client" client "$<" "$@"

wayland/server/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland server" server "$<" "$@"

bin_SCRIPTS = wayland-scanner-guile

nobase_mod_DATA = $(PROTOS) $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS) $(SCANNER_GOBJECTS)

pkgconfig_DATA = guile-wayland-client.pc guile-wayland-server.pc

CLEANFILES = $(GOBJECTS) $(SCANNER_GOBJECTS) $(BUILT_SOURCES) $(DOT_X)

EXTRA_DIST = $(PROTOS) $(SOURCES) $(SCANNER) $(GOBJECTS) $(SCANNER_GOBJECTS)
