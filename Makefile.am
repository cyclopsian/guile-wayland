# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS)

guileextension_LTLIBRARIES = \
	libguile-wayland-client.la libguile-wayland-server.la
guileextensiondir = $(GUILE_EXTENSION)

moddir = $(GUILE_SITE)
objdir = $(GUILE_SITE_CCACHE)

UTIL_SRCS = guile-wayland/guile-wayland-util.c
CLIENT_SRCS = \
	guile-wayland/guile-wayland-cursor.c \
	guile-wayland/guile-wayland-egl.c \
	guile-wayland/guile-wayland-client.c
SERVER_SRCS = \
	guile-wayland/guile-wayland-server.c

libguile_wayland_client_la_SOURCES = $(CLIENT_SRCS) $(UTIL_SRCS)
libguile_wayland_client_la_includedir = $(includedir)/guile-wayland-client
libguile_wayland_client_la_include_HEADERS = \
	guile-wayland/guile-wayland-client.h \
	guile-wayland/guile-wayland-cursor.h \
	guile-wayland/guile-wayland-egl.h
libguile_wayland_client_la_CFLAGS = \
	$(WAYLAND_CLIENT_CFLAGS) $(WAYLAND_CURSOR_CFLAGS) $(WAYLAND_EGL_CFLAGS) \
	$(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_client_la_LIBADD = \
	$(WAYLAND_CLIENT_LIBS) $(WAYLAND_CURSOR_LIBS) $(WAYLAND_EGL_LIBS) \
	$(GUILE_LIBS)
libguile_wayland_client_la_LDFLAGS = -export-dynamic

libguile_wayland_server_la_SOURCES = $(SERVER_SRCS) $(UTIL_SRCS)
libguile_wayland_server_la_includedir = $(includedir)/guile-wayland-server
libguile_wayland_server_la_include_HEADERS = \
	guile-wayland/guile-wayland-server.h
libguile_wayland_server_la_CFLAGS = \
	$(WAYLAND_SERVER_CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_server_la_LIBADD = \
	$(WAYLAND_SERVER_LIBS) $(GUILE_LIBS)
libguile_wayland_server_la_LDFLAGS = -export-dynamic

DOT_X = $(UTIL_SRCS:.c=.x)
CLIENT_DOT_X = $(CLIENT_SRCS:.c=.x)
SERVER_DOT_X = $(SERVER_SRCS:.c=.x)

$(DOT_X): %.x : %.c
	$(GUILE_SNARF) $(AM_CFLAGS) $(GUILE_CFLAGS) $(CFLAGS) $< > $@ \
		|| { rm $@; false; }

$(CLIENT_DOT_X): %.x : %.c
	$(GUILE_SNARF) $(libguile_wayland_client_la_CFLAGS) $(CFLAGS) $< > $@ \
		|| { rm $@; false; }

$(SERVER_DOT_X): %.x : %.c
	$(GUILE_SNARF) $(libguile_wayland_server_la_CFLAGS) $(CFLAGS) $< > $@ \
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

BUILT_SOURCES = $(PROTOS) $(CLIENT_DOT_X) $(SERVER_DOT_X) $(DOT_X)

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

CLEANFILES = $(GOBJECTS) $(SCANNER_GOBJECTS) \
	$(BUILT_SOURCES) $(DOT_X) $(CLIENT_DOT_X) $(SERVER_DOT_X)

EXTRA_DIST = $(PROTOS) $(SOURCES) $(SCANNER) \
	$(GOBJECTS) $(SCANNER_GOBJECTS)
