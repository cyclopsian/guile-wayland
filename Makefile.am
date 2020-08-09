# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

ACLOCAL_AMFLAGS = -I m4

AM_CFLAGS = $(WARN_CFLAGS) $(DEBUG_CFLAGS) -Iguile-wayland

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
	guile-wayland/guile-wayland-server.h \
	guile-wayland/guile-wayland-util.h \
	guile-wayland/guile-wayland-util-private.h
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

PROTO_XMLS = $(WAYLAND_PROTOCOLDIR)/stable/presentation-time/presentation-time.xml \
	     $(WAYLAND_PROTOCOLDIR)/stable/viewporter/viewporter.xml \
	     $(WAYLAND_PROTOCOLDIR)/stable/xdg-shell/xdg-shell.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/fullscreen-shell/fullscreen-shell-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/idle-inhibit/idle-inhibit-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/input-method/input-method-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/input-timestamps/input-timestamps-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/keyboard-shortcuts-inhibit/keyboard-shortcuts-inhibit-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/linux-dmabuf/linux-dmabuf-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/linux-explicit-synchronization/linux-explicit-synchronization-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/pointer-constraints/pointer-constraints-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/pointer-gestures/pointer-gestures-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/primary-selection/primary-selection-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/relative-pointer/relative-pointer-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/tablet/tablet-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/tablet/tablet-unstable-v2.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/text-input/text-input-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/text-input/text-input-unstable-v3.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xdg-foreign/xdg-foreign-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xdg-foreign/xdg-foreign-unstable-v2.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xdg-output/xdg-output-unstable-v1.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xdg-shell/xdg-shell-unstable-v5.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xdg-shell/xdg-shell-unstable-v6.xml \
	     $(WAYLAND_PROTOCOLDIR)/unstable/xwayland-keyboard-grab/xwayland-keyboard-grab-unstable-v1.xml
# Special case because of dependency on xdg-shell
XDG_DECORATION_XML = $(WAYLAND_PROTOCOLDIR)/unstable/xdg-decoration/xdg-decoration-unstable-v1.xml
vpath %.xml $(dir $(PROTO_XMLS))

SOURCES = \
	  wayland/client.scm \
	  wayland/client/core.scm \
	  wayland/client/util.scm \
	  wayland/config.scm \
	  wayland/cursor.scm \
	  wayland/egl.scm \
	  wayland/util.scm \
	  wayland/server.scm \
	  wayland/server/core.scm \
	  wayland/server/util.scm
SCANNER = wayland/scanner.scm
CLIENT_EXT_PROTOS = $(addprefix wayland/client/,$(notdir $(PROTO_XMLS:.xml=.scm)))
SERVER_EXT_PROTOS = $(addprefix wayland/server/,$(notdir $(PROTO_XMLS:.xml=.scm)))
PROTOS = wayland/client/protocol.scm wayland/server/protocol.scm \
	 wayland/client/xdg-decoration-unstable-v1.scm \
	 wayland/server/xdg-decoration-unstable-v1.scm \
	 $(CLIENT_EXT_PROTOS) $(SERVER_EXT_PROTOS)

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

wayland/client/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland client" client "$<" "$@"

wayland/server/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland server" server "$<" "$@"

$(CLIENT_EXT_PROTOS): wayland/client/%.scm: %.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland client" client "$<" "$@"

$(SERVER_EXT_PROTOS): wayland/server/%.scm: %.xml \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -m "wayland server" server "$<" "$@"

wayland/client/xdg-decoration-unstable-v1.scm: $(XDG_DECORATION_XML) \
		wayland/client/xdg-shell.scm \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -d "(wayland client xdg-shell)" \
	  -m "wayland client" client "$<" "$@"

wayland/server/xdg-decoration-unstable-v1.scm: $(XDG_DECORATION_XML) \
		wayland/server/xdg-shell.scm \
		wayland-scanner-guile wayland/scanner.scm wayland/scanner.go
	./env ./wayland-scanner-guile -d "(wayland server xdg-shell)" \
	  -m "wayland server" server "$<" "$@"

bin_SCRIPTS = wayland-scanner-guile

pkgconfig_DATA = guile-wayland.pc

TESTS = tests/server.scm tests/integration.scm
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = ./env $(GUILE) -s
check_SCRIPTS = $(TESTS:.scm=.go)

EXAMPLES = examples/cairo-client.scm examples/egl-client.scm
EXAMPLE_GOBJECTS = $(EXAMPLES:.scm=.go)

$(check_SCRIPTS) $(EXAMPLE_GOBJECTS) $(SCANNER_GOBJECTS): %.go: %.scm
	./env $(GUILD) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

nobase_mod_DATA = $(PROTOS) $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS) $(SCANNER_GOBJECTS) \
			 $(check_SCRIPTS) $(EXAMPLE_GOBJECTS)

CLEANFILES = $(GOBJECTS) $(SCANNER_GOBJECTS) $(BUILT_SOURCES) $(DOT_X) \
	     $(check_SCRIPTS) $(EXAMPLE_GOBJECTS)

EXTRA_DIST = $(SOURCES) $(SCANNER) $(TESTS) LICENSES .reuse
