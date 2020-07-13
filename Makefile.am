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
SNARF_SOURCES = \
	guile-wayland/guile-wayland-cursor.c \
	guile-wayland/guile-wayland-egl.c \
	guile-wayland/guile-wayland-client.c
libguile_wayland_la_SOURCES = $(SNARF_SOURCES) \
	guile-wayland/guile-wayland-client-init.c \
libguile_wayland_la_CFLAGS = $(WAYLAND_CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS)
libguile_wayland_la_LIBADD = $(WAYLAND_LIBS) $(GUILE_LIBS)
libguile_wayland_la_LDFLAGS = -export-dynamic

DOT_X_FILES = $(SNARF_SOURCES:.c=.x)

$(DOT_X_FILES): %.x : %.c
	$(GUILE_SNARF) $(libguile_wayland_la_CFLAGS) $< > $@ \
		|| { rm $@; false; }

SCM_BUILT = wayland/config.install.scm wayland/client/protocol.scm
SCM_NOINST_BUILT = wayland/config.scm
SOURCES = wayland/scanner.scm

BUILT_SOURCES = $(SCM_BUILT) $(SCM_NOINST_BUILT) $(DOT_X_FILES)

GOBJECTS = $(SCM_BUILT:.scm=.go) $(SOURCES:.scm=.go)

GUILE_WARNINGS = -Warity-mismatch \
		 -Wbad-case-datum \
		 -Wduplicate-case-datum \
		 -Wformat \
		 -Wmacro-use-before-definition \
		 -Wunbound-variable

$(GOBJECTS): %.go: %.scm $(guileextension_LTLIBRARIES) wayland/config.scm
	echo $(guileextension_LTLIBRARIES)
	$(GUILD) compile -L $(abs_top_srcdir) -L $(abs_top_builddir) \
		$(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"

wayland/config.scm: wayland/config.scm.in Makefile
	sed -e "s|@extensionlib\@|$(abs_top_builddir)/libguile-wayland|" \
	    "$<" > "$@"

wayland/config.install.scm: wayland/config.scm.in Makefile
	sed -e "s|@extensionlib\@|$(guileextensiondir)/libguile-wayland|" \
	    "$<" > "$@"

wayland/client/protocol.scm: $(WAYLAND_DATADIR)/wayland.xml scanner.scm
	$(GUILE) -L $(abs_top_srcdir) --no-auto-compile wayland/scanner.scm \
		client "$<" "$@"

install-data-hook:
	cd $(DESTDIR)$(moddir)/wayland && mv config.install.scm config.scm
	cd $(DESTDIR)$(objdir)/wayland && mv config.install.go config.go

nobase_mod_DATA = $(SCM_BUILT) $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-wayland.pc

CLEANFILES = $(BUILT_SOURCES) $(GOBJECTS) $(DOT_X_FILES)

EXTRA_DIST = $(SCM_BUILT) $(SOURCES) $(GOBJECTS)
