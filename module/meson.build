# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

fs = import('fs')

guild_exe = find_program(guile.get_variable(pkgconfig: 'guild'), native: true)
guild_warnings = [
  '-Warity-mismatch',
  '-Wbad-case-datum',
  '-Wduplicate-case-datum',
  '-Wformat',
  '-Wmacro-use-before-definition',
  '-Wunbound-variable',
]
scmdir = meson.current_source_dir()
scmbuilddir = meson.current_build_dir()

subdir('wayland')

