# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: CC0-1.0

project(
  'guile-wayland',
  'c',
  version: '1.0',
  license: 'LGPL-3.0-or-later',
  meson_version: '>=0.54',
  default_options: [
    'c_std=gnu11',
    'warning_level=2',
    'werror=true',
  ],
)

cc = meson.get_compiler('c')

guile_wayland_args = cc.get_supported_arguments([
  '-Wendif-labels',
  '-Wimplicit-fallthrough=2',
  '-Winit-self',
  '-Wlogical-op',
  '-Wmissing-include-dirs',
  '-Wno-missing-braces',
  '-Wno-missing-field-initializers',
  '-Wno-unused-parameter',
  '-Wold-style-definition',
  '-Woverflow',
  '-Wpointer-arith',
  '-Wstrict-prototypes',
  '-Wstrict-aliasing=2',
  '-Wundef',
])

pkgconfig      = find_program('pkg-config', native: true)
guile          = dependency('guile-3.0')
guile_exe_path = guile.get_variable(pkgconfig: 'guile')
guile_exe      = find_program(guile_exe_path, native: true)

guile_ver = guile.name().split('-').get(1)

subdir('guile-wayland')
subdir('module')
subdir('doc')

configure_file(
  input: 'wayland-scanner-guile.in',
  output: 'wayland-scanner-guile',
  configuration: scm_conf,
  install: true,
  install_mode: ['rwxr-xr-x'],
  install_dir: get_option('bindir')
)

