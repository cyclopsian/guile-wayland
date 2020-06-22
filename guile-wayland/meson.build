# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: CC0-1.0

fs = import('fs')

python3        = find_program('python3', native: true)
guile_snarf    = find_program('guile-snarf-3.0', native: true)
wayland_client = dependency('wayland-client',    version: '>=1.18')
wayland_cursor = dependency('wayland-cursor',    version: '>=1.18')
wayland_egl    = dependency('wayland-egl',       version: '>=1.18')

#wayland_server = dependency('wayland-server',    version: '>=1.18')
root = '/' / get_option('prefix')
guile_moddir = root / get_option('datadir') / 'guile/site' / guile_ver

guile_wayland_client_snarf_srcs = [
  'guile-wayland-client.c',
  'guile-wayland-cursor.c',
  'guile-wayland-egl.c',
]

guile_wayland_headers = [
  'guile-wayland-client.h',
  'guile-wayland-cursor.h',
  'guile-wayland-egl.h',
]

guile_wayland_client_gen_srcs = []
guile_wayland_client_gen_srcs += configure_file(
  input: 'guile-wayland-client-init.c.in',
  output: 'guile-wayland-client-init.c',
  configuration: configuration_data({'moddir': guile_moddir}),
)

snarf_helper = configure_file(
  input: 'snarf-helper.py.in',
  output: 'snarf-helper.py',
  configuration: configuration_data({
    'builddir'   : meson.build_root(),
    'srcdir'     : meson.source_root(),
    'guile'      : guile_exe_path,
    'guile-snarf': guile_snarf.path(),
  })
)

guile_wayland_client_x_srcs = []

foreach path : guile_wayland_client_snarf_srcs
  guile_wayland_client_x_srcs += custom_target(
    fs.stem(path).underscorify() + '_x',
    input: path,
    output: '@BASENAME@.x',
    command: [python3, snarf_helper, 'code', '@INPUT@', '@OUTPUT@']
  )
endforeach

extdir = get_option('libdir') / 'guile' / guile_ver / 'extensions'

guile_wayland_client = library(
  'guile-wayland-client',
  guile_wayland_client_snarf_srcs,
  guile_wayland_client_gen_srcs,
  guile_wayland_client_x_srcs,
  c_args: guile_wayland_args,
  dependencies: [guile, wayland_client, wayland_cursor, wayland_egl],
  install: true,
  install_dir: extdir
)

install_headers(
  guile_wayland_headers,
  subdir: meson.project_name()
)

guile_wayland_texi_srcs = []
foreach path : guile_wayland_client_snarf_srcs
  guile_wayland_texi_srcs += custom_target(
    fs.stem(path).underscorify() + '_texi',
    input: path,
    output: '@BASENAME@.texi',
    command: [python3, snarf_helper, 'docs', '@INPUT@', '@OUTPUT@'],
    depend_files: files('../doc/snarf-docs.scm'),
  )
endforeach

guile_wayland_txt_srcs = []
foreach path : guile_wayland_client_snarf_srcs
  guile_wayland_txt_srcs += custom_target(
    fs.stem(path).underscorify() + '_procedures_txt',
    input: path,
    output: '@BASENAME@-procedures.txt',
    command: [python3, snarf_helper, 'docstrings', '@INPUT@', '@OUTPUT@'],
    depend_files: files('../doc/snarf-docs.scm'),
  )
endforeach

custom_target(
  'wayland_procedures_txt',
  input: [guile_wayland_txt_srcs],
  output: 'wayland-procedures.txt',
  command: [find_program('cat', native: true), '@INPUT@'],
  capture: true,
  install: true,
  install_dir: guile_moddir / 'wayland'
)
