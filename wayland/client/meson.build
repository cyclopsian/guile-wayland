# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

scm_lib_moddir = guile_moddir / 'wayland' / 'client'
scm_lib_godir = guile_godir / 'wayland' / 'client'

scm_compiles = []

scm_lib_config = configure_file(
  input: 'config.scm.in',
  output: 'config.scm',
  configuration: configuration_data({
    'extensionlib': guile_wayland_client.full_path(),
  }),
  install: true,
  install_dir: scm_lib_moddir
)
scm_compiles += [['config', scm_lib_config]]

scm_client_protocol = custom_target(
  'wayland_client_protocol_scm',
  input: wayland_client.get_variable(pkgconfig: 'pkgdatadir') / 'wayland.xml',
  output: 'protocol.scm',
  command: [guile_exe, '-L', scmdir, '--no-auto-compile', wayland_scanner_guile,
    'client', '@INPUT@', '@OUTPUT@'],
  install: true,
  install_dir: scm_lib_moddir
)
scm_compiles += [['protocol', scm_client_protocol]]

foreach src : scm_compiles
  custom_target(
    'wayland_client_' + src[0] + '_go',
    input: src[1],
    output: '@BASENAME@.go',
    depends: [guile_wayland_client],
    depend_files: [scm_lib_config],
    command: [guild_exe, 'compile'] + guild_warnings \
      + ['-L', scmdir, '-L', scmbuilddir, '@INPUT@', '-o', '@OUTPUT@'],
    install: true,
    install_dir: scm_lib_godir
  )
endforeach
