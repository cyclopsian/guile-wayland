# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

scm_lib_moddir = guile_moddir / 'wayland'
scm_lib_godir = guile_godir / 'wayland'

scm_conf = configuration_data({
  'clientextensionlib': guile_wayland_client.full_path(),
  'guile': guile_exe_path,
  'moddir': guile_moddir,
  'godir': guile_godir,
})

scm_compiles = []
scm_lib_srcs = [
  'scanner.scm',
]
install_data(scm_lib_srcs, install_dir: scm_lib_moddir)
foreach src : scm_lib_srcs
  scm_compiles += [[fs.stem(src), src]]
endforeach

scm_lib_config = configure_file(
  input: 'config.scm.in',
  output: 'config.scm',
  configuration: scm_conf,
  install: true,
  install_dir: scm_lib_moddir
)
scm_compiles += [['client', scm_lib_config]]

foreach src : scm_compiles
  custom_target(
    'wayland_' + src[0] + '_go',
    input: src[1],
    output: '@BASENAME@.go',
    depends: [guile_wayland],
    depend_files: [scm_lib_config],
    command: [guild_exe, 'compile'] + guild_warnings \
      + ['-L', scmdir, '-L', scmbuilddir, '@INPUT@', '-o', '@OUTPUT@'],
    install: true,
    install_dir: scm_lib_godir
  )
endforeach
