# SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
# SPDX-License-Identifier: GPL-3.0-or-later

subdir('client')

scm_lib_moddir = guile_moddir / 'wayland'
scm_lib_godir = guile_godir / 'wayland'

scm_compiles = []
scm_lib_srcs = [
  #'client.scm',
  'scanner.scm',
  #'server.scm',
]
install_data(scm_lib_srcs, install_dir: scm_lib_moddir)
foreach src : scm_lib_srcs
  scm_compiles += [[fs.stem(src), src]]
endforeach

foreach src : scm_compiles
  custom_target(
    'wayland_' + src[0] + '_go',
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

