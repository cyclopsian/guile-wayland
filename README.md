# guile-wayland

[![License: GPL 3.0 or later][license-img]][license-spdx]

A set of [GNU Guile][guile] bindings for the [Wayland][wayland] protocol.

# Building

Requirements:

- autotools
- guile >=3.0
- libwayland

Build steps:

```sh
autoreconf -fiv
./configure
make
make install
```

## Usage

See the programs in the `tests` and `examples` folder for sample code. The
[guile-cairo][guile-cairo] and [guile-epoxy][guile-epoxy] libraries are
required to run the examples.

### Custom Protocols

The build process will generate Scheme code for all the protocols in
`wayland-protocols`. To generate a module for a private protocol, use the
provided `wayland-scanner-guile` utility.

```sh
# client bindings, exported in module (app my-custom-protocol)
wayland-scanner-guile client -m 'app' \
  ./my-custom-protocol.xml \
  app/my-custom-protocol.scm

# server bindings, exported in module (app server my-custom-protocol)
wayland-scanner-guile server -m 'app server' \
  ./my-custom-protocol.xml \
  app/server/my-custom-protocol.scm
```

Alternatively, you can generate protocol bindings at runtime by invoking the
scanner:

```scheme
(use-modules (wayland scanner))

; client by default
(eval-when (expand load eval)
  (wl-scanner-load "./my-custom-protocol.xml"))

; for server bindings
(eval-when (expand load eval)
  (wl-scanner-load #:type 'server "./my-custom-protocol.xml"))
```

[guile]: https://www.gnu.org/software/guile/
[wayland]: https://wayland.freedesktop.org/
[guile-cairo]: https://www.nongnu.org/guile-cairo/
[guile-epoxy]: https://github.com/cyclopsian/guile-epoxy

[license-img]:  https://img.shields.io/badge/License-GPL%203.0%20or%20later-blue.svg?logo=gnu
[license-spdx]: https://spdx.org/licenses/GPL-3.0-or-later.html
