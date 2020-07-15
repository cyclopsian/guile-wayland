;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(eval-when (expand load eval)
  (load-extension "libguile-wayland-server" "scm_init_wayland_server"))

(define-module (wayland server)
  #:use-module (oop goops)
  #:use-module (wayland server core)
  #:use-module (wayland server protocol)
  #:duplicates (merge-generics))

