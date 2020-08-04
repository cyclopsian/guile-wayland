;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland server core))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_server_core")
  (module-export-all! (current-module)))
