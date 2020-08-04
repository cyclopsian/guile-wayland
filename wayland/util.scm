;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland util)
  #:use-module (oop goops)
      #:export (monotonic-time mmap munmap create-shm-fdes name
                PROT_EXEC PROT_READ PROT_WRITE PROT_NONE
                MAP_SHARED MAP_PRIVATE))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_util"))

(define-method (name (interface <wl-interface>))
  (wl-interface-name interface))
