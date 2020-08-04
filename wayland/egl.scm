;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland egl)
  #:use-module (oop goops)
  #:use-module (wayland client)
      #:export (resize get-attached-size <wl-egl-window>)
   #:re-export (initialize delay destroy)
  #:duplicates (merge-generics))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_egl"))

(define-method (initialize (egl-window <wl-egl-window>) args)
  (apply wl-egl-window-create egl-window args))

(define-method (destroy (egl-window <wl-egl-window>))
  (wl-egl-window-destroy egl-window))

(define-method (resize (egl-window <wl-egl-window>) width height dx dy)
  (wl-egl-window-resize egl-window width height dx dy))

(define-method (get-attached-size (egl-window <wl-egl-window>))
  (wl-egl-window-get-attached-size egl-window))
