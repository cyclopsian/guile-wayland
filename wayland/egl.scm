;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_client"))

(define-module (wayland egl)
  #:use-module (oop goops)
  #:use-module (wayland egl core)
  #:use-module (wayland client protocol)
      #:export (resize get-attached-size)
   #:re-export (<wl-egl-window> initialize delay destroy)
  #:duplicates (merge-generics))

(define-method (initialize (egl-window <wl-egl-window>) args)
  (let ((other (apply wl-egl-window-create args)))
    (slot-set! egl-window 'egl-window (slot-ref other 'egl-window))
    (slot-set! other      'egl-window 0)))

(define-method (destroy (egl-window <wl-egl-window>))
  (wl-egl-window-destroy egl-window))

(define-method (resize (egl-window <wl-egl-window>) width height dx dy)
  (wl-egl-window-resize egl-window width height dx dy))

(define-method (get-attached-size (egl-window <wl-egl-window>))
  (wl-egl-window-get-attached-size egl-window))
