;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(eval-when (expand load eval)
  (load-extension (@ (wayland config) *wayland-lib-path*)
                  "scm_init_wayland"))

(define-module (wayland egl)
  #:use-module (oop goops)
  #:use-module (wayland egl core)
  #:use-module (wayland client protocol)
      #:export (resize get-attached-size)
   #:re-export (<wl-egl-window> destroy))

(define-method
  (initialize (egl-window <wl-egl-window>) (surface <wl-surface>) width height)
  (let ((other (wl-egl-window-create surface width height)))
    (slot-set! egl-window 'egl-window (slot-ref other 'egl-window))
    (slot-set! other      'egl-window 0)))

(define-method (destroy (egl-window <wl-egl-window>))
  (wl-egl-window-destroy egl-window))

(define-method (resize (egl-window <wl-egl-window>) width height dx dy)
  (wl-egl-window-resize egl-window width height dx dy))

(define-method (get-attached-size (egl-window <wl-egl-window>))
  (wl-egl-window-get-attached-size egl-window))
