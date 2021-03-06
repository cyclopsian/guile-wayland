;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland cursor)
  #:use-module (oop goops)
  #:use-module (wayland client)
      #:export (get-cursor get-buffer width height hotspot-x hotspot-y
                images name frame-and-duration

                <wl-cursor-theme> <wl-cursor> <wl-cursor-image>)
   #:re-export (initialize delay destroy)
  #:duplicates (merge-generics))

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_cursor"))

(define-method (initialize (theme <wl-cursor-theme>) args)
  (apply wl-cursor-theme-load theme args))

(define-method (destroy (theme <wl-cursor-theme>))
  (wl-cursor-theme-destroy theme))

(define-method (get-cursor (theme <wl-cursor-theme>) name)
  (wl-cursor-theme-get-cursor theme name))

(define-method (get-buffer (image <wl-cursor-image>))
  (make <wl-buffer> (wl-cursor-image-get-buffer image)))

(define-method (width (image <wl-cursor-image>))
  (wl-cursor-image-width image))

(define-method (height (image <wl-cursor-image>))
  (wl-cursor-image-height image))

(define-method (hotspot-x (image <wl-cursor-image>))
  (wl-cursor-image-hotspot-x image))

(define-method (hotspot-y (image <wl-cursor-image>))
  (wl-cursor-image-hotspot-y image))

(define-generic delay)

(define-method (delay (image <wl-cursor-image>))
  (wl-cursor-image-delay image))

(define-method (images (cursor <wl-cursor>))
  (wl-cursor-images cursor))

(define-method (name (cursor <wl-cursor>))
  (wl-cursor-name cursor))

(define-method (frame-and-duration (cursor <wl-cursor>) time)
  (wl-cursor-frame-and-duration cursor time))
