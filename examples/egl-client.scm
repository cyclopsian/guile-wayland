;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland examples egl-client)
  #:use-module (epoxy egl)
  #:use-module (epoxy gles2)
  #:use-module (ice-9 match)
  #:use-module (wayland client)
  #:use-module (wayland egl)
  #:use-module (wayland scanner)
  #:duplicates (merge-generics))

(wl-scanner-load
  (string-append *wl-protocol-dir* "/stable/xdg-shell/xdg-shell.xml"))

(let* ((disp (make <wl-display>))
       (registry (get-registry disp))
       (seat #f)
       (compositor #f)
       (xdg-wm-base #f)
       (focused #f))
  (add-listener registry
    #:global
    (λ (name interface version)
      (cond
        ((equal? interface (name <wl-seat>))
         (set! seat (bind registry name <wl-seat> 1))
         (add-listener seat
           #:capabilities
           (λ (capabilities)
             (unless (zero? (logand capabilities WL_SEAT_CAPABILITY_POINTER))
               (add-listener (get-pointer seat)
                 #:button
                 (λ (serial time button state)
                   (move focused seat serial)))))))
        ((equal? interface (name <wl-compositor>))
         (set! compositor (bind registry name <wl-compositor> 1)))
        ((equal? interface (name <xdg-wm-base>))
         (set! xdg-wm-base (bind registry name <xdg-wm-base> 1))))))
  (dispatch disp)
  (roundtrip disp)
  (unless compositor
    (error "wl_compositor interface not found"))
  (unless xdg-wm-base
    (error "xdg_wm_base interface not found"))
  (match-let*
    ((egl-display (egl-get-platform-display EGL_PLATFORM_WAYLAND_EXT
                                            (slot-ref disp 'proxy))))
    ((config-attribs '(EGL_SURFACE_TYPE      EGL_WINDOW_BIT
                       EGL_RED_SIZE          8
                       EGL_GREEN_SIZE        8
                       EGL_BLUE_SIZE         8
                       EGL_ALPHA_SIZE        0
                       EGL_RENDERABLE_TYPE   EGL_OPENGL_ES2_BIT
                       EGL_NATIVE_RENDERABLE EGL_TRUE))
     (ctx-attribs    '(EGL_CONTEXT_CLIENT_VERSION 2))
     ((config) (begin
                 (egl-initialize egl-display)
                 (egl-choose-config egl-display attribs 1)))
     (context (egl-create-context egl-display config ctx-attribs))
     (surface (create-surface compositor))
     (xdg-surface (get-xdg-surface xdg-wm-base surface))
     (xdg-toplevel (get-toplevel xdg-surface))
     (egl-window (make-wl-egl-window surface 128 128))
     (egl-surface (egl-create-window-surface
                    egl-display config (slot-ref egl-window 'egl-window)))
     (running #t))
    (set! focused xdg-toplevel)
    (add-listener xdg-surface
      #:configure (λ (serial)
                    (ack-configure xdg-surface serial)
                    (commit surface))
    (add-listener xdg-toplevel
      #:close (λ () (set! running #f)))
    (commit surface)
    (roundtrip disp)
    (egl-make-current egl-display egl-surface egl-surface context)
    (egl-swap-interval egl-display 0)
    (define (render)
      (gl-clear-color 0 0 1 1)
      (gl-clear GL_COLOR_BUFFER_BIT)
      (let ((callback (frame surface)))
        (add-listener callback
          #:done (λ (time) (destroy callback) (render))))
      (egl-swap-buffers egl-display egl-surface))
    (render)
    (while running (dispatch disp))
    (destroy xdg-toplevel)
    (destroy xdg-surface)
    (destroy surface))))
