;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (examples cairo-client)
  #:use-module (cairo)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (wayland client)
  #:use-module (wayland client util)
  #:use-module (wayland scanner)
  #:duplicates (merge-generics))

(eval-when (expand load eval)
  (wl-scanner-load
    (string-append *wl-protocol-dir* "/stable/xdg-shell/xdg-shell.xml")))

(define-class <client-state> ()
  (display #:accessor display)
  (store   #:accessor store)
  (focused #:accessor focused)
  (seats   #:accessor seats #:init-value '()))

(define-method (initialize (state <client-state>) args)
  (set! (display state) (make <wl-display>))
  (set! (store state)
    (listen-interfaces
      (display state)
      (list <wl-seat>
            #:after  (λ (seat) (listen-seat state seat))
            #:remove (λ (seat) (destroy-seat state seat)))
      <wl-shm>
      <wl-compositor>
      (list <xdg-wm-base>
            #:after (λ (wm)
                      (add-listener
                        wm #:ping (λ (serial) (pong wm serial))))))))

(define-method (destroy (state <client-state>))
  (destroy (store state))
  (destroy (display state)))

(define-class <client-seat> ()
  (wl-seat  #:accessor wl-seat #:init-keyword #:wl-seat)
  (pointer  #:accessor pointer #:init-value #f))

(define-method (listen-seat (state <client-state>) (seat <wl-seat>))
  (let ((cseat (make <client-seat> #:wl-seat seat)))
    (add-listener seat
      #:capabilities
      (λ (capabilities)
         (if (positive? (logand capabilities WL_SEAT_CAPABILITY_POINTER))
           (unless (pointer cseat)
             (let ((ptr (get-pointer seat)))
               (set! (pointer cseat) ptr)
               (add-listener ptr
                 #:button
                 (λ (serial time button flags)
                    (move (focused state) seat serial)))))
           (begin (and=> (pointer cseat) destroy)
                  (set! (pointer cseat) #f)))))))

(define-method (destroy-seat (state <client-state>) (seat <wl-seat>))
  (let ((cseat (find (λ (s) (eq? (wl-seat s) seat)) (seats state))))
    (and=> (pointer cseat) destroy)
    (delq1! cseat (seats state))))

(define π 3.14159265358979323846264338327950288)

(let* ((state (make <client-state>))
       (disp (display state))
       (objs (store state))
       (w 128)
       (h 128)
       (buffer       (create-argb-buffer (get objs <wl-shm>) w h))
       (cr-surface   (cairo-image-surface-create-for-data
                       (data buffer) 'argb32 w h (stride buffer)))
       (cr           (cairo-create cr-surface))
       (surface      (create-surface (get objs <wl-compositor>)))
       (xdg-surface  (get-xdg-surface (get objs <xdg-wm-base>) surface))
       (xdg-toplevel (get-toplevel xdg-surface))
       (running #t))
  (set! (focused state) xdg-toplevel)

  (cairo-set-source-rgba cr 0 0 1 0.5)
  (cairo-paint cr)
  (cairo-set-source-rgba cr 1 0 0 1)
  (cairo-arc cr (/ w 2) (/ h 2) (/ (min w h) 2) 0 (* π 2))
  (cairo-fill cr)
  (cairo-destroy cr)
  (cairo-surface-destroy cr-surface)
  (unmap buffer)

  (add-listener xdg-surface
                #:configure (λ (serial)
                              (ack-configure xdg-surface serial)
                              (attach surface (wl-buffer buffer) 0 0)
                              (commit surface)))
  (add-listener xdg-toplevel
                #:close (λ () (set! running #f)))
  (commit surface)
  (roundtrip disp)

  (while running (dispatch disp))

  (destroy xdg-toplevel)
  (destroy xdg-surface)
  (destroy surface)
  (destroy buffer)
  (destroy state))
