;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests integration)
  #:use-module (wayland client)
  #:use-module (wayland client util)
  #:use-module (wayland server)
  #:use-module (wayland util)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64)
  #:duplicates (merge-generics replace))

(module-set! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)
(test-runner-current (test-runner-create))

(define (print-buffer buffer)
  (let* ((shm-buffer (get-shm-buffer buffer))
         (pixels (get-data shm-buffer))
         (width (get-width shm-buffer))
         (height (get-height shm-buffer))
         (stride (get-stride shm-buffer)))
    (begin-access shm-buffer)
    (test-assert (equal? (bytevector-u8-ref pixels 0) 255))
    (test-assert (equal? (bytevector-u8-ref pixels 1) 0))
    (test-assert (equal? (bytevector-u8-ref pixels 2) 0))
    (test-assert (equal? (bytevector-u8-ref pixels 3) 128))
    (for-each
      (λ (y)
         (for-each
           (λ (x)
              (let* ((off (+ (* x 4) (* y stride)))
                     (r (u8vector-ref pixels (+ off 1)))
                     (g (u8vector-ref pixels (+ off 2)))
                     (b (u8vector-ref pixels (+ off 3))))
                (format #t "\x1b[48;2;~a;~a;~am " r g b)))
           (iota width))
         (format #t "\x1b[0m\n"))
      (iota height))
    (end-access shm-buffer)))

(define (make-server)
  (let* ((server (make <wl-display-server>))
         (socket-name (add-socket server))
         (make-compositor
           (λ (client version id)
              (let ((compositor
                      (make <wl-compositor-resource> client version id))
                    (surfaces '()))
                (set-implementation compositor
                  #:create-surface
                  (λ (id)
                     (let ((surface (make <wl-surface-resource> client 4 id))
                           (pending #f)
                           (current #f))
                       (append! surfaces (list surface))
                       (set-implementation surface
                         #:attach (λ (buffer x y) (set! pending buffer))
                         #:commit (λ ()
                                     (set! current pending)
                                     (set! pending #f)
                                     (print-buffer current))
                         #:destructor (λ ()
                                         (when pending (destroy pending))
                                         (when current (destroy current))))))
                  #:destructor (λ () (for-each destroy surfaces))))))
         (compositor-global (make <wl-global> server <wl-compositor-resource> 4
                                  make-compositor))
         (created-listener #f)
         (destroy-listener #f))
    (set! created-listener
      (add-client-created-listener server
        (λ (client)
           (set! destroy-listener
             (add-destroy-listener client
               (λ ()
                  (destroy created-listener)
                  (destroy destroy-listener)
                  (terminate server)))))))
    (init-shm server)
    (list server socket-name)))

(define (make-client socket-name destroyed)
  (let* ((client (make <wl-display> socket-name))
         (registry (get-registry client))
         (compositor #f)
         (shm #f)
         (done #f))
    (add-listener registry
      #:global
      (λ (id interface version)
         (cond ((equal? interface (name <wl-compositor>))
                (set! compositor (bind registry id <wl-compositor> 4)))
               ((equal? interface (name <wl-shm>))
                (set! shm (bind registry id <wl-shm> 1))))
         (when (and shm compositor (not done))
           (set! done #t)
           (let* ((surface (create-surface compositor))
                  (buffer (create-argb-buffer shm 40 12))
                  (pixels (data buffer))
                  (width (width buffer))
                  (height (height buffer))
                  (stride (stride buffer)))
             (for-each
               (λ (y)
                  (for-each
                    (λ (x)
                       (let* ((o (+ (* x 4) (* y stride))))
                         (bytevector-u8-set! pixels (+ o 0) 255)
                         (bytevector-u8-set! pixels (+ o 1)
                                             (floor (* 255 (/ x width))))
                         (bytevector-u8-set! pixels (+ o 2)
                                             (floor (* 255 (/ y height))))
                         (bytevector-u8-set! pixels (+ o 3) 128)))
                    (iota width)))
               (iota height))
             (attach surface (wl-buffer buffer) 0 0)
             (commit surface)
             (unmap buffer)
             (let ((callback (sync client)))
               (add-listener callback
                 #:done (λ (serial)
                           (destroy callback)
                           (destroy buffer)
                           (destroy shm)
                           (destroy compositor)
                           (destroy registry)
                           (disconnect client)
                           (destroyed))))))))
    client))

(test-group "client-server-integration"
  (define (run-server-and-client)
    (match-let* (((server socket-name) (make-server))
                 (loop (get-event-loop server))
                 (client #f)
                 (client-source #f)
                 (client-dispatch
                   (λ ()
                      (unless (prepare-read client)
                        (dispatch-pending client))
                      (flush client)
                      (read-events client)
                      (dispatch-pending client)))
                 (client-destroyed
                   (λ () (remove client-source))))
      (set! client (make-client socket-name client-destroyed))
      (set! client-source (add-fd loop (get-fd client)
                                  (logior WL_EVENT_READABLE
                                          WL_EVENT_WRITABLE)
                                  client-dispatch))
      (run server)
      (destroy server)))

  (test-assert (run-server-and-client)))

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
