;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland client)
  #:use-module (oop goops)
  #:use-module (wayland client core)
  #:use-module (wayland client protocol)
  #:use-module (wayland util)
      #:export (interface

                create-wrapper wrapper-destroy
                get-version get-id get-class get-symbol set-queue

                disconnect get-fd dispatch dispatch-queue
                dispatch-queue-pending dispatch-pending get-error
                get-protocol-error flush roundtrip-queue roundtrip create-queue
                prepare-read-queue prepare-read cancel-read read-events)
   #:re-export (<wl-display> <wl-event-queue> <wl-proxy> <wl-proxy-class>
                initialize make-instance name destroy wl-set-log-port-client)
  #:duplicates (merge-generics replace))

(eval-when (expand load eval)
  ((@ (rnrs base) let*-values)
   (((guile) (resolve-interface '(guile)))
    ((protocol-syms) (module-map (compose car cons)
                                 (resolve-interface
                                   '(wayland client protocol))))
    ((replaces re-exports) ((@ (srfi srfi-1) partition)
                            (λ (sym) (module-bound? guile sym))
                            protocol-syms)))
   (module-re-export! (current-module) replaces #:replace? #t)
   (module-re-export! (current-module) re-exports)))

(define-syntax add-accessors
  (syntax-rules ()
    ((_ class)
     ((@@ (oop goops) compute-slot-accessors) class (class-slots class)))
    ((_ class name rest ...)
     (begin
       (define-accessor name)
       (slot-set! (class-slot-definition class 'name) 'accessor name)
       (add-accessors class rest ...)))))

(add-accessors <wl-proxy-class> interface)

(define-method (make-instance (class <wl-proxy-class>) (proxy <wl-proxy>))
  (wl-proxy-cast proxy class))

(define-method (name (proxy-class <wl-proxy-class>))
  (name (interface proxy-class)))

(define-method (destroy (queue <wl-event-queue>))
  (wl-event-queue-destroy queue))

(define-method (create-wrapper (proxy <wl-proxy>))
  (make (class-of proxy) (wl-proxy-create-wrapper proxy)))

(define-method (wrapper-destroy (proxy <wl-proxy>))
  (wl-proxy-wrapper-destroy proxy))

(define-method (get-version (proxy <wl-proxy>))
  (wl-proxy-get-version proxy))

(define-method (get-id (proxy <wl-proxy>))
  (wl-proxy-get-id proxy))

(define-method (get-class (proxy <wl-proxy>))
  (wl-proxy-get-class proxy))

(define-method (get-symbol (proxy <wl-proxy>))
  (wl-proxy-get-symbol proxy))

(define-method (set-queue (proxy <wl-proxy>) (queue <wl-event-queue>))
  (wl-proxy-set-queue proxy queue))

(define-method (display-connect (disp <wl-display>))
  (wl-display-connect disp))

(define-method (display-connect (disp <wl-display>) (socket-name <string>))
  (wl-display-connect disp socket-name))

(define-method (display-connect (disp <wl-display>) (fd <integer>))
  (wl-display-connect-to-fd disp fd))

(define-method (initialize (disp <wl-display>) args)
  (apply display-connect disp args))

(define-method (disconnect (disp <wl-display>))
  (wl-display-disconnect disp))

(define-method (get-fd (disp <wl-display>))
  (wl-display-get-fd disp))

(define-method (dispatch (disp <wl-display>))
  (wl-display-dispatch disp))

(define-method (dispatch-queue (disp <wl-display>) (queue <wl-event-queue>))
  (wl-display-dispatch-queue disp queue))

(define-method (dispatch-queue-pending (disp <wl-display>) (queue <wl-event-queue>))
  (wl-display-dispatch-queue-pending disp queue))

(define-method (dispatch-pending (disp <wl-display>))
  (wl-display-dispatch-pending disp))

(define-method (get-error (disp <wl-display>))
  (wl-display-get-error disp))

(define-method (get-protocol-error (disp <wl-display>))
  (wl-display-get-protocol-error disp))

(define-method (flush (disp <wl-display>))
  (wl-display-flush disp))

(define-method (roundtrip-queue (disp <wl-display>) (queue <wl-event-queue>))
  (wl-display-roundtrip-queue disp queue))

(define-method (roundtrip (disp <wl-display>))
  (wl-display-roundtrip disp))

(define-method (create-queue (disp <wl-display>))
  (wl-display-create-queue disp))

(define-method (prepare-read-queue (disp <wl-display>) (queue <wl-event-queue>))
  (wl-display-prepare-read-queue disp queue))

(define-method (prepare-read (disp <wl-display>))
  (wl-display-prepare-read disp))

(define-method (cancel-read (disp <wl-display>))
  (wl-display-cancel-read disp))

(define-method (read-events (disp <wl-display>))
  (wl-display-read-events disp))

(define-method
  (bind (registry <wl-registry>) name (cls <wl-proxy-class>) version)
  (make cls (bind registry name (interface cls) version)))
