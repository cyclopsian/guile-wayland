;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(load-extension "libguile-wayland" "scm_init_wayland")

(define-module (wayland client)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (wayland client core)
  #:use-module (wayland client protocol)
      #:export (name

                create-wrapper wrapper-destroy
                get-version get-id get-class get-symbol set-queue

                disconnect get-fd dispatch dispatch-queue
                dispatch-queue-pending dispatch-pending get-error
                get-protocol-error flush roundtrip-queue roundtrip create-queue
                prepare-read-queue prepare-read cancel-read read-events)
   #:re-export (<wl-display> <wl-event-queue>
                destroy wl-set-log-port-client)
   #:re-export-and-replace (initialize))

(module-use! (module-public-interface (current-module))
             (resolve-interface '(wayland client protocol)))

(define-method (name (interface <wl-interface>))
  (wl-interface-name interface))

(define-method (destroy (queue <wl-event-queue>))
  (wl-event-queue-destroy queue))

(define-method (initialze (proxy <wl-proxy>) (other <wl-proxy>))
  (wl-proxy-move other proxy))

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

(define-method (initialize (disp <wl-display>) args)
  (wl-proxy-move
    (match args
      ((arg)
        (cond
          ((is-a? arg <string>) (wl-display-connect arg))
          ((is-a? arg <integer>) (wl-display-connect-to-fd arg))))
      (() (wl-display-connect)))
    disp))

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
