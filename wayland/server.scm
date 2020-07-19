;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(eval-when (expand load eval)
  (load-extension "libguile-wayland" "scm_init_wayland_server"))

(define-module (wayland server)
  #:use-module (oop goops)
  #:use-module (wayland server core)
  #:use-module (wayland server protocol)
      #:export (interface destroy

                add-fd fd-update add-timer add-signal timer-update remove check
                dispatch dispatch-idle add-idle get-fd add-destroy-listener

                get-event-loop add-socket terminate run flush-clients
                destroy-clients get-serial next-serial
                add-client-created-listener

                get-interface get-client-list flush get-credentials get-object
                add-resource-created-listener

                for-each-resource map-resources get-resource-list get-id
                get-client get-version set-destructor get-class

                get-shm-buffer begin-access end-access get-data get-stride
                get-format get-width get-height ref-pool unref

                init-shm add-shm-format add-protocol-logger)
   #:re-export (<wl-event-source> <wl-event-loop> <wl-display-server>
                <wl-client> <wl-global> <wl-resource> <wl-listener>
                <wl-shm-buffer> <wl-shm-pool-ref> <wl-protocol-logger>
                <wl-resource-class>
                initialize wl-set-log-port-server)
  #:duplicates (merge-generics))

(module-use! (module-public-interface (current-module))
             (resolve-interface '(wayland server protocol)))

(define-syntax add-accessors
  (syntax-rules ()
    ((_ class)
     ((@@ (oop goops) compute-slot-accessors) class (class-slots class)))
    ((_ class name rest ...)
     (begin
       (define-accessor name)
       (slot-set! (class-slot-definition class 'name) 'accessor name)
       (add-accessors class rest ...)))))

(add-accessors <wl-resource-class> interface)

(define-method (destroy (listener <wl-listener>))
  (wl-listener-destroy listener))

(define-method (initialize (loop <wl-event-loop>))
  (let ((other (wl-event-loop-create)))
    (slot-set! loop 'loop (slot-ref other 'loop))
    (slot-set! other 'loop 0)))

(define-method (destroy (loop <wl-event-loop>))
  (wl-event-loop-destroy loop))

(define-method (add-fd (loop <wl-event-loop>) fd mask proc)
  (wl-event-loop-add-fd loop fd mask proc))

(define-method (fd-update (source <wl-event-source>) mask)
  (wl-event-source-fd-update source mask))

(define-method (add-timer (loop <wl-event-loop>) thunk)
  (wl-event-loop-add-timer loop thunk))

(define-method (add-signal (loop <wl-event-loop>) signal thunk)
  (wl-event-loop-add-signal loop signal thunk))

(define-method (timer-update (source <wl-event-source>) ms-delay)
  (wl-event-source-timer-update source ms-delay))

(define-method (remove (source <wl-event-source>))
  (wl-event-source-remove source))

(define-method (check (source <wl-event-source>))
  (wl-event-source-check source))

(define-method (dispatch (loop <wl-event-loop>) timeout)
  (wl-event-loop-dispatch loop timeout))

(define-method (dispatch-idle (loop <wl-event-loop>))
  (wl-event-loop-dispatch-idle loop))

(define-method (add-idle (loop <wl-event-loop>) thunk)
  (wl-event-loop-add-idle loop thunk))

(define-method (get-fd (loop <wl-event-loop>))
  (wl-event-loop-get-fd loop))

(define-method (add-destroy-listener (loop <wl-event-loop>) thunk)
  (wl-event-loop-add-destroy-listener loop thunk))

(define-method (initialize (disp <wl-display-server>))
  (let ((other (wl-display-create)))
    (slot-set! disp 'display (slot-ref other 'display))
    (slot-set! other 'display 0)))

(define-method (destroy (disp <wl-display-server>))
  (wl-display-destroy disp))

(define-method (get-event-loop (disp <wl-display-server>))
  (wl-display-get-event-loop disp))

(define-method (add-socket (disp <wl-display-server>) . args)
  (apply
    (case-lambda
      ((arg)
       (cond
         ((is-a? arg <string>) (wl-display-add-socket disp arg))
         ((is-a? arg <integer>) (wl-display-add-socket-fd disp arg))
         (else (scm-error 'wrong-type-arg "wl-display-server-initialize"
                          "Expected string, integer or #f: ~a"
                          (list arg) (list arg)))))
      (() (wl-display-add-socket-auto disp)))
    args))

(define-method (terminate (disp <wl-display-server>))
  (wl-display-terminate disp))

(define-method (run (disp <wl-display-server>))
  (wl-display-run disp))

(define-method (flush-clients (disp <wl-display-server>))
  (wl-display-flush-clients disp))

(define-method (destroy-clients (disp <wl-display-server>))
  (wl-display-destroy-clients disp))

(define-method (get-serial (disp <wl-display-server>))
  (wl-display-get-serial disp))

(define-method (next-serial (disp <wl-display-server>))
  (wl-display-next-serial disp))

(define-method (add-destroy-listener (disp <wl-display-server>) thunk)
  (wl-display-add-destroy-listener disp thunk))

(define-method (add-client-created-listener (disp <wl-display-server>) proc)
  (wl-display-add-client-created-listener disp proc))

(define-method (initialize (global <wl-global>) args)
  (apply
    (λ (disp interface version bind-proc)
      (wl-global-create global disp interface version bind-proc))
    args))

(define-method (remove (source <wl-global>))
  (wl-global-remove source))

(define-method (destroy (global <wl-global>))
  (wl-global-destroy global))

(define-method (get-interface (global <wl-global>))
  (wl-global-get-interface global))

(define-method (initialize (client <wl-client>) args)
  (apply
    (λ (disp fd)
      (let ((other (wl-client-create disp fd)))
        (slot-set! client 'client (slot-ref other 'client))
        (slot-set! other 'client 0)))
    args))

(define-method (get-client-list (disp <wl-display-server>))
  (wl-display-get-client-list disp))

(define-method (destroy (client <wl-client>))
  (wl-client-destroy client))

(define-method (flush (client <wl-client>))
  (wl-client-flush client))

(define-method (get-credentials (client <wl-client>))
  (wl-client-get-credentials client))

(define-method (get-fd (client <wl-client>))
  (wl-client-get-fd client))

(define-method (add-destroy-listener (client <wl-client>) thunk)
  (wl-client-add-destroy-listener client thunk))

(define-method (get-object (client <wl-client>) id)
  (wl-client-get-object client id))

(define-method (add-resource-created-listener (client <wl-client>) proc)
  (wl-client-add-resource-created-listener client proc))

(define-method (for-each-resource (client <wl-client>) proc)
  (wl-client-for-each-resource client proc))

(define-method (map-resources (client <wl-client>) proc)
  (let* ((acc '())
         (f (λ (res) (set! acc (cons (proc res) acc)))))
    (wl-client-for-each-resource client f)
    (reverse acc)))

(define-method (get-resource-list (client <wl-client>))
  (map-resources client identity))

(define-method (destroy (resource <wl-resource>))
  (wl-resource-destroy resource))

(define-method (get-id (resource <wl-resource>))
  (wl-resource-get-id resource))

(define-method (get-client (resource <wl-resource>))
  (wl-resource-get-client resource))

(define-method (get-version (resource <wl-resource>))
  (wl-resource-get-version resource))

(define-method (set-destructor (resource <wl-resource>) destructor)
  (wl-resource-set-destructor resource destructor))

(define-method (get-class (resource <wl-resource>))
  (wl-resource-get-class resource))

(define-method (add-destroy-listener (resource <wl-resource>) thunk)
  (wl-resource-add-destroy-listener resource thunk))

(define-method (get-shm-buffer (resource <wl-resource>))
  (wl-shm-buffer-get resource))

(define-method (begin-access (buffer <wl-shm-buffer>))
  (wl-shm-buffer-begin-access buffer))

(define-method (end-access (buffer <wl-shm-buffer>))
  (wl-shm-buffer-end-access buffer))

(define-method (get-data (buffer <wl-shm-buffer>))
  (wl-shm-buffer-get-data buffer))

(define-method (get-stride (buffer <wl-shm-buffer>))
  (wl-shm-buffer-get-stride buffer))

(define-method (get-format (buffer <wl-shm-buffer>))
  (wl-shm-buffer-get-format buffer))

(define-method (get-width (buffer <wl-shm-buffer>))
  (wl-shm-buffer-get-width buffer))

(define-method (get-height (buffer <wl-shm-buffer>))
  (wl-shm-buffer-get-height buffer))

(define-method (ref-pool (buffer <wl-shm-buffer>))
  (wl-shm-buffer-ref-pool buffer))

(define-method (unref (pool <wl-shm-pool-ref>))
  (wl-shm-pool-unref pool))

(define-method (init-shm (disp <wl-display-server>))
  (wl-display-init-shm disp))

(define-method (add-shm-format (disp <wl-display-server>) format)
  (wl-display-add-shm-format disp format))

(define-method (add-protocol-logger (disp <wl-display-server>) proc)
  (wl-display-add-protocol-logger disp proc))

(define-method (destroy (logger <wl-protocol-logger>))
  (wl-protocol-logger-destroy logger))
