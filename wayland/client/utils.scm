;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(load-extension "libguile-wayland" "scm_init_wayland")

(define-module (wayland client utils)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (srfi srfi-2)
  #:use-module (wayland client)
  #:use-module (wayland utils)
  #:export (create-argb-buffer <wl-mapped-buffer>
            wl-buffer fdes data stride size width height unmap destroy

            listen-interfaces <wl-object-store>
            get get-keywords wl-registry objects ids)
  #:re-export (initialize remove))

(define-class <wl-mapped-buffer> ()
  (wl-buffer #:accessor wl-buffer #:init-keyword #:wl-buffer)
  (fdes      #:accessor fdes      #:init-keyword #:fdes)
  (data      #:accessor data      #:init-keyword #:data)
  (stride    #:accessor stride    #:init-keyword #:stride)
  (size      #:accessor size      #:init-keyword #:size)
  (width     #:accessor width     #:init-keyword #:width)
  (height    #:accessor height    #:init-keyword #:height))

(define-method (unmap (buf <wl-mapped-buffer>))
  (when (data buf)
    (munmap (data buf))
    (set! (data buf) #f)
    (close (fdes buf))
    (set! (fdes buf) -1)))

(define-method (destroy (buf <wl-mapped-buffer>))
  (unmap buf)
  (destroy (wl-buffer buf)))

(define-method (create-argb-buffer (shm <wl-shm>) width height)
  (let* ((stride (* width 4))
         (size (* stride height))
         (fd (create-shm-fdes size))
         (data (mmap size (logior PROT_READ PROT_WRITE) MAP_SHARED fd))
         (pool (create-pool shm fd size))
         (buffer (create-buffer
                   pool 0 width height stride WL_SHM_FORMAT_ARGB8888)))
    (destroy pool)
    (make <wl-mapped-buffer>
          #:wl-buffer buffer #:fdes fd #:data data
          #:stride stride #:size size #:width width #:height height)))

(define-class <wl-object-store> ()
  (wl-registry #:accessor wl-registry)
  (objects     #:accessor objects #:init-thunk make-hash-table))
  (ids         #:accessor ids     #:init-thunk make-hash-table))

(define-method (initialize (store <wl-object-store>) . args)
  (apply (λ (disp)
           (wl-registry store (get-registry disp))) args))

(define-method (get (store <wl-object-store>) (cls <wl-proxy-class>))
  (hashq-ref (objects store) cls))

(define-generic remove)

(define-method (remove (store <wl-object-store>) (id <integer>))
  (and-let* ((pair (hashq-ref (ids store) id))
             (obj (car pair))
             (rem (cdr pair))
             (cls (class-of obj))
             (objs (hashq-ref (objects store) cls)))
    (rem obj)
    (hashq-remove! (ids store) id)
    (if (pair? objs)
        (begin (delq1! obj objs)
               (if (null? objs)
                   (hashq-remove! (objects store) cls)
                   (hashq-set! (objects store) cls objs)))
        (hashq-remove! (objects store) cls))
    (destroy obj)
    #t))

(define-method (remove (store <wl-object-store>) (obj <wl-proxy>))
  (remove store (get-id obj)))

(define-method (get-keywords (store <wl-object-store>))
  (hash-fold (λ (cls obj acc)
               (cons (symbol->keyword (get-symbol cls))
                     (cons obj acc))) '() (objects store)))

(define-method (destroy (store <wl-object-store>))
  (for-each (λ (obj) (remove store obj))
            (hash-map->list (λ (c o) o) (ids store)))
  (destroy (wl-registry store)))

(define-method (listen-interfaces (disp <wl-display>) . interfaces)
  (set! interfaces
    (map (λ (def) (if (is-a? def <class>) `(,def) def)) interfaces))
  (let ((store (make <wl-object-store> disp))
        (is-multiple? (λ (cls)
                        (->bool (member cls (list <wl-output> <wl-seat>)))))
        (unpack
          (lambda* (id interface obj-version cls
                       #:key (version 1)
                             (before noop) (after noop) (remove noop)
                             (mutliple? (is-multiple? cls)))
            (when (and (equal? interface (name cls)) (<= version obj-version))
              ((or before noop))
              (let ((obj (bind (wl-registry store) id def version))
                    (objs (hashq-ref (objects store) cls)))
                (hashq-set! (objects store) cls
                            (if multiple?
                                (if objs (cons obj objs) (list obj))
                                obj))
                (hashq-set! (ids store) id (cons obj (or remove noop)))
                ((or after noop) obj)))))
        (check-required
          (lambda* (cls #:key (required? #t) (version 1))
            (when (and required? (not (hashq-ref (objects store) cls)))
              (error (format #f "~a v~a interface not found"
                             (class-name cls) version))))))
    (add-listener
      (wl-registry store)
      #:global
      (λ (id int ver)
        (for-each (λ (def) (apply unpack id int ver def)) interfaces))
      #:global-remove
      (λ (id) (remove store id)))
    (dispatch disp)
    (roundtrip disp)
    (for-each (λ (def) (apply check-required def)) interfaces)
    store))


