;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (tests server)
  #:use-module (wayland server)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:duplicates (merge-generics))

(module-set! (resolve-module '(srfi srfi-64)) 'test-log-to-file #f)
(test-runner-current (test-runner-create))

(test-group "make <wl-display-server>"
  (destroy (make <wl-display-server>)))

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))

