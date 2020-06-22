;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(use-modules (ice-9 getopt-long)
             (ice-9 match)
             (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-26)
             (texinfo)
             (texinfo plain-text))

(define snarf-re (make-regexp "^\\^\\^ \\{ cname ([a-zA-Z_][a-zA-Z0-9_]*) \\^\\^ fname \"([^\"]+)\" \\^\\^ type primitive \\^\\^ location \"([^\"]+)\" ([0-9]+) \\^\\^ arglist \\(([^)]+)\\) \\^\\^ argsig ([0-9]+) ([0-9]+) ([0-9]+) \\^\\^ (.+) \\^\\^ \\}"))
(define scm-re (make-regexp "^SCM "))

(define (each-line proc)
  (let loop ()
    (let ((line (read-line)))
      (unless (eof-object? line)
        (begin
          (proc line)
          (loop))))))

(define (match->list m)
  (cdr (map (cut match:substring m <>) (iota (match:count m)))))

(define (parse-arglist arglist)
  (map (compose
         (cut regexp-substitute/global #f scm-re <> 'pre "" 'post)
         string-trim)
       (string-split arglist #\,)))

(define (parse-docstring strings)
  (with-input-from-string
    strings
    (λ ()
       (let loop ((result ""))
         (let ((str (read)))
           (if (eof-object? str)
               result
               (loop (string-append result str))))))))

(define (list-modify! li k proc)
  (list-set! li k (proc (list-ref li k))))

(define (read-snarfdoc)
  (define symbols '())
  (each-line
    (λ (line)
       (and=>
         (regexp-exec snarf-re line)
         (λ (m)
            (let ((m (match->list m)))
              (list-modify! m 3 string->number)
              (list-modify! m 4 parse-arglist)
              (list-modify! m 5 string->number)
              (list-modify! m 7 string->number)
              (list-modify! m 6 string->number)
              (list-modify! m 8 parse-docstring)
              (set! symbols (cons m symbols)))))))
  (reverse symbols))



(define* (snarfdoc->texi fields category #:key c-proc)
  (match-let*
    (((cname fname filename lineno arglist req opt var docstring) fields)
     (nice-arglist
       (string-join
         (append
           (list-head arglist req)
           (map (cut string-append "[" <> "]")
                (list-head (list-tail arglist req) opt))
           (if (> var 0)
               (list "." (list-ref arglist (+ req opt)))
               '()))
         " ")))
      (with-output-to-string
        (λ ()
           (format #t "@c snarfed from ~a:~a\n" filename lineno)
           (format #t "@deffn {~a} ~a ~a\n" category fname nice-arglist)
           (if c-proc
               (format #t "@deffnx {C Function} ~a (~a)\n" cname
                       (string-join arglist ", ")))
           (format #t "~a\n" docstring)
           (format #t "@end deffn")))))

(define* (transform-snarfdoc #:optional category
                             #:key manual docstrings)
  (for-each
    (λ (fields)
       (let ((texi (snarfdoc->texi fields category #:c-proc manual)))
         (if docstrings
             (begin
               (format #t "\n~a~a\n" #\page (cadr fields))
               (format #t "~a"
                       (stexi->plain-text (texi-fragment->stexi texi))))
             (begin
               (format #t "~a\n\n" texi)))))
    (read-snarfdoc)))

(define usage-text "\
~a [OPTIONS] [INFILE]

Supported options:
-o, --output=FILE          Write output to FILE
-c, --category=CATEGORY    Set procedure category (default: Scheme Procedure)
--manual                   Generate C procedures for manual
--docstrings               Output plaintext docstrings for ice-9 documentation
-h, --help                 Display this help
")

(define (main args)
  (let*
    ((option-spec '((output     (single-char #\o) (value #t))
                    (category   (single-char #\c) (value #t))
                    (manual                       (value #f))
                    (docstrings                   (value #f))
                    (help       (single-char #\h) (value #f))))
     (options (getopt-long args option-spec))
     (rest    (cdar options))
     (outport (match (option-ref options 'output "-")
                     ("-" (current-output-port))
                     (f (open-output-file f))))
     (inport  (match rest
                     ((or ("-") '()) (current-input-port))
                     ((f _ ...)      (open-input-file f))))
     (category    (option-ref options 'category "Scheme Procedure"))
     (manual      (option-ref options 'manual #f))
     (docstrings  (option-ref options 'docstrings #f))
     (help-wanted (option-ref options 'help #f)))

    (when help-wanted
      (format #t usage-text (car args))
      (quit 0))
    (when (and manual docstrings)
      (format (current-error-port)
              "Must specify only one of --manual or --docstrings\n")
      (quit 1))

    (with-input-from-port
      inport
      (cut with-output-to-port outport
           (cut transform-snarfdoc category
                #:manual manual #:docstrings docstrings)))))

(main (command-line))
(quit 0)
