;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland scanner)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 string-fun)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (texinfo string-utils)
  #:export (wl-scanner wl-scanner-main))

(define-class <wl-def-object> () description summary)
(define-class <wl-protocol> (<wl-def-object>)
              (name #:init-keyword #:name)
              copyright
              (interfaces #:init-value '()))
(define-class <wl-interface> (<wl-def-object>)
              (name     #:init-keyword #:name)
              (version  #:init-keyword #:version)
              (requests #:init-value '())
              (events   #:init-value '())
              (enums    #:init-value '()))
(define-class <wl-event> (<wl-def-object>)
              (name  #:init-keyword #:name)
              (since #:init-keyword #:since)
              (args  #:init-keyword #:args #:init-value '()))
(define-class <wl-request> (<wl-event>)
              (type  #:init-keyword #:type))
(define-class <wl-arg> ()
              (name       #:init-keyword #:name)
              (type       #:init-keyword #:type)
              (summary    #:init-keyword #:summary)
              (interface  #:init-keyword #:interface)
              (allow-null #:init-keyword #:allow-null)
              (enum       #:init-keyword #:enum))
(define-class <wl-enum> (<wl-def-object>)
              (name     #:init-keyword #:name)
              (since    #:init-keyword #:since)
              (bitfield #:init-keyword #:bitfield)
              (entries  #:init-keyword #:entries #:init-value '()))
(define-class <wl-entry> (<wl-def-object>)
              (name    #:init-keyword #:name)
              (value   #:init-keyword #:value)
              (since   #:init-keyword #:since))

(define-syntax sxml-match-children-internal
  (syntax-rules ()
    ((sxml-match-children func children ((name args ...) body ...) ...)
     (func
       (λ (elem)
         (match
           (car elem)
           ((quote name) (sxml-match elem ((name args ...) body ...))) ...
           (else #f)))
      (filter (negate string?) children)))))

(define-syntax sxml-match-children
  (syntax-rules ()
    ((sxml-match-children args ...)
     (sxml-match-children-internal for-each args ...))))

(define-syntax sxml-match-children-map
  (syntax-rules ()
    ((sxml-match-children-map args ...)
     (sxml-match-children-internal map args ...))))

(define (read-protocol)
  (define (get-args elems)
    (filter
      identity
      (sxml-match-children-map
        elems
        ((arg (@ (name ,name) (type ,type)
                 (summary (,summary #f)) (interface (,interface #f))
                 (allow-null (,allow-null #f)) (enum (,enum #f))))
         (make <wl-arg>
               #:name name #:type type #:summary summary
               #:interface interface #:allow-null allow-null #:enum enum)))))

  (define (get-entries elems)
    (filter
      identity
      (sxml-match-children-map
        elems
        ((entry (@ (name ,name) (value ,value)
                   (since (,since #f)) (summary (,summary #f))) . ,children)
         (let ((entry (make <wl-entry> #:name name #:value value
                            #:since since #:summary summary)))
           (get-description entry children))))))

  (define (get-description obj children)
    (sxml-match-children
      children
      ((description (@ (summary ,summary)) . ,text)
       (slot-set! obj 'description (string-concatenate text))
       (slot-set! obj 'summary     summary))))

  (define (read-version version)
    (if version (string->number version) #f))

  (sxml-match
    (caddr (xml->sxml #:trim-whitespace? #t))
    ((protocol (@ (name ,proto-name)) . ,proto-children)
     (define protocol (make <wl-protocol> #:name proto-name))
     (get-description protocol proto-children)
     (sxml-match-children
       proto-children
       ((copyright  ,text) (slot-set! protocol 'copyright text))
       ((interface (@ (name ,int-name) (version ,int-version)) . ,int-children)
        (define interface (make <wl-interface>
                                #:name int-name
                                #:version (string->number int-version)))
        (get-description interface int-children)
        (slot-set! protocol 'interfaces
                   (append (slot-ref protocol 'interfaces) (list interface)))
        (sxml-match-children
          int-children
          ((request (@ (name   ,req-name)
                       (type  (,req-type #f))
                       (since (,req-since #f))) . ,req-children)
           (define request (make <wl-request>
                                 #:name req-name
                                 #:type req-type
                                 #:since (read-version req-since)
                                 #:args (get-args req-children)))
           (get-description request req-children)
           (slot-set! interface 'requests
                      (append (slot-ref interface 'requests)
                              (list request))))
          ((event (@ (name   ,ev-name)
                     (since (,ev-since #f))) . ,ev-children)
           (define event (make <wl-event>
                               #:name ev-name #:since (read-version ev-since)
                               #:args (get-args ev-children)))
           (get-description event ev-children)
           (slot-set! interface 'events
                      (append (slot-ref interface 'events)
                              (list event))))
          ((enum (@ (name      ,en-name)
                    (bitfield (,en-bitfield #f))
                    (since    (,en-since #f))) . ,en-children)
           (define enum (make <wl-enum>
                               #:name en-name
                               #:bitfield en-bitfield
                               #:since (read-version en-since)
                               #:entries (get-entries en-children)))
           (get-description enum en-children)
           (slot-set! interface 'enums
                      (append (slot-ref interface 'enums)
                              (list enum)))))))
     protocol)))

(define* (format-text text #:key (prefix ""))
  ((compose
     (cut string-join <> "\n")
     (cut map (cut string-append prefix <>) <>)
     reverse
     (cut drop-while string-null? <>)
     reverse
     (cut drop-while string-null? <>)
     (cut map string-trim <>)
     (cut string-split <> #\newline))
   text))

(define* (format-docstring event #:key (prefix ""))
  (string-join
    (filter
      (negate string-null?)
      (list
        (if (slot-bound? event 'summary) (slot-ref event 'summary) "")
        (if (slot-bound? event 'description)
            (format-text (slot-ref event 'description) #:prefix prefix) "")))
    "\n\n"))

(define-class <scheme-formatter> ()
              (exports  #:init-value '())
              (generics #:init-value '(add-listener destroy)))

(define-generic format-symbol)
(define-generic format-args)
(define-generic format-func-name)
(define-generic format-stub)
(define-generic format-protocol-header)
(define-generic format-add-listener)
(define-generic format-destructor)
(define-generic format-interface-name)
(define-generic format-messages)
(define-generic format-protocol-footer)

(define-method (format-symbol (f <scheme-formatter>) str)
  (string->symbol (string-replace-substring str "_" "-")))

(define* (format-symbol-append f . rest)
         (format-symbol f (string-concatenate
                            (map (cut format #f "~a" <>) rest))))

(define-method (format-args (f <scheme-formatter>) event call)
  (fold-right
    (λ (arg acc)
      (match (slot-ref arg 'type)
             ("new_id"
              (if (slot-ref arg 'interface)
                  (if call (cons #f acc) acc)
                  (append (list
                            (if call
                                '(wl-interface-name interface)
                                'interface) 'version) acc)))
             (else
               (cons (format-symbol f (slot-ref arg 'name)) acc))))
    '() (slot-ref event 'args)))

(define-method (format-func-name
                 (f <scheme-formatter>)
                 interface
                 (request <wl-request>))
  (format-symbol f (format #f "~a_~a"
                           (slot-ref interface 'name)
                           (slot-ref request 'name))))

(define-method (format-func-name
                 (f <scheme-formatter>)
                 interface
                 (event <wl-event>))
  (format-symbol f (format #f "~a_send_~a"
                           (slot-ref interface 'name)
                           (slot-ref event 'name))))

(define-method (format-stub (f <scheme-formatter>) interface request index)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (req-name (format-symbol f (slot-ref request 'name)))
         (name (format-func-name f interface request))
         (ret (find (compose (cut equal? <> "new_id") (cut slot-ref <> 'type))
                    (slot-ref request 'args)))
         (marshal-func
           (if ret
               (if (slot-ref ret 'interface)
                   'wl-proxy-marshal-constructor
                   'wl-proxy-marshal-constructor-versioned)
               'wl-proxy-marshal)))
    (slot-set! f 'exports (cons name (slot-ref f 'exports)))
    (slot-set! f 'generics (lset-adjoin eq? (slot-ref f 'generics) req-name))
    ; TODO - interface should be interface->name in function call
    (format #t "(define ~a\n  \"~a\"\n  ~a)\n"
            (append (list name int-name) (format-args f request #f))
            (escape-special-chars (format-docstring request) "\"\\" #\\)
            (append `(,marshal-func ,int-name ,index)
                    (if ret
                        (let ((ret-int (slot-ref ret 'interface)))
                          (if ret-int
                            (list (format-interface-name f ret-int))
                            '(interface version)))
                        '())
                    (format-args f request #t)))))

(define-method (format-protocol-header (f <scheme-formatter>) protocol)
  (if (slot-bound? protocol 'copyright)
      (format #t "~a\n\n"
              (format-text (slot-ref protocol 'copyright) #:prefix ";;; ")))
  (pretty-print '(use-modules (oop goops)
                              (wayland client))))

(define-method (format-add-listener (f <scheme-formatter>) interface)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (name (format-symbol-append f int-name "_add_listener"))
         (events (map (compose (cut format-symbol f <>)
                               (cut slot-ref <> 'name))
                      (slot-ref interface 'events))))
    (slot-set! f 'exports (cons name (slot-ref f 'exports)))
    (pretty-print
      `(define* (,name ,int-name #:key ,@events)
                (wl-proxy-add-listener ,int-name ,@events)))))

(define-method (format-destructor (f <scheme-formatter>) interface)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (name (format-symbol-append f int-name "_destroy"))
         (destructor (list-index
                       (λ (req) (equal? (slot-ref req 'type) "destructor"))
                       (slot-ref interface 'requests))))
    (slot-set! f 'exports (cons name (slot-ref f 'exports)))
    (pretty-print
      `(define (,name ,int-name)
        ,@(if destructor `((wl-proxy-marshal ,int-name ,destructor)) '())
        (wl-proxy-destroy ,int-name)))))

(define-method (format-interface-name (f <scheme-formatter>) interface-name)
  (format-symbol-append f interface-name "_interface"))

(define-method (format-messages (f <scheme-formatter>) events)
  (map
    (λ (event)
      (list
        'list
        (slot-ref event 'name)
        (string-concatenate
          (map
            (λ (arg)
               (let ((type (slot-ref arg 'type)))
                 (string-append
                   (if (equal? (slot-ref arg 'allow-null) "true") "?" "")
                   (if (and (equal? type "new_id")
                            (not (slot-ref arg 'interface))) "su" "")
                   (match type
                          ("int"    "i") ("uint"   "u") ("fixed"  "f")
                          ("string" "s") ("object" "o") ("new_id" "n")
                          ("array"  "a") ("fd"     "h")))))
            (slot-ref event 'args)))
        (cons 'list
              (fold-right
                (λ (arg acc)
                   (let* ((interface (slot-ref arg 'interface))
                          (int-name (if interface
                                        (format-interface-name f interface)
                                        #f))
                          (type      (slot-ref arg 'type)))
                     (match type
                            ("new_id" (if interface
                                          (cons int-name acc)
                                          (append '(#f #f #f) acc)))
                            ("object" (cons int-name acc))
                            (else     (cons #f acc)))))
                '() (slot-ref event 'args)))))
    events))

(define-method (format-protocol-footer (f <scheme-formatter>) protocol)

  (for-each
    (λ (interface)
       (let ((name (format-interface-name f (slot-ref interface 'name))))
         (slot-set! f 'exports (cons name (slot-ref f 'exports)))
         (pretty-print `(define ,name
                          (make-wl-interface
                            ,(slot-ref interface 'name)
                            ,(slot-ref interface 'version)
                            ,(cons 'list (format-messages
                               f (slot-ref interface 'requests)))
                            ,(cons 'list (format-messages
                               f (slot-ref interface 'events))))))
         (newline)))
     (slot-ref protocol 'interfaces))

  (for-each
    (λ (generic)
       (pretty-print `(define-generic ,generic)))
    (slot-ref f 'generics))
  (newline)

  (for-each
    (λ (interface)
       (let* ((name (format-symbol f (slot-ref interface 'name)))
              (int-name (format-interface-name f (slot-ref interface 'name)))
              (class-name (format-symbol-append f "<" name ">")))
         (pretty-print `(define-class ,class-name (<wl-proxy>)))
         (pretty-print
           `(define-method
              (initialize (,name ,class-name) (proxy <wl-proxy>))
              (unless (equal? (slot-ref proxy 'interface)
                              (slot-ref ,int-name 'interface))
                (scm-error 'wrong-type-arg "initialize"
                           ,(string-append
                              "Wrong type proxy in initialize: "
                              "got ~a, expected " (slot-ref interface 'name))
                           (list (wl-proxy-get-class proxy)) (list proxy)))
              (slot-set! ,name 'proxy (slot-ref proxy 'proxy))
              (slot-set! ,name 'interface (slot-ref proxy 'interface))))
         (newline)

         (for-each
           (λ (request)
              (unless (equal? (slot-ref request 'type) "destructor")
                (let ((req-name (format-symbol f (slot-ref request 'name)))
                      (func-name (format-func-name f interface request))
                      (args (format-args f request #f)))
                  (pretty-print
                    `(define-method
                       (,req-name (,name ,class-name) ,@args)
                       (,func-name ,name ,@args)))
                  (newline))))
           (slot-ref interface 'requests))

         (unless (null? (slot-ref interface 'events))
           (pretty-print
             `(define-method
                (add-listener (,name ,class-name) . args)
                (apply ,(format-symbol-append f name "_add_listener")
                       ,name args)))
           (newline))

         (pretty-print
           `(define-method
              (destroy (,name ,class-name))
              (,(format-symbol-append f name "_destroy") ,name)))
         (newline)

         (newline)))
    (slot-ref protocol 'interfaces))

  (pretty-print `(export ,@(reverse (slot-ref f 'exports))
                         ,@(slot-ref f 'generics))))

(define* (wl-scanner type)
  (define protocol (read-protocol))
  (define f
    (match type
           ('client (make <scheme-formatter>))))

  (format-protocol-header f protocol)
  (newline) (newline)

  (for-each
    (λ (interface)
      (fold
        (λ (request index)
           (unless (equal? (slot-ref request 'type) "destructor")
             (format-stub f interface request index)
             (newline))
           (1+ index))
        0 (slot-ref interface 'requests))
      (newline)

      (unless (null? (slot-ref interface 'events))
        (format-add-listener f interface)
        (newline))

      (unless (equal? (slot-ref interface 'name) "wl_display")
        (format-destructor f interface)
        (newline))

      (newline))
    (slot-ref protocol 'interfaces))
  (format-protocol-footer f protocol))

(define usage-text "\
[OPTIONS] [client|server|c-client|c-server] [input_file output_file]

Supported options:
-h, --help                 Display this help
")

(define (wl-scanner-main args)
  (let*
    ((option-spec '((help       (single-char #\h) (value #f))))
     (options (getopt-long args option-spec))
     (rest    (cdar options))
     (help-wanted (option-ref options 'help #f)))

    (when help-wanted
      (format #t "~a ~a" usage-text (car args))
      (quit 0))

    (match
      rest
      ((type input output)
       (let
         ((outport (match output
                          ("-" (current-output-port))
                          (f (open-output-file f))))
          (inport  (match input
                          ("-" (current-input-port))
                          (f (open-input-file f)))))
         (with-input-from-port
           inport
           (cut with-output-to-port outport
                (cut wl-scanner (string->symbol type))))))
      (_
        (format #t "~a ~a" usage-text (car args))
        (quit 1)))))
