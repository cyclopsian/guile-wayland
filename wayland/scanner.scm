;;;; -*- coding: utf-8; mode: scheme -*-
;;;; SPDX-FileCopyrightText: 2020 Jason Francis <jason@cycles.network>
;;;; SPDX-License-Identifier: GPL-3.0-or-later

(define-module (wayland scanner)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format-pretty f)
  #:use-module (ice-9 string-fun)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (texinfo string-utils)
  #:use-module (wayland config)
  #:export (wl-scanner wl-scanner-load wl-scanner-main)
  #:re-export (*wl-protocol-dir*))

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
     (cut string-split <> #\format-newline f))
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
              (expressions  #:init-value '())
              (exports      #:init-value '())
              (generics     #:init-value '(add-listener destroy)))

(define-method (format-pretty (f <scheme-formatter>) expr)
  (slot-set! f 'expressions (cons expr (slot-ref f 'expressions))))

(define-method (format-newline (f <scheme-formatter>)))

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
    ;(slot-set! f 'exports (cons name (slot-ref f 'exports)))
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

(define-method (format-protocol-header
                 (f <scheme-formatter>) type protocol module-prefix extra-deps)
  (if (slot-bound? protocol 'copyright)
      (format #t "~a\n\n"
              (format-text (slot-ref protocol 'copyright) #:prefix ";;; ")))

  (format-pretty f
    '(load-extension "libguile-wayland" "scm_init_wayland"))

  (format-newline f)

  (let* ((proto-name (slot-ref protocol 'name))
         (core-proto? (equal? proto-name "wayland"))
         (mod-name (format-symbol
                     f (if core-proto? "protocol"
                         (string-append proto-name "-protocol"))))
         (module-deps `((oop goops)
                        (wayland ,type core)
                        ,@(if core-proto? '() `((wayland ,type protocol)))
                        ,@extra-deps)))
    (if module-prefix
      (format-pretty f `(define-module
                       (,@module-prefix ,mod-name)
                       ,@(fold-right
                           (λ (m acc) (append (list '#:use-module m) acc))
                           '() module-deps)))
      (format-pretty f `(use-modules ,@module-deps))))

  (format-newline f)
  (for-each
    (λ (generic)
       (format-pretty f `(define-generic ,generic)))
    (slot-ref f 'generics))
  (format-newline f))

(define-method (format-add-listener (f <scheme-formatter>) interface)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (name (format-symbol-append f int-name "_add_listener"))
         (events (map (compose (cut format-symbol f <>)
                               (cut slot-ref <> 'name))
                      (slot-ref interface 'events))))
    ;(slot-set! f 'exports (cons name (slot-ref f 'exports)))
    (format-pretty f
      `(define* (,name ,int-name #:key ,@events)
                (wl-proxy-add-listener ,int-name ,@events)))))

(define-method (format-destructor (f <scheme-formatter>) interface)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (name (format-symbol-append f int-name "_destroy"))
         (destructor (list-index
                       (λ (req) (equal? (slot-ref req 'type) "destructor"))
                       (slot-ref interface 'requests))))
    ;(slot-set! f 'exports (cons name (slot-ref f 'exports)))
    (format-pretty f
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
       (let* ((name (format-symbol f (slot-ref interface 'name)))
              (int-name (format-interface-name f (slot-ref interface 'name)))
              (class-name (format-symbol-append f "<" name ">")))
         (slot-set! f 'exports (cons class-name (slot-ref f 'exports)))
         (format-pretty f `(define-class ,class-name (<wl-proxy>)))
         (format-pretty f
           `(define-method
              (initialize (,name ,class-name) args)
              (let ((proxy (car args)))
                (wl-proxy-assert-type proxy ,int-name)
                (wl-proxy-move proxy ,name))))
         (format-newline f)

         (for-each
           (λ (request)
              (unless (equal? (slot-ref request 'type) "destructor")
                (let ((req-name (format-symbol f (slot-ref request 'name)))
                      (func-name (format-func-name f interface request))
                      (args (format-args f request #f)))
                  (format-pretty f
                    `(define-method
                       (,req-name (,name ,class-name) ,@args)
                       (,func-name ,name ,@args)))
                  (format-newline f))))
           (slot-ref interface 'requests))

         (unless (null? (slot-ref interface 'events))
           (format-pretty f
             `(define-method
                (add-listener (,name ,class-name) . args)
                (apply ,(format-symbol-append f name "_add_listener")
                       ,name args)))
           (format-newline f))

         (unless (equal? (slot-ref interface 'name) "wl_display")
           (format-pretty f
             `(define-method
                (destroy (,name ,class-name))
                (,(format-symbol-append f name "_destroy") ,name))))
         (format-newline f)

         (format-newline f)))
    (slot-ref protocol 'interfaces))

  (format-pretty f `(export ,@(reverse (slot-ref f 'exports))
                         ,@(slot-ref f 'generics))))

(define-method (flush (f <scheme-formatter>))
  (let ((exprs (slot-ref f 'expressions)))
    (slot-set! f 'expressions '())
    exprs))

(define-class <scheme-text-formatter> (<scheme-formatter>) port)

(define-method (initialize (f <scheme-text-formatter>) args)
  (slot-set! f 'port (match args ((#f)   (open-output-string))
                                 ((port) port))))

(define-method (format-pretty (f <scheme-text-formatter>) expr)
  (pretty-print expr (slot-ref f 'port) #:max-expr-width 79))

(define-method (format-newline (f <scheme-text-formatter>))
  (newline (slot-ref f 'port)))

(define-method (flush (f <scheme-text-formatter>))
  (let* ((port (slot-ref f 'port))
         (str (get-output-string port)))
    (close-port port)
    str))

(define* (wl-scanner #:key (type 'client)
                           output-port
                           input-port
                           (extra-deps '())
                           module-prefix
                           output-text?)
  (unless (member type '("client" "server"))
    (error "type must be one of [client|server]"))
  (when (eq? output-port #t)
    (set! output-port (current-output-port)))
  (unless input-port
    (set! input-port (current-input-port)))

  (define f (if output-text?
                (make <scheme-text-formatter> output-port)
                (make <scheme-formatter>)))
  (define protocol (with-input-from-port port read-protocol))

  (format-protocol-header f type protocol module-prefix extra-deps)
  (format-newline f) (format-newline f)

  (for-each
    (λ (interface)
      (let ((name (format-interface-name f (slot-ref interface 'name))))
        (slot-set! f 'exports (cons name (slot-ref f 'exports)))
        (format-pretty f `(define ,name (make-wl-interface)))))
    (slot-ref protocol 'interfaces))
  (format-newline f)
  (for-each
    (λ (interface)
      (let ((name (format-interface-name f (slot-ref interface 'name))))
        (format-pretty f
          `(wl-interface-set ,name
                             ,(slot-ref interface 'name)
                             ,(slot-ref interface 'version)
                             ,(cons 'list (format-messages
                                            f (slot-ref interface 'requests)))
                             ,(cons 'list (format-messages
                                            f (slot-ref interface 'events)))))
        (format-newline f)))
    (slot-ref protocol 'interfaces))

  (for-each
    (λ (interface)
      (fold
        (λ (request index)
           (unless (equal? (slot-ref request 'type) "destructor")
             (format-stub f interface request index)
             (format-newline f))
           (1+ index))
        0 (slot-ref interface 'requests))
      (format-newline f)

      (unless (null? (slot-ref interface 'events))
        (format-add-listener f interface)
        (format-newline f))

      (unless (equal? (slot-ref interface 'name) "wl_display")
        (format-destructor f interface)
        (format-newline f))

      (format-newline f))
    (slot-ref protocol 'interfaces))
  (format-protocol-footer f protocol)
  (unless output-port (flush f)))

(define* (wl-scanner-load file . args)
  (eval
    `(begin ,@(with-input-from-file file
                (cut apply wl-scanner (append args '(#:input-port #f
                                                     #:output-port #f
                                                     #:output-text? #f
                                                     #:module-prefix #f)))))
    (current-module)))

(define usage-text "\
[OPTIONS] [client|server] [input_file output_file]

Supported options:
-h, --help                 Display this help

-m, --module-prefix        Export the protocol as a module with given prefix
    Example: -m \"myapp protocols\"

-d, --extra-deps           Extra module dependencies
    Example: -d \"(myapp protocols xdg-shell) (myapp protocols customproto)\"
")

(define (wl-scanner-main args)
  (let*
    ((option-spec '((module-prefix (single-char #\m) (value #t))
                    (extra-deps    (single-char #\d) (value #t))
                    (help          (single-char #\h) (value #f))))
     (options (getopt-long args option-spec))
     (rest    (cdar options))
     (module-prefix (option-ref options 'module-prefix #f))
     (extra-deps    (option-ref options 'extra-deps ""))
     (help-wanted   (option-ref options 'help #f)))

    (when help-wanted
      (format #t "Usage: ~a ~a" (car args) usage-text)
      (quit 0))

    (define (read-string-vals str)
      (with-input-from-string str
        (λ ()
          (let lp ((vs '()))
            (let ((v (read)))
              (if (eof-object? v)(reverse vs) (lp (cons v vs))))))))

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
         (unless (member type '("client" "server"))
           (format (current-error-port)
                   "Invalid type: ~a\n  First argument must be one of [client|server]\n\n"
                   type)
           (format (current-error-port) "Usage: ~a ~a" (car args) usage-text)
           (quit 1))
         (wl-scanner
           #:type          (string->symbol type)
           #:output-port   outport
           #:input-port    inport
           #:output-text?  #t
           #:extra-deps    (read-string-vals extra-deps)
           #:module-prefix (and=> module-prefix
                             (compose (cut map string->symbol <>)
                                      (cut string-split <> #\space))))))
      (_
        (format (current-error-port) "Usage: ~a ~a" (car args) usage-text)
        (quit 1)))))
