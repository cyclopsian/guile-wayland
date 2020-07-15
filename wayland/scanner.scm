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
           (get-description entry children)
           entry)))))

  (define (get-description obj children)
    (sxml-match-children
      children
      ((description (@ (summary ,summary)) . ,text)
       (slot-set! obj 'description (string-concatenate text))
       (slot-set! obj 'summary     summary))))

  (define (read-version version)
    (if version (string->number version) #f))

  (sxml-match
    (last (xml->sxml #:trim-whitespace? #t))
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

;;; <scheme-formatter>
;;; Formats code as sexps

(define-class <scheme-formatter> ()
              (expressions  #:init-value '())
              (exports      #:init-value '())
              (generics     #:init-value '()))

(define-method (add-export (f <scheme-formatter>) name)
  (slot-set! f 'exports (cons name (slot-ref f 'exports))))

(define-method (add-generic (f <scheme-formatter>) name)
  (slot-set! f 'generics (lset-adjoin eq? (slot-ref f 'generics) name)))

(define-method (format-text (f <scheme-formatter>) text prefix))

(define-method (format-pretty (f <scheme-formatter>) expr)
  (slot-set! f 'expressions (cons expr (slot-ref f 'expressions))))

(define-method (format-newline (f <scheme-formatter>)))

(define-method (format-symbol (f <scheme-formatter>) str)
  (string->symbol (string-replace-substring str "_" "-")))

(define* (format-symbol-append f . rest)
         (format-symbol f (string-concatenate
                            (map (cut format #f "~a" <>) rest))))

(define-method (format-interface-name (f <scheme-formatter>) interface-name)
  (format-symbol-append f interface-name "_interface"))

(define-method (format-class-name (f <scheme-formatter>) name)
  (format-symbol-append f "<" name ">"))

(define-method (format-enum-entry (f <scheme-formatter>) interface enum entry)
  ((compose string->symbol string-upcase string-join)
   (map (cut slot-ref <> 'name) (list interface enum entry)) "_"))

(define-method (flush (f <scheme-formatter>))
  (let ((exprs (slot-ref f 'expressions)))
    (slot-set! f 'expressions '())
    exprs))

;;; <scheme-text-formatter>
;;; Formats code as a string to an output port

(define-class <scheme-text-formatter> (<scheme-formatter>) port)

(define-method (initialize (f <scheme-text-formatter>) args)
  (slot-set! f 'port (match args ((#f)   (open-output-string))
                                 ((port) port)))
  (next-method f '()))

(define-method (format-text (f <scheme-text-formatter>) text prefix)
  (display
    ((compose
       (cut string-join <> "\n")
       (cut map (cut string-append prefix <>) <>)
       reverse
       (cut drop-while string-null? <>)
       reverse
       (cut drop-while string-null? <>)
       (cut map string-trim <>)
       (cut string-split <> #\newline))
     text)
    (slot-ref f 'port)))

(define-method (format-pretty (f <scheme-text-formatter>) expr)
  (pretty-print expr (slot-ref f 'port) #:max-expr-width 79))

(define-method (format-newline (f <scheme-text-formatter>))
  (newline (slot-ref f 'port)))

(define-method (flush (f <scheme-text-formatter>))
  (let* ((port (slot-ref f 'port))
         (str (get-output-string port)))
    (close-port port)
    str))

;;; Interface and method formatters

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

(define-method (format-protocol-header
                 (f <scheme-formatter>) type protocol module-prefix extra-deps)
  (when (slot-bound? protocol 'copyright)
    (format-text f (slot-ref protocol 'copyright) ";;; ")
    (format-newline f)
    (format-newline f))

  (let ((type-str (symbol->string type)))
    (format-pretty f
      `(eval-when (expand load eval)
        (load-extension ,(string-append "libguile-wayland-" type-str)
                        ,(string-append "scm_init_wayland_" type-str)))))

  (format-newline f)

  (let* ((proto-name (slot-ref protocol 'name))
         (core-proto? (equal? proto-name "wayland"))
         (mod-name (format-symbol f (if core-proto? "protocol" proto-name)))
         (module-deps `((oop goops)
                        (wayland ,type core)
                        ,@(if core-proto? '() `((wayland ,type protocol)))
                        (wayland util)
                        ,@extra-deps)))
    (if module-prefix
      (format-pretty f `(define-module
                       (,@module-prefix ,mod-name)
                       ,@(fold-right
                           (λ (m acc) (append (list '#:use-module m) acc))
                           '() module-deps)
                       #:export (,@(reverse (slot-ref f 'exports))
                                 ,@(reverse (slot-ref f 'generics)))
                       #:re-export (initialize)))
      (format-pretty f `(use-modules ,@module-deps))))

  (format-newline f)

  (for-each
    (λ (generic)
       (format-pretty f `(define-generic ,generic)))
    (slot-ref f 'generics))
  (format-newline f)

  (for-each
    (λ (interface)
      (let ((int-name (format-interface-name f (slot-ref interface 'name))))
        (format-pretty f `(define ,int-name (make-wl-interface)))))
    (slot-ref protocol 'interfaces))
  (format-newline f))

(define-method (format-interface-header (f <scheme-formatter>) interface)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (int-name (format-interface-name f (slot-ref interface 'name)))
         (class-name (format-symbol-append f "<" name ">")))
    (format-pretty f
      `(wl-interface-set ,int-name
                         ,(slot-ref interface 'name)
                         ,(slot-ref interface 'version)
                         ,(cons 'list (format-messages
                                        f (slot-ref interface 'requests)))
                         ,(cons 'list (format-messages
                                        f (slot-ref interface 'events)))))
    (format-newline f)
    (format-pretty f `(define-class ,class-name (<wl-proxy>)
                                    #:metaclass <wl-proxy-class>))
    (format-pretty f `(slot-set! ,class-name 'interface ,int-name))
    (format-newline f)
    (format-pretty f
      `(define-method
         (initialize (,name ,class-name) args)
         (apply (lambda (proxy)
                  (wl-proxy-assert-type proxy ,int-name)
                  (wl-proxy-move proxy ,name))
                args)))
    (format-newline f)))

(define-method (format-enum (f <scheme-formatter>) interface enum)
  (for-each
    (λ (entry)
      (let* ((name (format-enum-entry f interface enum entry))
             (value-str (slot-ref entry 'value))
             (value (string->number
                      (if (string-prefix-ci? "0x" value-str)
                          (string-append "#" (substring/shared value-str 1))
                          value-str))))
        (format-pretty f `(define ,name ,value))))
    (slot-ref enum 'entries))
  (format-newline f))

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
                                'interface)
                            'version) acc)))
             (else
               (cons (format-symbol f (slot-ref arg 'name)) acc))))
    '() (slot-ref event 'args)))

(define-method (format-stub (f <scheme-formatter>) interface request index)
  (let* ((int-name (format-symbol f (slot-ref interface 'name)))
         (req-name (format-symbol f (slot-ref request 'name)))
         (class-name (format-class-name f int-name))
         (ret (find (λ (p) (equal? (slot-ref p 'type) "new_id"))
                    (slot-ref request 'args)))
         (ret-int (and ret (slot-ref ret 'interface)))
         (marshal-func
           (if ret
               (if ret-int
                   'wl-proxy-marshal-constructor
                   'wl-proxy-marshal-constructor-versioned)
               'wl-proxy-marshal))
         (ret-args
           (if ret
               (if ret-int
                   (list (format-interface-name f ret-int))
                   '(interface version))
               '()))
         (func-call `(,marshal-func ,int-name ,index ,@ret-args
                                    ,@(format-args f request #t))))
    (format-pretty f
      `(define-method
         (,req-name (,int-name ,class-name) ,@(format-args f request #f))
         ,(if ret
              (if ret-int
                  `(make ,(format-class-name f ret-int) ,func-call)
                  `(make interface ,func-call))
              func-call)))))

(define-method (format-add-listener (f <scheme-formatter>) interface)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (class-name (format-class-name f name))
         (events (map (λ (e) (format-symbol f (slot-ref e 'name)))
                      (slot-ref interface 'events))))
    (format-pretty f
      `(define-method
         (add-listener (,name ,class-name) . args)
         (apply
           (lambda* (#:key ,@events)
                    (wl-proxy-add-listener ,name ,@events))
           args)))))

(define-method (format-destructor (f <scheme-formatter>) interface)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (class-name (format-class-name f name))
         (destructor (list-index
                       (λ (req) (equal? (slot-ref req 'type) "destructor"))
                       (slot-ref interface 'requests))))
    (format-pretty f
      `(define-method
         (destroy (,name ,class-name))
         ,@(if destructor `((wl-proxy-marshal ,name ,destructor)) '())
         (wl-proxy-destroy ,name)))))

(define* (wl-scanner #:key (type 'client)
                           output-port
                           input-port
                           (extra-deps '())
                           module-prefix
                           output-text?)
  "Takes a wayland XML file from @var{input-port} and outputs Scheme code
to @var{output-port}. If @var{input-port} is omitted, it defaults to the
current input port. If @var{output-port} is omitted or set to @code{#f},
returns the code as a list of S-expressions. If @var{output-port} is set to
@code{#t}, it defaults to the current output port.

@var{output-text?} can be set to @code{#t} to output a string instead,
formatted with comments. This should be used when writing directly to a
file.

@var{type} specifies the type of code to output, and can be set to either
@code{'client} or @code{'server}.

@var{module-prefix} specifies a list of symbols representing a prefix to use
when defining the module.  If given, a @code{define-module} statement will be
placed at the top of the file using the prefix. For example, a prefix of
@lisp{'(myapp protocols)} when used with the @code{xdg-shell} protocol will
result in a statement like: @lisp{(define-module (myapp protocols xdg-shell))}.
If omitted, no module will be defined.

@var{extra-deps} gives a list of additional Guile modules to use at the top
of the file. This is useful if your protocol depends on other custom protocols.
If a module was defined with @var{module-prefix}, these will be included as
@code{#:use-module} keywords. Otherwise, they will appear at the top of the
file in a call to @code{use-module}."
  (unless (member type '(client server))
    (error "type must be one of [client|server]"))
  (when (eq? output-port #t)
    (set! output-port (current-output-port)))
  (unless input-port
    (set! input-port (current-input-port)))

  (define f (if output-text?
                (make <scheme-text-formatter> output-port)
                (make <scheme-formatter>)))
  (define protocol (with-input-from-port input-port read-protocol))

  (for-each
    (λ (interface)
      (add-export f (format-interface-name f (slot-ref interface 'name)))
      (add-export f (format-class-name f (slot-ref interface 'name)))
      (for-each
        (λ (enum)
          (for-each
            (λ (entry)
              (add-export f (format-enum-entry f interface enum entry)))
            (slot-ref enum 'entries)))
        (slot-ref interface 'enums))
      (for-each
        (λ (request)
           (unless (equal? (slot-ref request 'type) "destructor")
             (add-generic f (format-symbol f (slot-ref request 'name)))))
        (slot-ref interface 'requests))
      (unless (null? (slot-ref interface 'events))
        (add-generic f 'add-listener))
      (unless (equal? (slot-ref interface 'name) "wl_display")
        (add-generic f 'destroy)))
    (slot-ref protocol 'interfaces))

  (format-protocol-header f type protocol module-prefix extra-deps)
  (format-newline f)

  (for-each
    (λ (interface)
       (format-interface-header f interface)
       (for-each
         (λ (enum)
           (format-enum f interface enum))
         (slot-ref interface 'enums))
       (fold
         (λ (request index)
            (unless (equal? (slot-ref request 'type) "destructor")
              (format-stub f interface request index)
              (format-newline f))
            (1+ index))
         0 (slot-ref interface 'requests))

       (unless (null? (slot-ref interface 'events))
         (format-add-listener f interface)
         (format-newline f))

       (unless (equal? (slot-ref interface 'name) "wl_display")
         (format-destructor f interface)
         (format-newline f))

       (format-newline f))
    (slot-ref protocol 'interfaces))

  (unless output-port (flush f)))

(define* (wl-scanner-load filename #:key (type 'client) (extra-deps '()))
  "Loads a protocol from @var{filename} and evaluates all its definitions in
the current module. @var{type} and @code{extra-deps} are handled the same as
in @code{wl-scanner}."
  (eval
    `(begin ,@(with-input-from-file filename
                (wl-scanner #:type type #:extra-deps extra-deps)))
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
         (catch #t
           (λ ()
              (wl-scanner
                #:type          (string->symbol type)
                #:output-port   outport
                #:input-port    inport
                #:output-text?  #t
                #:extra-deps    (read-string-vals extra-deps)
                #:module-prefix (and=> module-prefix
                                       (λ (p) (map string->symbol
                                                   (string-split p #\space))))))
           (λ (key . args)
              (when (file-port? outport)
                (catch #t (λ () (delete-file (port-filename outport))) noop))
              (apply throw key args)))))
      (_
        (format (current-error-port) "Usage: ~a ~a" (car args) usage-text)
        (quit 1)))))
