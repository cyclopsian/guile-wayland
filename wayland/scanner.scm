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
  #:use-module (srfi srfi-11)
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

(define-method (format-class-name (f <scheme-formatter>) name type)
  (let ((n (match type ('client name)
                       ('server (string-append (format #f "~a" name)
                                               "-resource")))))
    (format-symbol-append f "<" n ">")))

(define-method (format-enum-entry (f <scheme-formatter>) interface enum entry)
  ((compose string->symbol string-upcase string-join)
   (map (cut slot-ref <> 'name) (list interface enum entry)) "_"))

(define-method (flush (f <scheme-formatter>))
  (let ((exprs (slot-ref f 'expressions)))
    (slot-set! f 'expressions '())
    (reverse exprs)))

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
                 (f <scheme-formatter>)
                 type protocol module-prefix extra-deps export?)
  (when (slot-bound? protocol 'copyright)
    (format-text f (slot-ref protocol 'copyright) ";;; ")
    (format-newline f)
    (format-newline f))

  (let* ((proto-name (slot-ref protocol 'name))
         (core-proto? (equal? proto-name "wayland"))
         (mod-name (format-symbol f (if core-proto? "protocol" proto-name)))
         (module-deps `((oop goops)
                        (wayland ,type core)
                        ,@(if core-proto? '() `((wayland ,type protocol)))
                        ,@extra-deps)))
    (if module-prefix
      (format-pretty f `(define-module (,@module-prefix ,mod-name)
                          ,@(fold (λ (m acc) (append acc `(#:use-module ,m)))
                                  '() module-deps)
                          #:duplicates (merge-generics replace)))
      (format-pretty f `(use-modules ,@module-deps))))

  (format-newline f)

  (when export?
    (for-each
      (λ (generic)
        (format-pretty f `(define-generic ,generic)))
      (slot-ref f 'generics))
    (format-newline f))

  (for-each
    (λ (interface)
      (let ((int-name (format-interface-name f (slot-ref interface 'name))))
        (format-pretty f `(define ,int-name
                            ((@@ (wayland util) make-wl-interface))))))
    (slot-ref protocol 'interfaces))
  (format-newline f))

(define-method
    (format-protocol-footer (f <scheme-formatter>) protocol type export?)
  (let*-values (((guile) (resolve-interface '(guile)))
                ((replaces generics) (partition
                                       (λ (sym) (module-bound? guile sym))
                                       (reverse (slot-ref f 'generics))))
                ((module-exports) (append (reverse (slot-ref f 'exports))
                                          generics)))
    (when export?
      (unless (null? module-exports)
        (format-pretty f `(export ,@module-exports)))
      (unless (null? replaces)
        (format-pretty f `(export! ,@replaces)))
      (when (equal? type 'server)
        (format-pretty f '(re-export initialize))))))

(define-method (format-interface-header (f <scheme-formatter>) interface type)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (int-name (format-interface-name f (slot-ref interface 'name)))
         (class-name (format-class-name f name type)))
    (format-pretty f
      `((@@ (wayland util) wl-interface-set)
         ,int-name
         ,(slot-ref interface 'name)
         ,(slot-ref interface 'version)
         ,(cons 'list (format-messages
                        f (slot-ref interface 'requests)))
         ,(cons 'list (format-messages
                        f (slot-ref interface 'events)))))
    (format-newline f)
    (match type
      ('client
       (format-pretty f `(define-class ,class-name (<wl-proxy>)
                                       #:metaclass <wl-proxy-class>)))
      ('server
       (format-pretty f `(define-class ,class-name (<wl-resource>)
                                       #:metaclass <wl-resource-class>))))
    (format-pretty f `(slot-set! ,class-name 'interface ,int-name))
    (format-newline f)
    (when (equal? type 'server)
      (format-pretty f
        `(define-method
           (initialize (,name ,class-name) args)
           (apply (lambda* (client version #:optional (id 0))
                    (wl-resource-create ,name client ,int-name version id))
                  args))))
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

(define-method (format-args (f <scheme-formatter>) event mode)
  (let ((type (if (is-a? event <wl-request>) 'server 'client)))
    (fold-right
      (λ (arg acc)
        (let ((append-sym
                (λ () (cons (format-symbol f (slot-ref arg 'name)) acc)))
              (append-make
                (λ () (let ((interface (slot-ref arg 'interface)))
                           (if interface
                             (let* ((name
                                      (format-symbol f (slot-ref arg 'name)))
                                    (wrap
                                     `(,(match type ('client 'wl-proxy-cast)
                                               ('server 'wl-resource-cast))
                                        ,name
                                        ,(format-class-name
                                           f (slot-ref arg 'interface) type))))
                               (cons
                                 (if (equal? (slot-ref arg 'allow-null) "true")
                                   `(if ,name ,wrap #f) wrap)
                                 acc))
                             (cons (format-symbol f (slot-ref arg 'name))
                                   acc))))))
          (match (slot-ref arg 'type)
            ("new_id"
             (if (slot-ref arg 'interface)
                 (match mode
                   ('call (if (equal? type 'client) (append-sym) (cons #f acc)))
                   ('args (if (equal? type 'client) (append-sym) acc))
                   ('wrap (append-make))
                   ('wrap-args (append-sym)))
                 (append (list
                           (if (equal? mode 'call)
                               '((@ (wayland util) wl-interface-name)
                                 interface)
                               'interface)
                           'version) (if (equal? mode 'call) '(#f) '()) acc)))
            ("object"
             (match mode ('wrap (append-make))
                         (else (append-sym))))
            (else (append-sym)))))
      '() (slot-ref event 'args))))

(define-method (format-stub (f <scheme-formatter>) interface event index)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (int-name (format-interface-name f (slot-ref interface 'name)))
         (type (if (is-a? event <wl-request>) 'client 'server))
         (event-name (format-symbol-append f (match type
                                               ('client "")
                                               ('server "send-"))
                                           (slot-ref event 'name)))
         (class-name (format-class-name f name type))
         (ret (find (λ (p) (equal? (slot-ref p 'type) "new_id"))
                    (slot-ref event 'args)))
         (ret-int (and ret (slot-ref ret 'interface)))
         (send-func
           (if (is-a? event <wl-request>)
               (if ret
                   (if ret-int
                       'wl-proxy-marshal-constructor
                       'wl-proxy-marshal-constructor-versioned)
                   'wl-proxy-marshal)
               'wl-resource-post-event))
         (ret-args
           (if ret
               (if ret-int
                   (list (format-interface-name f ret-int))
                   '(interface version))
               '()))
         (func-call `(,send-func ,name ,int-name ,index ,@ret-args
                                 ,@(format-args f event 'call))))
    (format-pretty f
      `(define-method
         (,event-name (,name ,class-name) ,@(format-args f event 'args))
         ,(if (and ret ret-int (equal? type 'client))
              `(wl-proxy-cast ,func-call ,(format-class-name f ret-int type))
              func-call)))))

(define-method (format-dispatcher (f <scheme-formatter>) interface type)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (int-name (format-interface-name f (slot-ref interface 'name)))
         (class-name (format-class-name f name type))
         (events (slot-ref interface (match type ('client 'events)
                                                 ('server 'requests))))
         (event-names (map (λ (e) (format-symbol f (slot-ref e 'name))) events))
         (wrapped-types (match type ('client '("new_id" "object"))
                                    ('server '("object"))))
         (event-calls (map (λ (e)
                             (if (any (λ (a) (member (slot-ref a 'type)
                                                     wrapped-types))
                                      (slot-ref e 'args))
                                 (let ((sym (format-symbol
                                              f (slot-ref e 'name))))
                                   `(if ,sym (lambda
                                               ,(format-args f e 'wrap-args)
                                               (,sym ,@(format-args f e 'wrap)))
                                        #f))
                                 (format-symbol f (slot-ref e 'name))))
                           events)))
    (when (equal? type 'server)
      (append! event-names '(destructor))
      (append! event-calls '(destructor)))

    (format-pretty f
      `(define-method
         (,(match type ('client 'add-listener) ('server 'set-implementation))
           (,name ,class-name) . args)
         (apply
           (lambda* (#:key ,@event-names)
             (,(match type ('client 'wl-proxy-add-listener)
                           ('server 'wl-resource-set-implementation))
               ,name ,int-name ,@event-calls))
           args)))))

(define-method (format-destructor (f <scheme-formatter>) interface type)
  (let* ((name (format-symbol f (slot-ref interface 'name)))
         (int-name (format-interface-name f (slot-ref interface 'name)))
         (class-name (format-class-name f name type))
         (destructor (list-index
                       (λ (req) (equal? (slot-ref req 'type) "destructor"))
                       (slot-ref interface 'requests))))
    (format-pretty f
      `(define-method
         (destroy (,name ,class-name))
         ,@(if destructor `((wl-proxy-marshal ,name ,int-name ,destructor)) '())
         (wl-proxy-destroy ,name)))))

(define* (wl-scanner #:key (type 'client)
                           output-port
                           input-port
                           (extra-deps '())
                           module-prefix
                           output-text?
                           (export? #t))
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
file in a call to @code{use-module}.

@var{export?} can be set to @code{#f} to disable symbol exporting, for use
when including the file directly in another module."
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
      (add-export f (format-class-name f (slot-ref interface 'name) type))
      (for-each
        (λ (enum)
          (for-each
            (λ (entry)
              (add-export f (format-enum-entry f interface enum entry)))
            (slot-ref enum 'entries)))
        (slot-ref interface 'enums))
      (match type
        ('client
         (for-each
           (λ (request)
             (unless (equal? (slot-ref request 'type) "destructor")
               (add-generic f (format-symbol f (slot-ref request 'name)))))
           (slot-ref interface 'requests)))
        ('server
         (for-each
           (λ (event)
             (add-generic f (format-symbol-append f "send-"
                                                  (slot-ref event 'name))))
           (slot-ref interface 'events))))
      (match type
        ('client (unless (null? (slot-ref interface 'events))
                   (add-generic f 'add-listener)))
        ('server (unless (null? (slot-ref interface 'requests))
                   (add-generic f 'set-implementation))))
      (when (and (equal? type 'client)
                 (not (equal? (slot-ref interface 'name) "wl_display")))
        (add-generic f 'destroy)))
    (slot-ref protocol 'interfaces))

  (format-protocol-header f type protocol module-prefix extra-deps export?)
  (format-newline f)

  (for-each
    (λ (interface)
       (format-interface-header f interface type)
       (for-each
         (λ (enum)
           (format-enum f interface enum))
         (slot-ref interface 'enums))

       (match type
         ('client
          (fold
            (λ (request index)
              (unless (equal? (slot-ref request 'type) "destructor")
                (format-stub f interface request index)
                (format-newline f))
              (1+ index))
            0 (slot-ref interface 'requests)))
         ('server
          (fold
            (λ (event index)
              (format-stub f interface event index)
              (format-newline f)
              (1+ index))
            0 (slot-ref interface 'events))))

       (unless (null? (slot-ref interface (match type ('client 'events)
                                                      ('server 'requests))))
         (format-dispatcher f interface type)
         (format-newline f))

       (when (and (equal? type 'client)
                  (not (equal? (slot-ref interface 'name) "wl_display")))
         (format-destructor f interface type)
         (format-newline f))

       (format-newline f))
    (slot-ref protocol 'interfaces))

  (format-protocol-footer f protocol type export?)

  (unless output-port (flush f)))

(define* (wl-scanner-load filename #:key (type 'client)
                                         (extra-deps '())
                                         (export? #f))
  "Loads a protocol from @var{filename} and evaluates all its definitions in
the current module. @var{type}, @var{extra-deps} and @var{export?} are handled
the same as in @code{wl-scanner}."
  (primitive-eval
    `(begin ,@(with-input-from-file filename
                (λ () (wl-scanner #:type type
                                  #:extra-deps extra-deps
                                  #:export? export?))))))

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
                #:export?       #t
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
