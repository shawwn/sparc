#lang racket/base

(require racket/provide)
(provide (matching-identifiers-out #rx"^ar-" (all-defined-out)))

(require racket/undefined)
(require (only-in racket/tcp tcp-listener?))
(require (only-in racket/async-channel async-channel?))
(require (only-in racket/sequence sequence->list sequence-ref sequence-length))
(require (only-in racket/vector vector-append))
(require (only-in racket/list make-list))

(define ar-unset undefined)

(define (ar-unset? x)
  (ar-id x ar-unset))

(define ar-nil '())
(define ar-t #t)
(define ar-nan +nan.0)
(define ar-inf +inf.0)
(define ar-ninf -inf.0)

(define ar-err error)

(define (ar-list? x)
  (or (null? x) (pair? x)))

(define (ar-seq? x)
  (and (sequence? x)
       (not (number? x))
       (not (null? x))
       (not (hash? x))
       (not (port? x))))

(define (ar-nil? x)
  (or (null? x)
      (ar-unset? x)
      (void? x)))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (not x) (ar-nil? x)))

(define (ar-true? x)
  (not (ar-false? x)))

(define (ar-join (x ar-nil) (y ar-nil))
  (if (ar-unset? x) y (cons x y)))

(define (ar-car x)
  (cond ((pair? x)     (car x))
        ((null? x)     x)
        (#t            (ar-err "Can't take car of" x))))

(define (ar-cdr x)
  (cond ((pair? x)     (cdr x))
        ((null? x)     x)
        (#t            (ar-err "Can't take cdr of" x))))

(define (ar-car? l (k ar-unset) #:test (test ar-id))
  (and (pair? l)
       (if (ar-unset? k) (car l)
         (if (procedure? k) (k (car l))
           (test (car l) k)))))

(define (ar-caar? l (k ar-unset) #:test (test ar-id))
  (ar-car? (ar-car? l) k #:test test))

(define (ar-caar x) (ar-car (ar-car x)))
(define (ar-cadr x) (ar-car (ar-cdr x)))
(define (ar-cddr x) (ar-cdr (ar-cdr x)))
(define (ar-cadar x) (ar-car (ar-cdr (ar-car x))))
(define (ar-caddr x) (ar-car (ar-cdr (ar-cdr x))))
(define (ar-cadddr x) (ar-car (ar-cdr (ar-cdr (ar-cdr x)))))

; waterhouse's code to modify Racket's immutable pairs.
; http://arclanguage.org/item?id=13616
(require (only-in racket/unsafe/ops unsafe-set-immutable-car! unsafe-set-immutable-cdr!))

(define (ar-xar p x)
  (if (pair? p)
      (unsafe-set-immutable-car! p x)
      (raise-type-error 'xar "pair" p))
  x)

(define (ar-xdr p x)
  (if (pair? p)
      (unsafe-set-immutable-cdr! p x)
      (raise-type-error 'xdr "pair" p))
  x)

(define (nth-set! lst n val)
  (ar-xar (list-tail lst n) val))

(define (ar-sref com val ind)
  (cond ((hash? com)  (if (ar-nil? val)
                          (hash-remove! com ind)
                          (hash-set! com ind val)))
        ((string? com) (string-set! com ind val))
        ((bytes? com)  (bytes-set! com ind val))
        ((pair? com)   (nth-set! com ind val))
        ((namespace? com) (ar-namespace-set! ind val com))
        (#t (ar-err "Can't set reference " com ind val)))
  val)

(define (ar-each-kv xs (f list))
  (cond ((hash? xs) (hash-for-each xs f))
        ((null? xs) xs)
        (#t (f (car xs) (cadr xs))
            (ar-each-kv (cddr xs) f))))

(define (ar-join! l x . args)
  (cond ((hash? l)
         (ar-each-kv x (lambda (k v) (ar-sref l v k)))
         (if (null? args) l (apply ar-join! l args)))
        (#t (ar-err "Can't join" l))))

(define (ar-tagged type . rep)
  `(lit ,type ,@rep))

(define (ar-tagged? x)
  (ar-car? x 'lit))

(define (ar-tagged-type x)
  (cadr x))

(define (ar-tagged-rep x)
  (caddr x))

(define (ar-rep x)
  (if (ar-tagged? x)
      (ar-tagged-rep x)
      x))

; (type nil) -> nil

(define (exint? x) (and (integer? x) (exact? x)))

(provide exint?)

(define (ar-type x)
  (cond ((ar-tagged? x)     (ar-tagged-type x))
        ((null? x)          ar-nil)
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((parameter? x)     'param)
        ((procedure? x)     'fn)
        ((char? x)          'char)
        ((string? x)        'string)
        ((bytes? x)         'bytes)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((hash? x)          'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((channel? x)       'channel)
        ((async-channel? x) 'channel)
        ((boolean? x)       'bool)
        ((eof-object? x)    'eof)
        ((void? x)          'void)
        ((ar-unset? x)      'unset)
        (#t                 (ar-typeof x))))

(define (ar-typeof x)
  (let ((tag (symbol->string (vector-ref (struct->vector x) 0))))
    (string->symbol (substring tag (string-length "struct:")))))

(define (ar-typename type (val ar-unset))
  (case type
    ((tab) 'table)
    ((str) 'string)
    ((key) 'keyword)
    (else type)))

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (keyword->symbol x) (string->symbol (keyword->string x)))
(define (symbol->keyword x) (string->keyword (symbol->string x)))

(provide keyword->symbol)
(provide symbol->keyword)

(define (iround x) (inexact->exact (round x)))

(define (ar-encoding (x ar-nil))
  (if (ar-nil? x)
      (ar-encoding 'latin1)
      (case x
        ((utf8   utf-8)   'utf8)
        ((latin1 latin-1) 'latin1)
        (else (ar-err "Expected latin1 or utf8, got" x)))))

(define (ar-coerce x kind . args)
  (define type (ar-typename kind x))
  (define (retry x (type type))
    (apply ar-coerce x type args))
  (define (fail)
    (apply ar-err "Can't coerce" x type args))
  (cond
    ((ar-tagged? x) (ar-err "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->ascii x))
                      ((string)  (string x))
                      ((bytes)   (retry (string x)))
                      ((sym)     (string->symbol (string x)))
                      ((keyword) (string->keyword (string x)))
                      ((bool)    #t)
                      (else      (fail))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (ascii->char x))
                      ((string)  (apply number->string x args))
                      ((bytes)   (retry (number->string x)))
                      ((bool)    #t)
                      ((cons)    (sequence->list x))
                      (else      (fail))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (ascii->char (iround x)))
                      ((string)  (apply number->string x args))
                      ((bytes)   (retry (number->string x)))
                      ((bool)    #t)
                      (else  (fail))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (string->list x))
                      ((keyword) (string->keyword x))
                      ((bytes)   (retry x (ar-encoding (ar-car args))))
                      ((latin1)  (string->bytes/latin-1 x))
                      ((utf8)    (string->bytes/utf-8 x))
                      ((num)     (or (apply string->number x args) (fail)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n (iround n) (fail))))
                      ((bool)    #t)
                      ((char)    (if (= (string-length x) 1)
                                     (string-ref x 0)
                                     (fail)))
                      (else      (fail))))
    ((bytes? x)     (case type
                      ((cons)    (bytes->list x))
                      ((string)  (retry x (ar-encoding (ar-car args))))
                      ((latin1)  (bytes->string/latin-1 x))
                      ((utf8)    (bytes->string/utf-8 x))
                      ((bool)    #t)
                      (else      (fail))))
    ((pair? x)      (case type
                      ((bytes)   (list->bytes x))
                      ((string)  (if (ar-car? x byte?)
                                     (retry (list->bytes x))
                                     (apply ar-cat x)))
                      ((sym)     (string->symbol (apply ar-cat x)))
                      ((keyword) (string->keyword (apply ar-cat x)))
                      ((bool)    #t)
                      ((bytes)   (if (ar-car? x byte?)
                                     (list->bytes x)
                                     (fail)))
                      (else      (fail))))
    ((ar-nil? x)    (case type
                      ((bool)    #f)
                      ((string)  "")
                      ((bytes)   #"")
                      ((sym)     (string->symbol ""))
                      ((keyword) (string->keyword ""))
                      (else      (fail))))
    ((keyword? x)    (case type
                      ((sym)     (keyword->symbol x))
                      ((string)  (keyword->string x))
                      ((cons)    (string->list (keyword->string x)))
                      ((bytes)   (retry (keyword->string x)))
                      ((bool)    #t)
                      (else      (fail))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      ((keyword) (symbol->keyword x))
                      ((cons)    (string->list (symbol->string x)))
                      ((bytes)   (retry (symbol->string x)))
                      ((bool)    #t)
                      (else      (fail))))
    ((boolean? x)   (case type
                      ((string)  (if x  "t"  "false"))
                      ((bytes)   (if x #"t" #"false"))
                      (else      (fail))))
    ((sequence? x)  (retry (sequence->list x)))
    (#t             (fail))))

(define (ar-to type . args)
  (lambda (x) (apply ar-coerce x type args)))

(define (ar-cat . args)
  (if (ar-car? args bytes?)
      (apply bytes-append (map (ar-to 'bytes 'utf8) args))
      (apply string-append (map (ar-to 'string) args))))

(define scm-reserved '(
  do lambda let let* and or if cond else when unless set!
  while for loop case
  define define-syntax define-values
  begin begin-for-syntax
  + - / *
  < <= = == >= > 
  true false t nil
  car cdr caar cadr cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
  lib require provide module load eof read write eval
  length empty last keep set max min abs round count
  eq eqv equal eq? eqv? equal?
  cons list member assoc compose all map string thread
  tag link only any nor private public
  sort close error with-handlers
  date tokens
  place place* place/context place-kill
  compile partition
  apply
))

(define (ar-name s)
  (if (and (symbol? s) (memq s scm-reserved))
      (string->symbol (string-append "arc--" (symbol->string s)))
      s))

(define (ar-bound? name (fail #f) (ns (current-namespace)))
  (define unbound (ar-join))
  (define it (ar-namespace-ref (ar-name name) (lambda () unbound) ns))
  (if (eq? it unbound) fail it))

; circular references will go into an infinite loop
(define (ar-symbol-value name (fail ar-unset))
  (let ((v (ar-bound? name fail)))
    (if (ar-unset? v) (ar-err "Unbound variable" name)
      (if (symbol? v) (ar-symbol-value v fail) v))))

(define (ar-symbol-function name)
  (let ((v (if (symbol? name) (ar-symbol-value name) name)))
    (if (procedure? v) v (ar-err "Not a procedure" name))))

; call a function or perform an array ref, hash ref, &c

; Non-fn constants in functional position are valuable real estate, so
; should figure out the best way to exploit it.  What could (1 foo) or
; ('a foo) mean?  Maybe it should mean currying.

; For now the way to make the default val of a hash table be other than
; nil is to supply the val when doing the lookup.  Later may also let
; defaults be supplied as an arg to table.  To implement this, need: an
; eq table within scheme mapping tables to defaults, and to adapt the
; code in arc.arc that reads and writes tables to read and write their
; default vals with them.  To make compatible with existing written tables,
; just use an atom or 3-elt list to keep the default.

(define (ar-apply fn args)
  (cond ((procedure? fn)
         (apply fn args))
        ((symbol? fn)
         (ar-apply (ar-symbol-value fn) args))
        ((pair? fn)
         (if (null? args)
             (map ar-car fn)
             (list-ref fn (car args))))
        ((hash? fn)
         (if (null? args)
             (hash-keys fn)
             (hash-ref fn
                       (car args)
                       (if (pair? (cdr args)) (cadr args) ar-nil))))
        ((namespace? fn)
         (if (null? args)
             (ar-namespace-keys fn)
             (ar-namespace-ref (car args)
                               (if (pair? (cdr args)) (ar-thunk (cadr args)) #f)
                               fn)))
        ((thread-cell? fn)
         (case (length args)
           ((0) (thread-cell-ref fn))
           ((1) (thread-cell-set! fn (car args)))
           (else (apply raise-arity-error 'apply-thread-cell-set! 1 args))))
        ((box? fn)
         (case (length args)
           ((0) (unbox fn))
           ((1) (set-box! fn (car args)))
           ((2) (box-cas! fn (cadr args) (car args)))
           (else (apply raise-arity-error 'apply-box-cas! 2 args))))
        ((ar-seq? fn)
         (sequence-ref fn (car args)))
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets
; passed to the first arg, i.e. ('kids item) means (item 'kids).
        (#t (ar-err "Function call on inappropriate object" fn args))))

(define (ar-thunk val)
  (if (procedure? val) val (lambda () val)))

; special cases of ar-apply for speed and to avoid consing arg lists

(define (ar-funcall0 fn)
  (if (procedure? fn)
      (fn)
      (ar-apply fn (list))))

(define (ar-funcall1 fn arg1)
  (if (procedure? fn)
      (fn arg1)
      (ar-apply fn (list arg1))))

(define (ar-funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (ar-apply fn (list arg1 arg2))))

(define (ar-funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (ar-apply fn (list arg1 arg2 arg3))))

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (ar-apply fn (list arg1 arg2 arg3 arg4))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4))
; and we should return '(1 2 3 4)

(define (ar-apply-args args)
  (cond ((null? args) args)
        ((null? (cdr args)) (car args))
        (#t (cons (car args) (ar-apply-args (cdr args))))))

(define (hash->plist h)
  (let ((al (hash->list h #t)))
    (apply append (map list (map car al) (map cdr al)))))

(provide hash->plist)

(define (ar-keyword? x)
  (or (and (keyword? x) x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (> (string-length s) 1)
                  (eq? (string-ref s (- (string-length s) 1)) #\:)
                  (not (eq? (string-ref s (- (string-length s) 2)) #\:))
                  (symbol->keyword (string->symbol (substring s 0 (- (string-length s) 1)))))))))

(define (ar-unstash args (kwargs #f) (xs '()) (kws (make-hasheq)))
  (cond ((ar-list? kwargs)
         (list args (cadr (ar-unstash kwargs #f xs kws))))
        ((hash? kwargs)
         (ar-unstash args (hash->plist kwargs) xs kws))
        ((null? args)
         (list (reverse xs) kws))
        ((ar-keyword? (car args))
         (ar-unstash (cddr args) kwargs xs (ar-join! kws (list (ar-keyword? (car args)) (cadr args)))))
        (#t (ar-unstash (cdr args) kwargs (cons (car args) xs) kws))))

(define (ar-kwapply-1 f kwargs . args)
  (let* ((it (ar-unstash (ar-apply-args args) kwargs))
         (xs (car it))
         (kvs (cadr it)))
    (if (hash-empty? kvs)
        (ar-apply f xs)
        (let ((al (hash->list kvs #t)))
          (keyword-apply (ar-symbol-function f) (map car al) (map cdr al) xs)))))

(define (ar-kwappend . args)
  (hash->plist (apply ar-join! (make-hasheq) args)))

(define ar-kwapply
  (make-keyword-procedure
    (lambda (keys vals fn kws . args)
      (let ((kws1 (apply ar-kwappend kws (map list keys vals))))
        (apply ar-kwapply fn kws1 args)))
    ar-kwapply-1))

; (ar-pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; reduce?

(define (ar-pairwise-1 pred lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (ar-true? (pred (car lst) (cadr lst)))
           (ar-pairwise-1 pred (cdr lst)))))

(define (ar-pairwise pred lst)
  (if (and (pair? lst)
           (null? (cdr lst)))
      (let ((v (car lst)))
        (procedure-rename 
          (lambda (x) (pred x v))
          (object-name pred)))
      (ar-pairwise-1 pred lst)))

; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(define (char-or-string? x)
  (or (char? x) (string? x) (bytes? x)))

(define (ar-+ (x 0) . args)
  (cond ((null? args) x)
        ((char-or-string? x)
         (apply ar-cat x args))
        ((ar-list? x)
         (apply append x args))
        ((evt? x)
         (apply choice-evt x args))
        ((path? x)
         (apply build-path x args))
        ((symbol? x)
         (string->symbol (apply ar-cat x args)))
        ((keyword? x)
         (string->keyword (apply ar-cat x args)))
        ((vector? x)
         (apply vector-append x args))
        (#t (apply + x args))))

(define (ar-+2 x y)
  (cond ((char-or-string? x)
         (ar-cat x y))
        ((ar-list? x)
         (append x y))
        ((evt? x)
         (choice-evt x y))
        ((path? x)
         (build-path x y))
        ((symbol? x)
         (string->symbol (ar-cat x y)))
        ((keyword? x)
         (string->keyword (ar-cat x y)))
        ((vector? x)
         (vector-append x y))
        (#t (+ x y))))

(define (ar-* . args)
  (define (seq? x) (or (ar-seq? x) (null? x)))
  (define (repeat seq n)
    (define v (make-list n seq))
    (if (null? v)
        (if (pair? seq) v (ar-coerce v (ar-type seq)))
        (apply ar-+ v)))
  (cond ((and (= (length args) 2) (seq? (car args)))
         (repeat (car args) (cadr args)))
        ((and (= (length args) 2) (seq? (cadr args)))
         (repeat (cadr args) (car args)))
        (#t (apply * args))))

; generic comparison

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

(define (ar-id a b)
  (or (eqv? a b)
      (and (number? a) (number? b) (= a b))
      (and (string? a) (string? b) (string=? a b))
      (and (bytes? a) (bytes? b) (bytes=? a b))))

(define (ar-is a b)
  (or (ar-id a b)
      (equal?/recur a b ar-is)))

(define (ar->2 x y)
  (ar-<2 y x))

(define (ar-<2 x y)
  (cond ((and (number? x) (number? y)) (< x y))
        ((and (string? x) (string? y)) (string<? x y))
        ((and (symbol? x) (symbol? y)) (string<? (symbol->string x) (symbol->string y)))
        ((and (keyword? x) (keyword? y)) (string<? (keyword->string x) (keyword->string y)))
        ((and (bytes? x) (bytes? y)) (bytes<? x y))
        ((and (char? x) (char? y)) (char<? x y))
        (#t (< x y))))

(define (ar-len x)
  (cond ((ar-list? x) (length x))
        ((ar-seq? x) (sequence-length x))
        ((hash? x) (hash-count x))
        ((namespace? x) (ar-namespace-count x))
        ((symbol? x) (string-length (symbol->string x)))
        ((keyword? x) (string-length (keyword->string x)))
        (#t (ar-err "Can't get len of" x))))

(define (ar-protect during after)
  (dynamic-wind (lambda () #t) during after))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(define (ar-atomic-invoke f)
  (if (thread-cell-ref ar-sema-cell)
      (ar-apply f '())
      (dynamic-wind
        (lambda ()
          (thread-cell-set! ar-sema-cell #t))
        (lambda ()
          (call-with-semaphore
            ar-the-sema
            (lambda () (ar-apply f '()))))
        (lambda ()
          (thread-cell-set! ar-sema-cell #f)))))

; namespace accessors

(define (ar-namespace-ref name (fail #f) (ns (current-namespace)))
  (namespace-variable-value name #t fail ns))

(define (ar-namespace-set! name value (ns (current-namespace)))
  (namespace-set-variable-value! name value #t ns))

(define (ar-namespace-unset! name (ns (current-namespace)))
  (namespace-undefine-variable! name ns))

(define (ar-namespace-keys (ns (current-namespace)))
  (namespace-mapped-symbols ns))

(define (ar-namespace-count (ns (current-namespace)))
  (length (ar-namespace-keys ns)))

