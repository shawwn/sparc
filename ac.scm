; Arc Compiler.
#lang racket/load

(require json)
(require syntax/stx)
(require racket/port)
(require racket/pretty)
(require racket/runtime-path)
(require racket/system)
(require racket/tcp)
(require racket/unsafe/ops)
(require racket/path)
(require racket/trace)
(require racket/async-channel)
(require racket/struct)
(require syntax/srcloc)
(require racket/undefined)
(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/vector)
(require ffi/cvector)
(require ffi/unsafe/cvector)

; configure reader
; (read-square-bracket-with-tag #t)
; (read-curly-brace-with-tag #t)
(print-hash-table #t)
(print-syntax-width 10000)

(define (car? l (k undefined) #:test (test ar-id))
  (and (pair? l)
       (if (eq? k undefined) (car l)
         (if (procedure? k) (k (car l))
           (test (car l) k)))))

(define (caar? l (k undefined) #:test (test ar-id))
  (car? (car? l) k #:test test))

(define (ar-tagged type . rep)
  `(lit ,type ,@rep))

(define (ar-tagged? x)
  (car? x 'lit))

(define (ar-tagged-type x)
  (cadr x))

(define (ar-tagged-rep x)
  (caddr x))

; sread = scheme read. eventually replace by writing read

(define (shebang? (port (current-input-port)))
  (equal? "#!" (peek-string 2 0 port)))

(define (sread (p (current-input-port)) (eof eof))
  (parameterize ((read-accept-lang #t)
                 (read-accept-reader #t))
    (port-count-lines! p)
    (if (shebang? p)
        (begin (read-line p) (sread p eof))
        (let ((expr (read-syntax (object-name p) p)))
          (if (eof-object? expr) eof expr)))))

(define (sdata (p (current-input-port)) (eof eof))
  (parameterize ((read-accept-lang #f)
                 (read-accept-reader #f))
    (if (shebang? p)
        (begin (read-line p) (sdata p eof))
        (let ((expr (read p)))
          (if (eof-object? expr) eof (ac-quoted expr))))))

(define (syn x (src #f))
  (if (syntax? x)
      x
      (datum->syntax #f x (if (syntax? src) src #f))))

(define (datum x)
  (if (syntax? x) (syntax->datum x) x))

(define env* (make-parameter (list) #f 'env*))

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (ac s)
  (set! s (ac-macex s))
  (cond ((syntax? s) (syn (ac (syntax->datum s)) s))
        ((literal? s) (ac-literal s))
        ((ssyntax? s) (ac (expand-ssyntax s)))
        ((symbol? s) (ac-var-ref s))
        ((car? s '%do) (ac-do (cdr s)))
        ((car? s 'lexenv) (ac-lexenv))
        ((car? s 'syntax) (cadr s))
        ((caar? s 'syntax) (map ac s))
        ((car? s ssyntax?) (ac (cons (expand-ssyntax (car s)) (cdr s))))
        ((car? s 'quote) (list 'quote (ac-quoted (cadr s))))
        ((car? s 'quasiquote) (ac-qq (cadr s)))
        ((car? s 'quasisyntax) (ac-qs (cadr s)))
        ((car? s 'if) (ac-if (cdr s)))
        ((car? s 'fn) (ac-fn (cadr s) (cddr s)))
        ((car? s 'assign) (ac-set (cdr s)))
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        ((caar? s 'compose) (ac (decompose (cdar s) (cdr s))))
        ((caar? s 'complement)
         (ac (list 'no (cons (cadar s) (cdr s)))))
        ((caar? s 'andf) (ac-andf s))
        ((pair? s) (ac-call (car s) (cdr s)))
        (#t s)))

(define ar-nil '())
(define ar-t #t)
(define unset undefined)
(define (unset? x) (eq? x unset))

(define (ar-nil? x)
  (eqv? x ar-nil))

(define atstrings #t)

(define (ac-string s)
  (if atstrings
      (if (atpos s 0)
          (ac (cons 'string (map (lambda (x)
                                   (if (string? x)
                                       (unescape-ats x)
                                       x))
                                 (codestring s))))
          (list 'string-copy (unescape-ats s)))
      (list 'string-copy s)))     ; avoid immutable strings

(define (keywordp x)
  (or (and (keyword? x) x)
      (and (symbol? x)
           (let ((s (symbol->string x)))
             (and (> (string-length s) 1)
                  (eq? (string-ref s (- (string-length s) 1)) #\:)
                  (not (eq? (string-ref s (- (string-length s) 2)) #\:))
                  (symbol->keyword (string->symbol (substring s 0 (- (string-length s) 1)))))))))

(define (ac-flag? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 1)
              (eq? (string-ref s 0) #\:)
              (not (eq? (string-ref s 1) #\:))
              (string->symbol (substring s 1))))))

(define (ac-unflag x)
  (let* ((n (ac-flag? x))
         (k (and n (symbol->keyword n)))
         (v (and n (if (ac-lex? n) n ar-t))))
    (if n `(,k ,v) `(,x))))

(define (ac-unflag-args args)
  (apply append (imap ac-unflag args)))

(define (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (bytes? x)
      (ar-false? x)
      (syntax? x)
      (keywordp x)
      (ar-tagged? x)
      (ac-number-literal x)))

(define (ac-literal x)
  (cond ((null? x) (list 'quote x))
        ((ar-tagged? x) (list 'quote x))
        ((string? x) (ac-string x))
        (#t (ac-quoted x))))

(define (ac-identifier x)
  (string->symbol (string-append " " (symbol->string x))))

(define (ac-identifier? x)
  (and (symbol? x) (string-prefix? (symbol->string x) " ")))

(define (ssyntax? x)
  (and (symbol? x)
       (not (eqv? x '_))
       (not (ac-identifier? x))
       (has-ssyntax-char? (symbol->string x))))

(define (has-ssyntax-char? string (i (- (string-length string) 2)))
  (and (>= i 0)
       (let ((c (string-ref string i)))
         (or (eqv? c #\:) (eqv? c #\~)
             (eqv? c #\&)
             ;(eqv? c #\_)
             (eqv? c #\.)  (eqv? c #\!)
             (has-ssyntax-char? string (- i 1))))))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not
; (complement (andf foo bar)).

(define (expand-ssyntax sym)
  ((cond ((insym? #\& sym) expand-and)
         ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
     ;   ((insym? #\_ sym) expand-curry)
         (#t (error "Unknown ssyntax" sym)))
   sym))

(define (expand-compose sym)
  (let ((elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'compose elts))))

(define (expand-and sym)
  (let ((elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'andf elts))))

; How to include quoted arguments?  Can't treat all as quoted, because
; never want to quote fn given as first.  Do we want to allow quote chars
; within symbols?  Could be ugly.

; If release, fix the fact that this simply uses v0... as vars.  Should
; make these vars gensyms.

(define (expand-curry sym)
  (let ((expr (exc (map (lambda (x)
                          (if (pair? x) (chars->value x) x))
                        (tokens (lambda (c) (eqv? c #\_))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
                    0)))
    (list 'fn
          (keep (lambda (s)
                  (and (symbol? s)
                       (eqv? (string-ref (symbol->string s) 0)
                             #\v)))
                expr)
          expr)))

(define (keep f xs)
  (cond ((null? xs) '())
        ((f (car xs)) (cons (car xs) (keep f (cdr xs))))
        (#t (keep f (cdr xs)))))

(define (exc elts n)
  (cond ((null? elts)
         '())
        ((eqv? (car elts) #\_)
         (cons (string->symbol (string-append "v" (number->string n)))
               (exc (cdr elts) (+ n 1))))
        (#t
         (cons (car elts) (exc (cdr elts) n)))))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (build-sexpr toks orig)
  (cond ((null? toks)
         '%get)
        ((null? (cdr toks))
         (chars->value (car toks)))
        (#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                   (list 'quote (chars->value (car toks)))
                   (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                       (err "Bad ssyntax" orig)
                       (chars->value (car toks))))))))

(define (insym? char sym) (member char (cdr (reverse (symbol->chars sym)))))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars)
  (let ((s (list->string chars)))
    (if (string-contains? s " ")
        (string->symbol s)
        (read-from-string s))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (if (pair? token)
                      (cons (reverse token) acc)
                      acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null? token)
                            acc
                            (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?))))

(define (ac-global-name s)
  (if (and (symbol? s) (memq s scm-reserved))
      (string->symbol (string-append "arc--" (symbol->string s)))
      s))

(define (ac-var-ref s)
  (cond ((ac-boxed? 'get s) (ac-boxed-get s))
        ((ac-lex? s)        s)
        (#t                 (ac-global-name s))))

(define (ac-tonumber s (base 10))
  (with-handlers ((exn:fail? (lambda (c) #f)))
    (ar-coerce s 'int base)))

(define (ac-parse-number s)
  (set! s (string-replace s "_" ""))
  (and (string? s)
       (cond ((string-prefix? s "-")
              (let ((n (ac-parse-number (substring s 1))))
                (and n (- n))))
             ((or (string-prefix? s "0x")
                  (string-prefix? s "0X"))
              (ac-tonumber (substring s 2) 16))
             ((or (string-prefix? s "0b")
                  (string-prefix? s "0B"))
              (ac-tonumber (substring s 2) 2))
             ((or (string-prefix? s "0o")
                  (string-prefix? s "0O"))
              (ac-tonumber (substring s 2) 8))
             (#t (ac-tonumber s)))))

(define (ac-number-literal s)
  (and (symbol? s) (ac-parse-number (symbol->string s))))

; quote

(define (ac-quoted x)
  (cond ((pair? x)
         (imap ac-quoted x))
        ((eqv? x 'nil)
         ar-nil)
        ((eqv? x 't)
         ar-t)
        ((eqv? x 'true)
         ar-t)
        ((eqv? x 'false)
         #f)
        (#t (or (keywordp x) (ac-number-literal x) x))))

(define (ac-unquoted x)
  (cond ((pair? x)
         (imap ac-unquoted x))
        ((ar-nil? x)
         'nil)
        ((eqv? x ar-t)
         't)
        ((eqv? x #f)
         'false)
        (#t x)))

; quasiquote

(define (ac-qq args)
  (list 'quasiquote (ac-qq1 1 args)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))

(define (ac-qq1 level x)
  (cond ((= level 0)
         (ac x))
        ((car? x 'unquote)
         (list 'unquote (ac-qq1 (- level 1) (cadr x))))
        ((and (car? x 'unquote-splicing) (= level 1))
         (list 'unquote-splicing
               (ac-qq1 (- level 1) (cadr x))))
        ((car? x 'quasiquote)
         (list 'quasiquote (ac-qq1 (+ level 1) (cadr x))))
        ((pair? x)
         (imap (lambda (x) (ac-qq1 level x)) x))
        (#t (ac-quoted x))))

; quasisyntax

(define (ac-qs args)
  (ac-qs1 1 args))

; process the argument of a quasisyntax. keep track of
; depth of nesting. handle unsyntax only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unsyntax (fn x))

(define (ac-qs1 level x)
  (cond ((= level 0)
         (ac x))
        ((car? x 'unsyntax)
         (ac-qs1 (- level 1) (cadr x)))
        ((and (car? x 'unsyntax-splicing) (= level 1))
         (ac-qs1 (- level 1) (cadr x)))
        ((car? x 'quasisyntax)
         (ac-qs1 (+ level 1) (cadr x)))
        ((pair? x)
         (imap (lambda (x) (ac-qs1 level x)) x))
        (#t x)))

; like map, but don't demand '()-terminated list

(define (imap f l)
  (cond ((pair? l)
         (cons (f (car l)) (imap f (cdr l))))
        ((null? l)
         '())
        (#t (f l))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args)
  (cond ((null? args) (list 'quote ar-nil))
        ((null? (cdr args)) (ac (car args)))
        (#t `(if (ar-true? ,(ac (car args)))
                 ,(ac (cadr args))
                 ,(ac-if (cddr args))))))

(define (ac-dbname! name (env (env*)))
  (env* (if (symbol? name)
            (cons (list name) env)
            env))
  (env*))

(define (ac-dbname (env (env*)))
  (reverse (map car (keep pair? env))))

(define (ac-env! x)
  (cond ((symbol? x)
         (env* (cons x (env*))))
        ((caar? x 'o)
         (ac-env! (cadr x)))
        ((pair? x)
         (imap ac-env! x)))
  x)

(define (ac-fn-args a)
  (cond ((null? a) '())
        ((symbol? a)
         (ac-env! a))
        ((caar? a 'o)
         (let* ((it (cdar a))
                (var (car it))
                (key (ac-flag? var))
                (var (or key var))
                (val (ar-xcar (cdr it)))
                (expr (parameterize ((env* (env*)))
                        (ac-dbname! var)
                        (ac val))))
           (ac-env! var)
           (if key
               `(,(symbol->keyword var)
                 (,var ,expr) ,@(ac-fn-args (cdr a)))
               `((,var ,expr) ,@(ac-fn-args (cdr a))))))
        ((car? a keywordp)
         (let* ((key (keywordp (car a)))
                (var (cadr a))
                (var (if (symbol? var) `(o ,var) var))) ; ensure all kwargs are optional
           (cons key (ac-fn-args `(,var ,@(cddr a))))))
        ((car? a ac-flag?)
         (let* ((var (ac-flag? (car a)))
                (key (symbol->keyword var)))
           (ac-fn-args `(,key (o ,var) ,@(cdr a)))))
        (#t
         (cons (ac-env! (car a)) (ac-fn-args (cdr a))))))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(define (ac-fn args body)
  (parameterize ((env* (env*)))
    (if (ac-complex-args? args)
        (ac-complex-fn args body)
        (ac-simple-fn args body))))

(define (ac-simple-fn args body)
  (let* ((a (ac-fn-args args))
         (f (ac-nameit `(lambda ,a ,@(ac-body* body)))))
    (if (ac-kwargs? a)
        `(ar-kwproc ,f)
        f)))

(define (ac-kwargs? args)
  (if (pair? args)
      (or (ac-kwargs? (car args))
          (ac-kwargs? (cdr args)))
      (eq? args '#:kwargs)))

(define (ar-kwproc f)
  (make-keyword-procedure
    (lambda (ks vs . args)
      (let ((kwargs (apply append (map list ks vs))))
        (apply f args #:kwargs kwargs)))
    f))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond ((null? args) #f)
        ((symbol? args) #f)
        ((or (car? args symbol?)
             (car? args keywordp)
             (and (caar? args 'o)
                  (car? (cdar args) symbol?)))
         (ac-complex-args? (cdr args)))
        (#t #t)))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(define (ac-complex-fn args body)
  (let* ((ra (ar-gensym))
         (z (ac-complex-args args ra #t)))
    (ac-nameit
      `(lambda ,ra
         (let* ,z
           ,@(ac-body* body))))))

; returns a list of two-element lists, first is variable name,
; second is (compiled) expression. to be used in a let.
; caller should extract variables and add to env.
; ra is the rest argument to the fn.
; is-params indicates that args are function arguments
;   (not destructuring), so they must be passed or be optional.

(define (ac-complex-args args ra is-params (is-kw #f))
  (cond ((null? args) '())
        ((symbol? args)
         (list (list (ac-env! args) ra)))
        ((car? args ac-flag?)
         (let ((var (ac-flag? (car args))))
           (ac-complex-args `(,(symbol->keyword var) (o ,var) ,@(cdr args)) ra is-params)))
        ((car? args keywordp)
         (ac-complex-args (cdr args) ra #f (keyword->symbol (keywordp (car args)))))
        ((and (caar? args 'o)
              (ac-flag? (cadar args)))
         (let ((var (ac-flag? (cadar args)))
               (val (cddar args)))
           (ac-complex-args
             `(,(symbol->keyword var) (o ,var ,@val) ,@(cdr args)) ra is-params #f)))
        ((pair? args)
         (let* ((x (if (caar? args 'o)
                       (let ((var (cadar args))
                             (val (if (pair? (cddar args))
                                           (caddar args)
                                           ar-nil)))
                         (ac-complex-opt var val ra is-kw))
                       (ac-complex-args
                        (car args)
                        (cond (is-kw     `(ar-funcall2 ,ra ',is-kw ar-nil))
                              (is-params `(car ,ra))
                              (#t        `(ar-xcar ,ra)))
                        #f))))
           (append x (ac-complex-args (cdr args)
                                      (if is-kw ra `(ar-xcdr ,ra))
                                      is-params))))
        (#t (err "Can't understand fn arg list" args))))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(define (ac-complex-opt var expr ra is-kw)
  (let* ((val (ac expr)))
    (list (list (ac-env! var)
                (if is-kw
                    `(if (unset? (ar-funcall2 ,ra ',is-kw unset))
                         ,val
                         (ar-funcall1 ,ra ',is-kw))
                    `(if (pair? ,ra) (car ,ra) ,val))))))

; (a b . c) -> (a b c)
; a -> (a)
; ((o a)) -> (a)
; (a: b) -> (b)
; (a: (o b)) -> (b)

(define (ac-arglist a)
  (cond ((null? a) '())
        ((symbol? a) (list a))
        ((caar? a 'o)
         (cons (cadar a) (ac-arglist (cdr a))))
        ((car? a keywordp) (ac-arglist (cdr a)))
        (#t (cons (car a) (ac-arglist (cdr a))))))

(define (ac-body body)
  (map ac body))

; like ac-body, but spits out a nil expression if empty

(define (ac-body* body)
  (if (null? body)
      (list (list 'quote ar-nil))
      (ac-body body)))

(define (ac-do body)
  (let ((expr (ac-body* body)))
    (cond ((= (length expr) 0)
           '(begin))
          ((= (length expr) 1)
           (car expr))
          (#t `(begin ,@expr)))))

; (set v1 expr1 v2 expr2 ...)

(define (ac-set x)
  `(begin ,@(ac-setn x)))

(define (ac-setn x)
  (if (null? x)
      '()
      (cons (ac-set1 (ac-macex (car x)) (cadr x))
            (ac-setn (cddr x)))))

; trick to tell Scheme the name of something, so Scheme
; debugging and profiling make more sense.

(define (ac-nameit v (name (ac-lexname)))
  (if (symbol? name)
      (let ((n (ac-identifier name)))
        (list 'let `((,n ,v)) n))
      v))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b1)
  (if (symbol? a)
      (let ((n (ac-identifier a))
            (b (parameterize ((env* (env*)))
                 (ac-dbname! a)
                 (ac b1))))
        (list 'let `((,n ,b))
               (cond ((eqv? a 'nil) (err "Can't rebind nil"))
                     ((eqv? a 't) (err "Can't rebind t"))
                     ((eqv? a 'true) (err "Can't rebind true"))
                     ((eqv? a 'false) (err "Can't rebind false"))
                     ((ac-boxed? 'set a)  `(begin ,(ac-boxed-set a b) ,(ac-boxed-get a)))
                     ((ac-lex? a) `(set! ,a ,n))
                     (#t `(namespace-set-variable-value! ',(ac-global-name a)
                                                         ,n
                                                         #t)))
               n))
      (err "First arg to set must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define (ac-args names exprs)
  (if (null? exprs)
      '()
      (cons (parameterize ((env* (env*)))
              (ac-dbname! (car? names))
              (ac (car exprs)))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)))))

(define (ac-lexname (names (ac-dbname)))
  (ar-concat (map (ar-to 'sym) (keep ar-true? names)) "--"))

(define (ac-lexvars (env (env*)))
  (remove-duplicates (keep symbol? env)))

(define (ac-lexenv (env (env*)))
  `(list ,@(map ac-lexframe (ac-lexvars env))))

(define (ac-lexframe var)
  (let ((val (ar-gensym 'val)))
    `(list ',var
           (lambda () ,var)
           (lambda (,val) (set! ,var ,val)))))

(define boxed* (make-parameter '() #f 'boxed*))

(define (ac-boxed? op name)
  (let ((result
    (when (not (ar-false? name))
      (when (not (ar-false? (boxed*)))
        (let ((slot (assoc name (boxed*))))
          (case op
            ((get) (when (and slot (>= (length slot) 2)) (cadr slot)))
            ((set) (when (and slot (>= (length slot) 3)) (caddr slot)))
            (else (err "ac-boxed?: bad op" name op))))))))
    (if (void? result) #f result)))

(define (ac-boxed-set name val)
  (let ((setter (ac-boxed? 'set name)))
     (if (procedure? setter)
       `(,setter ,val)
       (err "invalid setter" name val setter))))

(define (ac-boxed-get name)
  (let ((getter (ac-boxed? 'get name)))
    (if (procedure? getter)
      `(,getter)
      getter)))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(define ac-binaries
  '((is ar-is2)
    (< ar-<2)
    (> ar->2)
    (+ ar-+2)))

; (foo bar) where foo is a global variable bound to a procedure.

(define (ac-global-call fn args)
  (cond ((and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args)))
        (#t
         `(,(ac-global-name fn) ,@(ac-args '() args)))))

; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(define direct-calls #f)

(define (ac-call fn args)
  (let ((args (ac-unflag-args args)))
    (cond ((car? fn 'fn)
           `(,(ac fn) ,@(ac-args (cadr fn) args)))
          ((and direct-calls (symbol? fn) (not (ac-lex? fn))
                (procedure? (bound? fn)))
           (ac-global-call fn args))
          ((memf keywordp args)
           `(,(ac fn) ,@(map ac args)))
          ((= (length args) 0)
           `(ar-funcall0 ,(ac fn) ,@(map ac args)))
          ((= (length args) 1)
           `(ar-funcall1 ,(ac fn) ,@(map ac args)))
          ((= (length args) 2)
           `(ar-funcall2 ,(ac fn) ,@(map ac args)))
          ((= (length args) 3)
           `(ar-funcall3 ,(ac fn) ,@(map ac args)))
          ((= (length args) 4)
           `(ar-funcall4 ,(ac fn) ,@(map ac args)))
          (#t
           `(ar-apply ,(ac fn)
                      (list ,@(map ac args)))))))

(define (ar-unstash args (kwargs #f) (vals '()) (keys '()))
  (cond ((ar-list? kwargs)
         (list args (cadr (ar-unstash kwargs #f vals keys))))
        ((null? args)
         (list (reverse vals) (sort keys keyword<? #:key car)))
        ((keywordp (car args))
         (if (or (null? (cdr args))
                 (keywordp (cadr args)))
             (ar-unstash (cdr args) kwargs vals (cons (list (keywordp (car args)) #t) keys))
             (ar-unstash (cddr args) kwargs vals (cons (list (keywordp (car args)) (cadr args)) keys))))
        (#t (ar-unstash (cdr args) kwargs (cons (car args) vals) keys))))

(define (ar-kwapply f args (kwargs #f))
  (let* ((it (ar-unstash args kwargs))
         (args (car it))
         (kwargs (cadr it)))
    (if (null? kwargs)
        (ar-apply f args)
        (keyword-apply f (map car kwargs) (map cadr kwargs) args))))

; returns #f or the macro function

(define (ac-macro? fn (kind 'mac))
  (if (symbol? fn)
      (let ((v (bound? fn)))
        (if (and v
                 (ar-tagged? v)
                 (eq? (ar-type v) kind))
            (ar-rep v)
            #f))
      #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e (once #f))
  (if (pair? e)
      (let ((m (ac-macro? (car e))))
        (if m
            (let ((expansion (ar-kwapply m (ac-unflag-args (cdr e)))))
              (if (car? expansion '%expansion)
                  (cadr expansion)
                  (if once expansion (ac-macex expansion))))
            e))
      e))

; is v lexically bound?

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

(define (ac-lex? v (env (env*)))
  (memq v env))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))

(define (ac-andf s)
  (ac (let ((gs (map (lambda (x) (or (keywordp x) (ar-gensym 'andf))) (cdr s))))
               `((fn ,gs
                   (and ,@(map (lambda (f) `(,f ,@gs))
                               (cdar s))))
                 ,@(cdr s)))))

(define err error)

(define-namespace-anchor arc-anchor)
; (define (arc-namespace) (namespace-anchor->namespace arc-anchor))

(define arc-namespace (make-parameter (current-namespace) #f 'arc-namespace))

; run-time primitive procedures

;(define (xdef a b)
;  (namespace-set-variable-value! (ac-global-name a) b)
;  b)

(define-syntax xdef
  (syntax-rules ()
    ((xxdef a b)
     (let* ((nm (ac-global-name 'a))
            (a b)
            (val (namespace-variable-value nm #t (lambda () (void)))))
       (when (and (not (eqv? 'a 'b))
                  (not (void? val)))
         (display "*** redefining " (current-error-port))
         (display 'a (current-error-port))
         (display " (was " (current-error-port))
         (write a (current-error-port))
         (display ")\n" (current-error-port)))
       (namespace-set-variable-value! nm a #t)))))

(define fn-signatures (make-hash))

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-set! fn-signatures a (list parms))
  b)

(xdef unstash ar-unstash)

(xdef lex ac-lex?)

(xdef lexname ac-lexname)

(xdef eof eof)

(xdef sig fn-signatures)

(xdef quoted ac-quoted)

(xdef unquoted ac-unquoted)

; versions of car and cdr for parsing arguments for optional
; parameters, that yield nil for nil. maybe we should use
; full Arc car and cdr, so we can destructure more things

(define (ar-xcar x)
  (if (ar-nil? x) x (car x)))

(define (ar-xcdr x)
  (if (ar-nil? x) x (cdr x)))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (ar-nil? x)
      (eq? x #f)
      (void? x)
      (eq? x undefined)))

(define (ar-true? x)
  (not (ar-false? x)))

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
        ((pair? fn)
         (list-ref fn (car args)))
        ((string? fn)
         (string-ref fn (car args)))
        ((hash? fn)
         (hash-ref fn
                   (car args)
                   (if (pair? (cdr args)) (cadr args) ar-nil)))
        ((and (ar-seq? fn) (not (null? fn)))
         (sequence-ref fn (car args)))
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets
; passed to the first arg, i.e. ('kids item) means (item 'kids).
        (#t (err "Function call on inappropriate object" fn args))))

(xdef apply
      (make-keyword-procedure
        (lambda (keys vals fn . args)
          (keyword-apply fn keys vals (ar-apply-args args)))
        (lambda (fn . args)
          (ar-kwapply fn (ar-apply-args args)))))

(xdef kwapply ar-kwapply)

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



(xdef join (lambda ((x ar-nil) (y ar-nil))
             (if (unset? x) y (cons x y))))


(xdef car (lambda (x)
             (cond ((pair? x)     (car x))
                   ((null? x)     x)
                   (#t            (err "Can't take car of" x)))))

(xdef cdr (lambda (x)
             (cond ((pair? x)     (cdr x))
                   ((null? x)     x)
                   (#t            (err "Can't take cdr of" x)))))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; reduce?

(define (pairwise pred lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (ar-true? (pred (car lst) (cadr lst)))
           (pairwise pred (cdr lst)))))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

; for (is x y)

(define (ar-id a b)
  (or (eqv? a b)
      (and (number? a) (number? b) (= a b))
      (and (string? a) (string? b) (string=? a b))
      (and (bytes? a) (bytes? b) (bytes=? a b))))

(xdef id (lambda args (pairwise ar-id args)))

(define (ar-is2 a b)
  (or (ar-id a b)
      (and (ar-false? a) (ar-false? b))))

; for all other uses of is

(xdef is (lambda args (pairwise ar-is2 args)))

(xdef raise raise)
(xdef err err)
(xdef nil ar-nil)
(xdef t   ar-t)
(xdef false #f)
(xdef true  #t)

(define (all test seq)
  (or (null? seq)
      (and (test (car seq)) (all test (cdr seq)))))

; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(define (ar-to type . args)
  (lambda (x) (apply ar-coerce x type args)))

(define (ar-cat . args)
  (if (bytes? (ar-xcar args))
      (apply bytes-append (map (ar-to 'bytes 'utf8) args))
      (apply string-append (map (ar-to 'string) args))))

(define (char-or-string? x)
  (or (char? x) (string? x) (bytes? x)))

(define (ar-list? x)
  (or (null? x) (pair? x)))

(define (ar-concat xs (sep ""))
  (and (pair? xs)
       (apply ar-+ (add-between xs sep))))

(define (ar-+ . args)
  (cond ((null? args) 0)
        ((char-or-string? (car args))
         (apply ar-cat args))
        ((ar-list? (car args))
         (apply append args))
        ((evt? (car args))
         (apply choice-evt args))
        ((path? (car args))
         (apply build-path args))
        ((symbol? (car args))
         (string->symbol (apply ar-cat args)))
        ((keyword? (car args))
         (string->keyword (apply ar-cat args)))
        (#t (apply + args))))

(xdef + ar-+)

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
        (#t (+ x y))))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)

; generic comparison

(define (ar->2 x y)
  (cond ((and (number? x) (number? y)) (> x y))
        ((and (string? x) (string? y)) (string>? x y))
        ((and (symbol? x) (symbol? y)) (string>? (symbol->string x) (symbol->string y)))
        ((and (keyword? x) (keyword? y)) (string>? (keyword->string x) (keyword->string y)))
        ((and (bytes? x) (bytes? y)) (bytes>? x y))
        ((and (char? x) (char? y)) (char>? x y))
        (#t (> x y))))

(xdef > (lambda args (pairwise ar->2 args)))

(define (ar-<2 x y)
  (cond ((and (number? x) (number? y)) (< x y))
        ((and (string? x) (string? y)) (string<? x y))
        ((and (symbol? x) (symbol? y)) (string<? (symbol->string x) (symbol->string y)))
        ((and (keyword? x) (keyword? y)) (string<? (keyword->string x) (keyword->string y)))
        ((and (bytes? x) (bytes? y)) (bytes<? x y))
        ((and (char? x) (char? y)) (char<? x y))
        (#t (< x y))))

(xdef < (lambda args (pairwise ar-<2 args)))

(define (ar-seq? x)
  (and (sequence? x)
       (not (number? x))
       (not (null? x))
       (not (hash? x))
       (not (port? x))))

(xdef len (lambda (x)
             (cond ((ar-list? x) (length x))
                   ((ar-seq? x) (sequence-length x))
                   ((hash? x) (hash-count x))
                   ((symbol? x) (string-length (symbol->string x)))
                   ((keyword? x) (string-length (keyword->string x)))
                   (#t (err "Can't get len of" x)))))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (ar-tagged type rep))))

(xdef annotate ar-tag)

; (type nil) -> sym

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond ((ar-tagged? x)     (ar-tagged-type x))
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((null? x)          'sym)
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
        ((evt? x)           'event)
        ((boolean? x)       'bool)
        ((eof-object? x)    'eof)
        (#t                 (typeof x))))

(define (typeof x)
  (let ((tag (symbol->string (vector-ref (struct->vector x) 0))))
    (string->symbol (substring tag (string-length "struct:")))))

(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (ar-tagged-rep x)
      x))

(xdef rep ar-rep)

(define (ar-gensym . names)
  (ac-identifier (gensym (or (ac-lexname names) 'x))))

(xdef uniq ar-gensym)

(xdef ccc call-with-current-continuation)

(xdef modtime file-or-directory-modify-seconds)

(xdef infile  open-input-file)

(xdef outfile (lambda (f . args)
                 (open-output-file f
                                   #:mode 'text
                                   #:exists (if (equal? args '(append))
                                                'append
                                                'truncate))))

(xdef instring  (lambda (x) (if (bytes? x)
                                (open-input-bytes x)
                                (open-input-string x))))
(xdef outstring open-output-string)

; use as general fn for looking inside things

(define (ar-inside port #:bytes (bytes #f))
  (if (ar-false? bytes)
      (get-output-string port)
      (get-output-bytes port)))

(xdef inside ar-inside)

(xdef stdout current-output-port)  ; should be a vars
(xdef stdin  current-input-port)
(xdef stderr current-error-port)

(xdef call-w/param
      (lambda (var val thunk)
        (parameterize ((var val))
          (thunk))))

(xdef readc (lambda ((i (current-input-port)) (fail #f))
              (let ((c (read-char i)))
                (if (eof-object? c) fail c))))


(xdef readb (lambda ((i (current-input-port)) (fail #f))
              (let ((c (read-byte i)))
                (if (eof-object? c) fail c))))

(define (ready? check peek i fail)
  (atomic-invoke
    (lambda ()
      (if (check i) (peek i) fail))))

(xdef peekc (lambda ((i (current-input-port)) (fail #f))
              (let ((c (ready? char-ready? peek-char i eof)))
                (if (eof-object? c) fail c))))

(xdef peekb (lambda ((i (current-input-port)) (fail #f))
              (let ((c (ready? byte-ready? peek-byte i eof)))
                (if (eof-object? c) fail c))))

(xdef writec (lambda (c (p (current-output-port)))
                (write-char c p)
                c))

(xdef writeb (lambda (b (p (current-output-port)))
                (write-byte b p)
                b))

(define explicit-flush #f)

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (car args) port))
    (unless explicit-flush (flush-output port)))
  ar-nil)

(define (ar-display? x)
  (or (string? x)
      (symbol? x)
      (char? x)
      (bytes? x)))

(define (ar-display x port)
  (if (ar-display? x)
      (display x port)
      (write x port)))

(xdef write (lambda args (printwith write   args)))
(xdef disp  (lambda args (printwith ar-display args)))

(xdef sdata sdata)
(xdef sread sread)

; these work in PLT but not scheme48

(define char->ascii char->integer)
(define ascii->char integer->char)

(define (keyword->symbol x) (string->symbol (keyword->string x)))
(define (symbol->keyword x) (string->keyword (symbol->string x)))

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
  (define (retry x)
    (apply ar-coerce x type args))
  (cond
    ((ar-tagged? x) (err "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->ascii x))
                      ((string)  (string x))
                      ((bytes)   (retry (string x)))
                      ((sym)     (string->symbol (string x)))
                      ((keyword) (string->keyword (string x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (ascii->char x))
                      ((string)  (apply number->string x args))
                      ((bytes)   (retry (number->string x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (ascii->char (iround x)))
                      ((string)  (apply number->string x args))
                      ((bytes)   (retry (number->string x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (string->list x))
                      ((keyword) (string->keyword x))
                      ((bytes)   (if (null? args)
                                     (string->bytes/latin-1 x)
                                     (string->bytes/utf-8 x)))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((bytes? x)     (case type
                      ((cons)    (bytes->list x))
                      ((string)  (if (null? args)
                                     (bytes->string/latin-1 x)
                                     (bytes->string/utf-8 x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((pair? x)      (case type
                      ((bytes)   (list->bytes x))
                      ((string)  (if (car? x byte?)
                                     (retry (list->bytes x))
                                     (apply ar-cat x)))
                      ((sym)     (string->symbol (apply ar-cat x)))
                      ((keyword) (string->keyword (apply ar-cat x)))
                      ((bool)    #t)
                      ((bytes)   (if (car? x byte?)
                                     (list->bytes x)
                                     (err "Can't coerce" x type)))
                      (else      (err "Can't coerce" x type))))
    ((ar-nil? x)    (case type
                      ((bytes)   #"")
                      ((string)  "")
                      ((bool)    #f)
                      ((keyword) (string->keyword ""))
                      (else      (err "Can't coerce" x type))))
    ((keyword? x)    (case type
                      ((string)  (keyword->string x))
                      ((sym)     (keyword->symbol x))
                      ((cons)    (string->list (keyword->string x)))
                      ((bytes)   (retry (keyword->string x)))
                      ((bool)    #t)
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type
                      ((string)  (symbol->string x))
                      ((keyword) (symbol->keyword x))
                      ((cons)    (string->list (symbol->string x)))
                      ((bool)    #t)
                      ((bytes)   (retry (symbol->string x)))
                      (else      (err "Can't coerce" x type))))
    ((boolean? x)   (case type
                      ((string)  (if x  "t"  "false"))
                      ((bytes)   (if x #"t" #"false"))
                      (else      (err "Can't coerce" x type))))
    (#t             (err "Can't coerce" x type))))

(xdef coerce ar-coerce)

(xdef open-socket  (lambda (num) (tcp-listen num 50 #t)))

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef socket-accept (lambda (s)
                      (parameterize ((current-custodian (make-custodian)))
                        (let-values (((in out) (tcp-accept s)))
                          (let ((in1 (make-limited-input-port in 100000 #t)))
                            (associate-custodian (current-custodian) in1 out)
                            (list in1
                                  out
                                  (let-values (((us them) (tcp-addresses out)))
                                              them)))))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
; (define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)
;                  ; dummy version for Windows: http://arclanguage.org/item?id=10625.
;                  (lambda () (lambda (x) ar-nil))))
; (xdef setuid setuid)
(xdef setuid (lambda args ar-nil))

(define getuid (get-ffi-obj 'getuid #f (_fun -> _int)))
(xdef getuid getuid)

(xdef new-thread thread)
(xdef kill-thread kill-thread)
(xdef break-thread break-thread)
(xdef current-thread current-thread)
(xdef wait (lambda (thd)
             (when (thread? thd)
               (unless (eqv? thd (current-thread))
                 (thread-wait thd)))
             ar-t))

(define (wrapnil f) (lambda args (apply f args) ar-nil))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?

(xdef system (if (eqv? (system-type) 'windows) (lambda args ar-nil) (wrapnil system)))

(define (rmrf path)
  (delete-directory/files path #:must-exist? #f))
(xdef rmrf rmrf)

(xdef ensure-dir (wrapnil make-directory*))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (lambda args
              (let ((h (make-hash)))
                (when (pair? args) ((car args) h))
                h)))

(xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-for-each table fn)
                  table))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(xdef protect protect)

; need to use a better seed

(xdef rand random)

(xdef dir (lambda (name)
            (map path->string (directory-list name))))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(xdef file-exists (lambda (name)
                    (if (file-exists? name) name #f)))

(xdef dir-exists (lambda (name)
                   (if (directory-exists? name) name #f)))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (lambda (old new)
                (rename-file-or-directory old new #t)
                ar-nil))

(define (ar-expand-path name (directory (current-directory)))
  (let ((directory (path->complete-path (expand-user-path directory))))
    (path->string (simplify-path (path->complete-path (expand-user-path name) directory)))))

(xdef expandpath ar-expand-path)

(define-runtime-path here-path (build-path "."))
(define ar-libdir (make-parameter (ar-expand-path "." here-path)
                                #f
                                'libdir))

(xdef libdir ar-libdir)
(xdef cwd current-directory)

(define (ar-library-path . parts)
  (ar-expand-path (apply build-path parts)
                  (ar-libdir)))

(xdef libpath ar-library-path)

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

(define ac-verbose? (getenv "ARC_VERBOSE"))

(define (arc-eval expr (lexenv #f))
  (if lexenv
      (arc-eval-boxed expr lexenv)
      (seval (ac (if ac-verbose? (pp expr) expr)))))

(define (arc-eval-boxed expr lexenv)
  (parameterize ((boxed* (if (or (ar-false? (boxed*))
                                 (ar-false? lexenv))
                             lexenv
                             (append lexenv (boxed*)))))
    (arc-eval expr)))


(define (tle)
  (display "Arc> ")
  (let ((expr (read)))
    (when (not (eqv? expr ':a))
      (write (arc-eval expr))
      (newline)
      (tle))))

(define (interact)
  (display "Use (quit) to quit, (interact) to return here after an interrupt.\n")
  (tl2))

(define ac-that-expr* (make-parameter (void) #f 'ac-that-expr*))

(define (ac-read-interaction src in)
  (parameterize ((read-accept-reader #t)
                 (read-accept-lang #f))
    (let ((stx (read-syntax src in)))
      (if (eof-object? stx) stx
          (begin (ac-that-expr* stx)
                 (ac stx))))))

(define (ac-prompt-read)
  ; (display (format "arc:~a> " (source-location-line (ac-that-expr*))))
  (display "arc> ")
  (let ((in ((current-get-interaction-input-port))))
    (parameterize ((current-read-interaction ac-read-interaction))
      ((current-read-interaction) (object-name in) in))))

(define (pp-to-string val)
  (let* ((s (disp-to-string val pretty-print))
         (n (string-length s)))
    (if (and (> n 0)
             (char=? (string-ref s 0) #\'))
      (substring s 1 (- n 1))
      (substring s 0 (- n 1)))))

(define (pp val (port (current-output-port)))
  (display (pp-to-string val) port)
  (display #\newline port)
  val)
 
(define (ac-prompt-print val)
  (namespace-set-variable-value! (ac-global-name 'that) val)
  (namespace-set-variable-value! (ac-global-name 'thatexpr) (ac-that-expr*))
  (unless (or (void? val)
              (ar-nil? val))
    (pp val))
  val)

(define (tl2)
  (parameterize ((port-count-lines-enabled #t)
                 (current-namespace (arc-namespace))
                 (current-prompt-read ac-prompt-read)
                 (current-print       ac-prompt-print)
                 ; ((dynamic-require 'readline/pread 'current-prompt) #"arc> ")
                 ; (current-prompt-read (dynamic-require 'readline/pread 'read-cmdline-syntax))
                 )
    ; (dynamic-require 'xrepl #f)
    (port-count-lines! (current-input-port))
    (read-eval-print-loop)))

(define (aload1 p)
  (let ((x (sread p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ((x (sread p)))
    (if (eof-object? x)
        #t
        (begin
          (write x)
          (newline)
          (let ((v (arc-eval x)))
            (when (ar-false? v)
              (display "  FAILED")
              (newline)))
          (atests1 p)))))

(define (call-with-load-file filename thunk)
  (set! filename (ar-expand-path filename))
  (parameterize ((current-directory (ar-expand-path ".." filename)))
    (call-with-input-file filename thunk)))

(define (aload filename)
  (call-with-load-file filename aload1))

(define (test filename)
  (call-with-load-file filename atests1))

(define (acompile1 ip op)
  (let ((x (sread ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x)))
          (eval scm)
          (pp (datum scm) op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm")))
    (when (file-exists? outname)
      (delete-file outname))
    (call-with-load-file inname
      (lambda (ip)
        (call-with-output-file outname
          (lambda (op)
            (display "#lang racket/load" op)
            (display #\newline op)
            (acompile1 ip op)))))))

(xdef macex (lambda (e) (ac-macex e)))

(xdef macex1 (lambda (e) (ac-macex e 'once)))

(xdef eval arc-eval)

(define (seval s)
  (eval s))

(xdef seval seval)

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f)
  ((call-with-current-continuation
     (lambda (k)
       (lambda ()
         (with-handlers ((exn:fail? (lambda (c)
                                      (k (lambda () (errfn c))))))
                        (f)))))))
(xdef on-err on-err)

(define (disp-to-string x (writer display))
  (let ((o (open-output-string)))
    (writer x o)
    (close-output-port o)
    (get-output-string o)))

(xdef details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef scar (lambda (x val)
              (if (string? x)
                  (string-set! x 0 val)
                  (x-set-car! x val))
              val))

(xdef scdr (lambda (x val)
              (if (string? x)
                  (err "Can't set cdr of a string" x)
                  (x-set-cdr! x val))
              val))

; waterhouse's code to modify Racket's immutable pairs.
; http://arclanguage.org/item?id=13616
(require racket/unsafe/ops)

(define x-set-car!
  (let ((fn (namespace-variable-value 'set-car! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-immutable-car! p x)
              (raise-type-error 'set-car! "pair" p))))))

(define x-set-cdr!
  (let ((fn (namespace-variable-value 'set-cdr! #t (lambda () #f))))
    (if (procedure? fn)
        fn
        (lambda (p x)
          (if (pair? p)
              (unsafe-set-immutable-cdr! p x)
              (raise-type-error 'set-cdr! "pair" p))))))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref
  (lambda (com val ind)
    (cond ((hash? com)  (if (ar-nil? val)
                            (hash-remove! com ind)
                            (hash-set! com ind val)))
          ((string? com) (string-set! com ind val))
          ((bytes? com)  (bytes-set! com ind val))
          ((pair? com)   (nth-set! com ind val))
          (#t (err "Can't set reference " com ind val)))
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst n) val))

; rewrite to pass a (true) gensym instead of #f in case var bound to #f

(define (bound? arcname (fail #f))
  (let ((it (namespace-variable-value (ac-global-name arcname)
                                      #t
                                      (lambda () undefined))))
    (if (eq? it undefined) fail it)))

(xdef bound (lambda (x (fail ar-nil))
              (bound? x fail)))

(xdef newstring make-string)

(xdef trunc (lambda (x) (inexact->exact (truncate x))))

; bad name

(xdef exact exint?)

(xdef msec                         current-milliseconds)
(xdef mnow                         current-inexact-milliseconds)

(xdef seconds current-seconds)
(xdef now (lambda () (/ (current-inexact-milliseconds) 1000)))

(print-hash-table #t)

(xdef client-ip (lambda (port)
                   (let-values (((x y) (tcp-addresses port)))
                     y)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(define atomic-invoke (lambda (f)
                       (if (thread-cell-ref ar-sema-cell)
                           (ar-apply f '())
                           (begin
                             (thread-cell-set! ar-sema-cell #t)
			     (protect
			      (lambda ()
				(call-with-semaphore
				 ar-the-sema
				 (lambda () (ar-apply f '()))))
			      (lambda ()
				(thread-cell-set! ar-sema-cell #f)))))))
(xdef atomic-invoke atomic-invoke)

(xdef dead thread-dead?)

(xdef chan (lambda args
             (cond
               ((null? args)           (make-channel))
               ((ar-false? (car args)) (make-async-channel #f))
               ((positive? (car args)) (make-async-channel (car args)))
               ((zero? (car args))     (make-channel))
               (#t (err "Channel limit must be > 0 or nil: " (car args))))))

(define (sync? . args)
  (apply sync/timeout 0 args))

(define (chan-fn c method)
  (cond ((channel? c)
         (cond ((eq? method 'get)     channel-get)
               ((eq? method 'try-get) channel-try-get)
               ((eq? method 'put)     channel-put)
               ((eq? method 'put-evt) channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((async-channel? c)
         (cond ((eq? method 'get)     async-channel-get)
               ((eq? method 'try-get) async-channel-try-get)
               ((eq? method 'put)     async-channel-put)
               ((eq? method 'put-evt) async-channel-put-evt)
               (#t (err "chan-fn: invalid method: " method))))
        ((and (evt? c) (or (eq? method 'get) (eq? method 'try-get)))
         sync?)
        (#t (err "chan-fn: invalid channel: " c))))

(xdef <- (lambda (c . args)
           (if (null? args)
               ((chan-fn c 'get) c)
               (begin ((chan-fn c 'put) c (cons c args))
                      args))))

(xdef <-? (lambda (c . args)
            (if (null? args)
                ((chan-fn c 'try-get) c)
                (let* ((evt ((chan-fn c 'put-evt) c (cons c args)))
                       (ret (sync/timeout 0 evt)))
                  (if (eq? ret #f) ar-nil args)))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout (lambda ((port (current-output-port)))
                 (flush-output port)
                 ar-t))

(xdef ssyntax ssyntax?)

(xdef ssexpand (lambda (x)
                  (if (ssyntax? x) (expand-ssyntax x) x)))

(xdef quit exit)

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash))

(define (associate-custodian c i o)
  (hash-set! custodians i c)
  (hash-set! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ((c (hash-ref custodians p #f)))
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-remove! custodians p)
          #t)
        #f)))

(define (ar-close . args)
  (map (lambda (p)
         (cond ((input-port? p)   (close-input-port p))
               ((output-port? p)  (close-output-port p))
               ((tcp-listener? p) (tcp-close p))
               (#t (err "Can't close " p))))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  ar-nil)

(xdef close ar-close)

(xdef force-close (lambda args
                       (map (lambda (p)
                              (unless (try-custodian p)
                                  (ar-close p)))
                            args)
                       ar-nil))

(xdef memory current-memory-use)

(xdef declare (lambda (key val)
                (let ((flag (not (ar-false? val))))
                  (case key
                    ((atstrings)      (set! atstrings      flag))
                    ((direct-calls)   (set! direct-calls   flag))
                    ((explicit-flush) (set! explicit-flush flag)))
                  val)))

(xdef get-environment-variable getenv)
(xdef set-environment-variable putenv)

(void (putenv "TZ" ":GMT"))

(define (gmt-date sec) (seconds->date sec))

(xdef timedate
  (lambda args
    (let ((d (gmt-date (if (pair? args) (car args) (current-seconds)))))
      (list (date-second d)
            (date-minute d)
            (date-hour d)
            (date-day d)
            (date-month d)
            (date-year d)))))

(xdef sin sin)
(xdef cos cos)
(xdef tan tan)
(xdef asin asin)
(xdef acos acos)
(xdef atan atan)
(xdef log log)

(define (codestring s)
  (let ((i (atpos s 0)))
    (if i
        (cons (substring s 0 i)
              (let* ((rest (substring s (+ i 1)))
                     (in (open-input-string rest))
                     (expr (read in))
                     (expr (if (car? expr '%braces) (cadr expr) expr))
                     (i2 (let-values (((x y z) (port-next-location in))) z)))
                (close-input-port in)
                (cons expr (codestring (substring rest (- i2 1))))))
        (list s))))

; First unescaped @ in s, if any.  Escape by doubling.

(define (atpos s i)
  (cond ((eqv? i (string-length s))
         #f)
        ((eqv? (string-ref s i) #\@)
         (if (and (< (+ i 1) (string-length s))
                  (not (eqv? (string-ref s (+ i 1)) #\@)))
             i
             (atpos s (+ i 2))))
        (#t
         (atpos s (+ i 1)))))

(define (unescape-ats s)
  (list->string (letrec ((unesc (lambda (cs)
                                  (cond
                                    ((null? cs)
                                     '())
                                    ((and (eqv? (car cs) #\@)
                                          (not (null? (cdr cs)))
                                          (eqv? (cadr cs) #\@))
                                     (unesc (cdr cs)))
                                    (#t
                                     (cons (car cs) (unesc (cdr cs))))))))
                  (unesc (string->list s)))))

(define bcrypt-lib-path
  (if (eqv? (system-type) 'windows)
    (build-path (ar-libdir) "src" "bcrypt" "bcrypt")
    (build-path (ar-libdir) "src" "bcrypt" "build" "libbcrypt")))

(define bcrypt-lib (ffi-lib bcrypt-lib-path))

(define bcrypt-fn (get-ffi-obj "bcrypt" bcrypt-lib (_fun _string _string (out : _bytes = (make-bytes 256)) -> _void -> (cast out _bytes _string/utf-8))))

(define (bcrypt pwd salt . failed) ; see BSD manual crypt(3)
  (let ((x (bcrypt-fn pwd salt)))
    (if (or (<= (string-length x) 0)
            (not (eqv? (string-ref x 0) #\$)))
      (if (pair? failed)
        (car failed)
        (error "bcrypt failed; use a salt like (+ \"$2a$10$\" (rand-string 22))"))
      x)))

(xdef system-type system-type)

(xdef make-param make-parameter)

(xdef write-json write-json)
(xdef read-json read-json)

(define uuid-generate
  (unless (eqv? (system-type) 'windows)
    (get-ffi-obj "uuid_generate" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
      (_fun (out : _bytes = (make-bytes 16)) -> _void -> (uuid-unparse out)))))

(define uuid-unparse
  (unless (eqv? (system-type) 'windows)
    (get-ffi-obj "uuid_unparse" (ffi-lib (if (eqv? (system-type 'os) 'macosx) "libSystem" "libuuid") '("1" ""))
      (_fun (uuid : _bytes) (out : _bytes = (make-bytes 32)) -> _void -> (cast out _bytes _string/utf-8)))))

(xdef uuid uuid-generate)

