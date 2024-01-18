; Arc Compiler.
#lang racket/base

(require json)
(require racket/string)
(require racket/list)
(require racket/vector)
(require racket/sequence)
(require racket/file)
(require racket/port)
(require racket/pretty)
(require racket/runtime-path)
(require racket/system)
(require racket/tcp)
(require racket/unsafe/ops)
(require racket/async-channel)
(require racket/undefined)
(require ffi/unsafe)
(require "./ar.scm")

; configure reader
; (read-square-bracket-with-tag #t)
; (read-curly-brace-with-tag #t)
(print-hash-table #t)
(print-syntax-width 10000)

; sread = scheme read. eventually replace by writing read

(define (shebang? (port (current-input-port)))
  (equal? "#!" (peek-string 2 0 port)))

(define (skip-shebang! (port (current-input-port)))
  (when (shebang? port)
    (read-line port)))

(define (sread (p (current-input-port)) (eof eof))
  (parameterize ((read-accept-lang #t)
                 (read-accept-reader #t))
    (port-count-lines! p)
    (skip-shebang! p)
    (let ((expr (read-syntax (object-name p) p)))
      (if (eof-object? expr) eof expr))))

(define (sdata (p (current-input-port)) (eof eof))
  (parameterize ((read-accept-lang #f)
                 (read-accept-reader #f))
    (port-count-lines! p)
    (skip-shebang! p)
    (let ((expr (read p)))
      (if (eof-object? expr) eof (ac-quoted expr)))))

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
        ((ar-car? s '%do) (ac-do (cdr s)))
        ((ar-car? s 'lexenv) (ac-lexenv))
        ((ar-car? s 'syntax) (scm-quoted (cadr s)))
        ((ar-caar? s 'syntax) (map ac s))
        ((ar-car? s ssyntax?) (ac (cons (expand-ssyntax (car s)) (cdr s))))
        ((ar-car? s 'quote) (list 'quote (ac-quoted (cadr s))))
        ((ar-car? s 'quasiquote) (ac-qq (cadr s)))
        ((ar-car? s 'quasisyntax) (ac-qs (cadr s)))
        ((ar-car? s 'if) (ac-if (cdr s)))
        ((ar-car? s 'fn) (ac-fn (cadr s) (cddr s)))
        ((ar-car? s 'assign) (ac-set (cdr s)))
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        ((ar-caar? s 'compose) (ac (decompose (cdar s) (cdr s))))
        ((ar-caar? s 'complement)
         (ac (list 'complement (cons (cadar s) (cdr s)))))
        ((ar-caar? s 'andf) (ac-andf s))
        ((pair? s) (ac-call (car s) (cdr s)))
        (#t s)))

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
      (ar-keyword? x)
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
         (if (= i 0)
             (or (eqv? c #\~) (eqv? c #\!))
             (or (eqv? c #\:) (eqv? c #\~)
                 (eqv? c #\&)
                 ;(eqv? c #\_)
                 (eqv? c #\.)  (eqv? c #\!)
                 (has-ssyntax-char? string (- i 1)))))))

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
                       (ar-err "Bad ssyntax" orig)
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

(define (ac-var-ref s)
  (cond ((ac-boxed? 'get s) (ac-boxed-get s))
        ((ac-lex? s)        s)
        ((eq? s 'globe)     '(current-namespace))
        ((eq? s 'scope)     (ac-lexenv))
        (#t                 (ar-name s))))

(define (ac-tonumber s (base 10))
  (on-err #f (lambda () (ar-coerce s 'num base))))

(define (ac-parse-number s (prefix #f))
  (and (string? s)
       (cond ((string-prefix? s "+")
              (and (not prefix) (ac-parse-number (substring s 1) #t)))
             ((string-prefix? s "-")
              (and (not prefix)
                   (let ((n (ac-parse-number (substring s 1) #t)))
                     (and n (- n)))))
             (#t (and (not (string-prefix? s "_"))
                      (let ((s (string-replace s "_" "")))
                        (cond ((or (string-prefix? s "0x")
                                   (string-prefix? s "0X"))
                               (ac-tonumber (substring s 2) 16))
                              ((or (string-prefix? s "0b")
                                   (string-prefix? s "0B"))
                               (ac-tonumber (substring s 2) 2))
                              ((or (string-prefix? s "0o")
                                   (string-prefix? s "0O"))
                               (ac-tonumber (substring s 2) 8))
                              (#t (ac-tonumber s)))))))))

(define (ac-number-literal s)
  (and (symbol? s) (ac-parse-number (symbol->string s))))

; quote

(define (scm-quoted x)
  (cond ((ar-car? x '%brackets)
         (cdr x))
        ((ar-car? x '%braces)
         (cdr x))
        ((pair? x)
         (imap scm-quoted x))
        (#t x)))

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
        ((eqv? x 'nan)
         ar-nan)
        ((eqv? x 'inf)
         ar-inf)
        ((eqv? x '-inf)
         ar-ninf)
        (#t (or (ar-keyword? x) (ac-number-literal x) x))))

(define (ac-unquoted x)
  (cond ((pair? x)
         (imap ac-unquoted x))
        ((null? x)
         'nil)
        ((eqv? x ar-t)
         't)
        ((eqv? x #f)
         'false)
        ((eqv? x ar-nan)
         'nan)
        ((eqv? x ar-inf)
         'inf)
        ((eqv? x ar-ninf)
         '-inf)
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
        ((ar-car? x 'unquote)
         (list 'unquote (ac-qq1 (- level 1) (cadr x))))
        ((and (ar-car? x 'unquote-splicing) (= level 1))
         (list 'unquote-splicing
               (ac-qq1 (- level 1) (cadr x))))
        ((ar-car? x 'quasiquote)
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
        ((ar-car? x '%brackets)
         (cdr x))
        ((ar-car? x '%braces)
         (cdr x))
        ((ar-car? x 'unsyntax)
         (ac-qs1 (- level 1) (cadr x)))
        ((and (ar-car? x 'unsyntax-splicing) (= level 1))
         (ac-qs1 (- level 1) (cadr x)))
        ((ar-car? x 'quasisyntax)
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
                 ((lambda ()
                   ,(ac (cadr args))))
                 ((lambda ()
                   ,(ac-if (cddr args))))))))

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
        ((ar-caar? x 'o)
         (ac-env! (cadr x)))
        ((pair? x)
         (imap ac-env! x)))
  x)

(define (ac-fn-args a)
  (cond ((null? a) '())
        ((symbol? a)
         (ac-env! a))
        ((ar-caar? a 'o)
         (let* ((it (cdar a))
                (var (car it))
                (key (ac-flag? var))
                (var (or key var))
                (val (ar-car (cdr it)))
                (expr (parameterize ((env* (env*)))
                        (ac-dbname! var)
                        (ac val))))
           (ac-env! var)
           (if key
               `(,(symbol->keyword var)
                 (,var ,expr) ,@(ac-fn-args (cdr a)))
               `((,var ,expr) ,@(ac-fn-args (cdr a))))))
        ((ar-car? a ar-keyword?)
         (let* ((key (ar-keyword? (car a)))
                (var (cadr a))
                (var (if (symbol? var) `(o ,var) var))) ; ensure all kwargs are optional
           (cons key (ac-fn-args `(,var ,@(cddr a))))))
        ((ar-car? a ac-flag?)
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
  (or (ar-car? args '#:kws)
      (and (pair? args)
           (ac-kwargs? (ar-cdr args)))))

(define (ar-kwproc f)
  (make-keyword-procedure
    (lambda (ks vs . args)
      (let ((kwargs (apply append (map list ks vs))))
        (apply f args #:kws kwargs)))
    f))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond ((null? args) #f)
        ((symbol? args) #f)
        ((or (ar-car? args symbol?)
             (ar-car? args ar-keyword?)
             (and (ar-caar? args 'o)
                  (ar-car? (cdar args) symbol?)))
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
        ((ar-car? args ac-flag?)
         (let ((var (ac-flag? (car args))))
           (ac-complex-args `(,(symbol->keyword var) (o ,var) ,@(cdr args)) ra is-params)))
        ((ar-car? args ar-keyword?)
         (ac-complex-args (cdr args) ra #f (keyword->symbol (ar-keyword? (car args)))))
        ((and (ar-caar? args 'o)
              (ac-flag? (cadar args)))
         (let ((var (ac-flag? (cadar args)))
               (val (cddar args)))
           (ac-complex-args
             `(,(symbol->keyword var) (o ,var ,@val) ,@(cdr args)) ra is-params #f)))
        ((pair? args)
         (let* ((x (if (ar-caar? args 'o)
                       (let ((var (cadar args))
                             (val (if (pair? (cddar args))
                                           (caddar args)
                                           ar-nil)))
                         (ac-complex-opt var val ra is-kw))
                       (ac-complex-args
                        (car args)
                        (cond (is-kw     `(ar-funcall2 ,ra ',is-kw ar-nil))
                              (is-params `(car ,ra))
                              (#t        `(ar-car ,ra)))
                        #f))))
           (append x (ac-complex-args (cdr args)
                                      (if is-kw ra `(ar-cdr ,ra))
                                      is-params))))
        (#t (ar-err "Can't understand fn arg list" args))))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(define (ac-complex-opt var expr ra is-kw)
  (let* ((val (ac expr)))
    (list (list (ac-env! var)
                (if is-kw
                    `(if (ar-unset? (ar-funcall2 ,ra ',is-kw ar-unset))
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
        ((ar-caar? a 'o)
         (cons (cadar a) (ac-arglist (cdr a))))
        ((ar-car? a ar-keyword?) (ac-arglist (cdr a)))
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
               (cond ((ac-boxed? 'set a)  (ac-boxed-set a b))
                     ((ac-lex? a)        `(set! ,a ,n))
                     ((not (eqv? a (ac-quoted a)))
                      (ar-err "Can't rebind constant" a))
                     (#t `(ar-namespace-set! ',(ar-name a) ,n)))
               n))
      (ar-err "First arg to set must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define (ac-args names exprs)
  (if (null? exprs)
      '()
      (cons (parameterize ((env* (env*)))
              (ac-dbname! (ar-car? names))
              (ac (car exprs)))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)))))

(define (ac-lexname (names (ac-dbname)))
  (define (lex x)
    (if (ar-car? x 'quote)
      (lex (ar-cadr x))
      ((ar-to 'sym) x)))
  (ar-concat (map lex (keep ar-true? names)) "--"))

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
            (else (ar-err "ac-boxed?: bad op" name op))))))))
    (if (void? result) #f result)))

(define (ac-boxed-set name val)
  (let ((setter (ac-boxed? 'set name)))
     (if (procedure? setter)
       `(,setter ,val)
       (ar-err "invalid setter" name val setter))))

(define (ac-boxed-get name)
  (let ((getter (ac-boxed? 'get name)))
    (if (procedure? getter)
      `(,getter)
      getter)))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(define ac-binaries
  '((is ar-is)
    (< ar-<2)
    (> ar->2)
    (+ ar-+2)))

; (foo bar) where foo is a global variable bound to a procedure.

(define (ac-global-call fn args)
  (cond ((and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args)))
        (#t
         `(,(ar-name fn) ,@(ac-args '() args)))))

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
  (let* ((args (ac-unflag-args args))
         (argc (length args)))
    (cond ((ar-car? fn 'fn)
           `(,(ac fn) ,@(ac-args (cadr fn) args)))
          ((and direct-calls (symbol? fn) (not (ac-lex? fn))
                (procedure? (ar-bound? fn)))
           (ac-global-call fn args))
          ((memf ar-keyword? args)
           `((ar-symbol-function ,(ac fn)) ,@(map ac args)))
          ((= argc 0) `(ar-funcall0 ,(ac fn) ,@(map ac args)))
          ((= argc 1) `(ar-funcall1 ,(ac fn) ,@(map ac args)))
          ((= argc 2) `(ar-funcall2 ,(ac fn) ,@(map ac args)))
          ((= argc 3) `(ar-funcall3 ,(ac fn) ,@(map ac args)))
          ((= argc 4) `(ar-funcall4 ,(ac fn) ,@(map ac args)))
          (#t `(ar-apply ,(ac fn) (list ,@(map ac args)))))))

; returns #f or the macro function

(define (ac-macro? fn (kind 'mac))
  (define v (and (symbol? fn) (ar-bound? fn)))
  (and (eq? (ar-type v) kind)
       (ar-rep v)))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e (once #f))
  (if (pair? e)
      (let ((m (ac-macro? (car e))))
        (if m
            (let ((expansion (ar-kwapply m #f (ac-unflag-args (cdr e)))))
              (if (ar-car? expansion '%expansion)
                  (cadr expansion)
                  (if once expansion (ac-macex expansion))))
            e))
      e))

; is v lexically bound?

(define (ac-lex? v (env (env*)))
  (memq v env))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))

(define (ac-andf s)
  (ac (let ((gs (map (lambda (x) (or (ar-keyword? x) (ar-gensym 'andf))) (cdr s))))
               `((fn ,gs
                   (and ,@(map (lambda (f) `(,f ,@gs))
                               (cdar s))))
                 ,@(cdr s)))))

(define-namespace-anchor arc-anchor)
; (define (arc-namespace) (namespace-anchor->namespace arc-anchor))

(define arc-namespace (make-parameter (current-namespace) #f 'arc-namespace))

; run-time primitive procedures

;(define (xdef a b)
;  (namespace-set-variable-value! (ar-name a) b)
;  b)

(define-syntax xdef
  (syntax-rules ()
    ((xxdef a b)
     (let* ((nm (ar-name 'a))
            (a b)
            (val (namespace-variable-value nm #t (lambda () (void)))))
       (when (and (not (eqv? 'a 'b))
                  (not (ar-nil? val)))
         (display "*** redefining " (current-error-port))
         (display 'a (current-error-port))
         (display " (was " (current-error-port))
         (write a (current-error-port))
         (display ")\n" (current-error-port)))
       (ar-namespace-set! nm a)))
    ((xxdef name parms body ...)
     (begin
       (hash-set! fn-signatures 'name 'parms)
       (xdef name (lambda parms body ...))))))

(define fn-signatures (make-hash))

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (ar-namespace-set! (ar-name a) b)
  (hash-set! fn-signatures a (list parms))
  b)

(xdef null ar-nil?)

(xdef unstash (x)
  (let* ((it (ar-unstash x))
         (args (car it))
         (kws (cadr it)))
    (list args (hash->plist kws))))

(xdef lex ac-lex?)

(xdef lexname ac-lexname)

(xdef eof eof)

(xdef sig fn-signatures)

(xdef quoted ac-quoted)

(xdef unquoted ac-unquoted)

; circular references will go into an infinite loop
(xdef symbol-value (name (fail ar-unset))
  (ar-symbol-value name fail))

(xdef symbol-function (name)
  (ar-symbol-function name))

(xdef apply
      (make-keyword-procedure
        (lambda (keys vals fn . args)
          (keyword-apply (ar-symbol-function fn) keys vals (ar-apply-args args)))
        (lambda (fn . args)
          (apply ar-kwapply fn #f args))))

(xdef kwapply ar-kwapply)

(xdef join ((x ar-nil) (y ar-nil))
  (ar-join x y))

(xdef car (x) (ar-car x))

(xdef cdr (x) (ar-cdr x))

(xdef pairwise ar-pairwise)

(xdef id args (ar-pairwise ar-id args))

(xdef is args (ar-pairwise ar-is args))

(xdef raise raise)
(xdef err ar-err)

(xdef true  #t)
(xdef false #f)

(xdef t ar-t)
(xdef nil ar-nil)
(xdef unset ar-unset)

(xdef nan ar-nan)
(xdef inf ar-inf)
(xdef -inf ar-ninf)

(define (all test seq)
  (or (null? seq)
      (and (test (car seq)) (all test (cdr seq)))))

(define (ar-concat xs (sep ""))
  (and (pair? xs)
       (apply ar-+ (add-between xs sep))))

(xdef join! (l x . args)
  (apply ar-join! l x args))

(xdef + ar-+)

(xdef - -)

(xdef * ar-*)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)

(xdef > args (ar-pairwise ar->2 args))

(xdef < args (ar-pairwise ar-<2 args))

(xdef len (x) (ar-len x))

(xdef annotate (type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (ar-tagged type rep))))

(xdef type ar-type)

(xdef typeof ar-typeof)

(xdef typename ar-typename)

(xdef rep ar-rep)

(define (ar-gensym . names)
  (ac-identifier (gensym (or (ac-lexname names) 'x))))

(xdef uniq ar-gensym)

(xdef ccc call-with-current-continuation)

(xdef modtime file-or-directory-modify-seconds)

(xdef infile  (f #:bytes (binary? #f))
  (open-input-file f #:mode (if (ar-true? binary?) 'binary 'text)))

(xdef outfile (f #:bytes (binary? #f) #:append (append? #f))
  (open-output-file f
                    #:mode (if (ar-true? binary?) 'binary 'text)
                    #:exists (if (ar-true? append?) 'append 'truncate)))

(xdef instring (x)
  (if (bytes? x)
      (open-input-bytes x)
      (open-input-string x)))

(xdef outstring open-output-string)

; use as general fn for looking inside things

(xdef inside (port #:bytes (bytes #f))
  (if (ar-false? bytes)
      (get-output-string port)
      (get-output-bytes port)))

(xdef stdout current-output-port)  ; should be a vars
(xdef stdin  current-input-port)
(xdef stderr current-error-port)

(xdef readc ((i (current-input-port)) (fail #f))
  (let ((c (read-char i)))
    (if (eof-object? c) fail c)))


(xdef readb ((i (current-input-port)) (fail #f))
  (let ((c (read-byte i)))
    (if (eof-object? c) fail c)))

(define (ready? check peek i fail)
  (ar-atomic-invoke
    (lambda ()
      (if (check i) (peek i) fail))))

(xdef peekc ((i (current-input-port)) (fail #f))
  (let ((c (ready? char-ready? peek-char i eof)))
    (if (eof-object? c) fail c)))

(xdef peekb ((i (current-input-port)) (fail #f))
  (let ((c (ready? byte-ready? peek-byte i eof)))
    (if (eof-object? c) fail c)))

(xdef writec (c (p (current-output-port)))
  (write-char c p)
  c)

(xdef writeb (b (p (current-output-port)))
  (write-byte b p)
  b)

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

(xdef write args (printwith write   args))
(xdef disp  args (printwith ar-display args))

(xdef sdata sdata)
(xdef sread sread)

(xdef coerce ar-coerce)

(xdef open-socket  (num) (tcp-listen num 50 #t))

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef socket-accept (s)
  (parameterize ((current-custodian (make-custodian)))
    (let-values (((in out) (tcp-accept s)))
      (let ((in1 (make-limited-input-port in 100000 #t)))
        (associate-custodian (current-custodian) in1 out)
        (list in1
              out
              (let-values (((us them) (tcp-addresses out)))
                          them))))))

(xdef new-thread thread)
(xdef kill-thread kill-thread)
(xdef break-thread break-thread)
(xdef current-thread current-thread)
(xdef wait (thd)
  (when (thread? thd)
    (unless (eqv? thd (current-thread))
      (thread-wait thd)))
  ar-t)

(define (wrapnil f) (lambda args (apply f args) ar-nil))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?

(xdef system (if (eqv? (system-type) 'windows) (lambda args ar-nil) (wrapnil system)))

(xdef rmrf (path)
  (delete-directory/files path #:must-exist? #f))

(xdef ensure-dir (wrapnil make-directory*))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (#:equal (equal #t) #:kind (kind 'mutable))
  (hash-copy-clear (if (ar-true? equal)
                       (make-hash)
                       (make-hasheqv))
                   #:kind kind))

(xdef maptable (fn table)               ; arg is (fn (key value) ...)
  (hash-for-each table fn)
  table)

(xdef protect (during after)
  (ar-protect during after))

; need to use a better seed

(xdef rand random)

(xdef dir (name)
  (map path->string (directory-list name)))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(xdef file-exists (name)
  (if (file-exists? name) name #f))

(xdef dir-exists (name)
  (if (directory-exists? name) name #f))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (old new)
  (rename-file-or-directory old new #t)
  ar-nil)

(define (ar-expand-path name (directory (current-directory)))
  (let ((directory (path->complete-path (expand-user-path directory))))
    (path->string (simplify-path (path->complete-path (expand-user-path name) directory)))))

(xdef expandpath ar-expand-path)

(define-runtime-path ar-path ".")
(define ar-libdir (make-parameter (ar-expand-path "." ar-path)
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

(define (pp-to-string val (writer pretty-write))
  (string-trim (disp-to-string val writer)))

(define (pp val (port (current-output-port)) (writer pretty-write))
  (displayln (pp-to-string val writer) port)
  val)
 
(define (ac-prompt-print val)
  (ar-namespace-set! (ar-name 'that) val)
  (ar-namespace-set! (ar-name 'thatexpr) (ac-that-expr*))
  (unless (null? val)
    (pp val))
  val)

(define (tl2)
  (parameterize ((port-count-lines-enabled #t)
                 ; (current-namespace (arc-namespace))
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
            (displayln "#lang racket/load" op)
            (acompile1 ip op)))))))

(xdef macex (e) (ac-macex e))

(xdef macex1 (e) (ac-macex e 'once))

(xdef eval arc-eval)

(define (seval s)
  (eval s))

(xdef seval seval)

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err fail f)
  (with-handlers ((exn:fail? (lambda (c)
                               (if (procedure? fail) (fail c) fail))))
    (f)))

(xdef on-err on-err)

(define (disp-to-string x (writer display))
  (let ((o (open-output-string)))
    (writer x o)
    (close-output-port o)
    (get-output-string o)))

(xdef details (c)
  (disp-to-string (exn-message c)))

(xdef xar (x val) (ar-xar x val))

(xdef xdr (x val) (ar-xdr x val))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in xdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (ar-err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref (com val ind)
  (ar-sref com val ind))

(xdef bound (x (fail ar-nil))
  (ar-bound? x fail))

(xdef newstring make-string)

(xdef trunc (x) (inexact->exact (truncate x)))

; bad name

(xdef exact exint?)

(xdef msec                         current-milliseconds)
(xdef mnow                         current-inexact-milliseconds)

(xdef seconds current-seconds)
(xdef now () (/ (current-inexact-milliseconds) 1000))

(xdef client-ip (port)
  (let-values (((x y) (tcp-addresses port)))
    y))

(xdef atomic-invoke (f) (ar-atomic-invoke f))

(xdef dead thread-dead?)

(xdef chan args
  (cond
    ((null? args)           (make-channel))
    ((ar-false? (car args)) (make-async-channel #f))
    ((positive? (car args)) (make-async-channel (car args)))
    ((zero? (car args))     (make-channel))
    (#t (ar-err "Channel limit must be > 0 or nil: " (car args)))))

(define (sync? . args)
  (apply sync/timeout 0 args))

(define (chan-fn c method)
  (cond ((channel? c)
         (cond ((eq? method 'get)     channel-get)
               ((eq? method 'try-get) channel-try-get)
               ((eq? method 'put)     channel-put)
               ((eq? method 'put-evt) channel-put-evt)
               (#t (ar-err "chan-fn: invalid method: " method))))
        ((async-channel? c)
         (cond ((eq? method 'get)     async-channel-get)
               ((eq? method 'try-get) async-channel-try-get)
               ((eq? method 'put)     async-channel-put)
               ((eq? method 'put-evt) async-channel-put-evt)
               (#t (ar-err "chan-fn: invalid method: " method))))
        ((and (evt? c) (or (eq? method 'get) (eq? method 'try-get)))
         sync?)
        (#t (ar-err "chan-fn: invalid channel: " c))))

(xdef <- (c . args)
  (if (null? args)
      ((chan-fn c 'get) c)
      (begin ((chan-fn c 'put) c (cons c args))
             args)))

(xdef <-? (c . args)
  (if (null? args)
      ((chan-fn c 'try-get) c)
      (let* ((evt ((chan-fn c 'put-evt) c (cons c args)))
             (ret (sync/timeout 0 evt)))
        (if (eq? ret #f) ar-nil args))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout ((port (current-output-port)))
  (flush-output port)
  ar-t)

(xdef ssyntax ssyntax?)

(xdef ssexpand (x)
  (if (ssyntax? x) (expand-ssyntax x) x))

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
               (#t (ar-err "Can't close " p))))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  ar-nil)

(xdef close ar-close)

(xdef force-close args
  (map (lambda (p)
         (unless (try-custodian p)
           (ar-close p)))
       args)
  ar-nil)

(xdef memory current-memory-use)

(xdef declare (key val)
  (let ((flag (not (ar-false? val))))
    (case key
      ((atstrings)      (set! atstrings      flag))
      ((direct-calls)   (set! direct-calls   flag))
      ((explicit-flush) (set! explicit-flush flag)))
    val))

(xdef get-environment-variable getenv)
(xdef set-environment-variable putenv)

(void (putenv "TZ" ":GMT"))

(define (gmt-date sec) (seconds->date sec))

(xdef timedate ((s (current-seconds)))
  (let ((d (gmt-date s)))
    (list (date-year d)
          (date-month d)
          (date-day d)
          (date-hour d)
          (date-minute d)
          (date-second d))))

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
                     (expr (if (ar-car? expr '%braces) (cadr expr) expr))
                     (expr `(%atstring ,expr))
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

(provide (all-defined-out))
