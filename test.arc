#!bin/arc

; adapted from test.l in https://github.com/sctb/lumen
(declare 'atstrings t)

(= passed* 0
   failed* 0)

(or= tests* (obj))

(mac test! (x msg)
  `(if (no ,x)
       (do (= failed* (+ failed* 1))
           (throw ,msg))
     (++ passed*)))

(def writes (x)
  (tostring (write x)))

(def equal? (a b)
  (if (atom a) (is a b)
    (is (writes a) (writes b))))

(mac test? (a b)
  (w/uniq (x y)
    `(withs (,x ,a ,y ,b)
       (test! (equal? ,x ,y)
              (+ "failed: expected " (writes ,x) ", was " (writes ,y))))))

(mac define-test (name . body)
  (let id (sym (+ "test-" (string name)))
    `(do (def ,id () (catch ,@body))
         (= (tests* ',name) ,id))))

(def run-tests ()
  (= passed* 0
     failed* 0)
  (each (name f) tests*
    (let result (f)
      (when (isa result 'string)
        (prn (+ " " name " " result)))))
  (prn (+ " " passed* " passed, " failed* " failed")))

(define-test reader
  (let read quoted:read
    (test? nil (read ""))
    (test? nil (read "nil"))
    (test? 17 (read "17"))
    (test? 0.015 (read "1.5e-2"))
    ;(test? 15 (read "0xF"))
    ;(test? -15 (read "-0Xf"))
    ;(test? "0x" (read "0x"))
    ;(test? "-0X" (read "-0X"))
    ;(test? "-0Xg" (read "-0Xg"))
    (test? t (read "t"))
    (test? (no t) (read "nil"))
    (test? 'hi (read "hi"))
    (test? '"hi" (read "\"hi\""))
    (test? 'hi (read "|hi|"))
    (test? '(1 2) (read "(1 2)"))
    (test? '(1 (a)) (read "(1 (a))"))
    (test? '(quote a) (read "'a"))
    (test? '(quasiquote a) (read "`a"))
    (test? '(quasiquote (unquote a)) (read "`,a"))
    (test? '(quasiquote (unquote-splicing a)) (read "`,@@a"))
    ;(test? 2 (# (read "(1 2 a: 7)")))
    ;(test? 7 (get (read "(1 2 a: 7)") 'a))
    ;(test? t (get (read "(:a)") 'a))
    (test? 1 (- -1))
    (test? '0? (read "0?"))
    (test? '0! (read "0!"))
    (test? '0. (read "0."))))

;(define-test read-more
;  (let read (get reader 'read-string)
;    (test? 17 (read "17" true))
;    (let more ()
;      (test? more (read "(open" more))
;      (test? more (read "\"unterminated " more))
;      (test? more (read "|identifier" more))
;      (test? more (read "'(a b c" more))
;      (test? more (read "`(a b c" more))
;      (test? more (read "`(a b ,(z" more))
;      (test? more (read "`\"biz" more))
;      (test? more (read "'\"boz" more)))
;    (let ((ok e) (guard (read "(open")))
;      (test? false ok)
;      (test? "Expected ) at 5" (get e 'message)))))

;(define-test nil?
;  (test? true (nil? nil))
;  (test? true (nil? null))
;  (test? false (nil? true))
;  (test? false (nil? false))
;  (test? false (nil? (obj))))

;(define-test is?
;  (test? false (is? nil))
;  (test? false (is? null))
;  (test? true (is? true))
;  (test? true (is? false))
;  (test? true (is? (obj))))

(define-test no
  (test? true (no nil))
  ;(test? true (no null))
  (test? false (no true))
  (test? true (no false))
  (test? false (no (obj)))
  (test? false (no 0)))

(define-test yes
  (test? false (yes nil))
  ;(test? false (yes null))
  (test? true (yes true))
  (test? false (yes false))
  (test? true (yes (obj)))
  (test? true (yes 0)))

;(define-test boolean
;  (test? true (or true false))
;  (test? false (or false false))
;  (test? true (or false false true))
;  (test? true (not false))
;  (test? true (not (and false true)))
;  (test? false (not (or false true)))
;  (test? true (and true true))
;  (test? false (and true false))
;  (test? false (and true true false)))

;(define-test short
;  (test? true (or true (error 'bad)))
;  (test? false (and false (error 'bad)))
;  (let a true
;    (test? true (or true (do (= a false) false)))
;    (test? true a)
;    (test? false (and false (do (= a false) true)))
;    (test? true a))
;  (let b true
;    (test? true (or (do (= b false) false) (do (= b true) b)))
;    (test? true b)
;    (test? true (or (do (= b true) b) (do (= b true) b)))
;    (test? true b)
;    (test? true (and (do (= b false) true) (do (= b true) b)))
;    (test? true b)
;    (test? false (and (do (= b false) b) (do (= b true) b)))
;    (test? false b)))

(define-test numeric
  (test? 4 (+ 2 2))
  (test? 0 (apply * '(0 0)))
  (test? 4 (apply + '(2 2)))
  (test? 0 (apply + ()))
  (test? 18 18.00)
  (test? 4 (- 7 3))
  (test? 4 (apply - '(7 3)))
  ;(test? 0 (apply - ()))
  (test? 5 (/ 10 2))
  (test? 5 (apply / '(10 2)))
  ;(test? 1 (apply / ()))
  (test? 6.0 (* 2 3.00))
  (test? 6.0 (apply * '(2 3.00)))
  ;(test? 1 (apply * ()))
  (test? true (> 2.01 2))
  (test? true (>= 5.0 5.0))
  (test? true (> 2.1e3 2000))
  (test? true (< 2e-3 0.0021))
  (test? false (< 2 2))
  (test? true (<= 2 2))
  (test? -7 (- 7))
  ;(test? false (numeric ""))
  )

(define-test math
  (test? 3 (max 1 3))
  (test? 2 (min 2 7))
  (let n (rand)
    (test? true (and (> n 0) (< n 1))))
  (test? 4 (trunc 4.78)))

(define-test precedence
  (test? -3 (- (+ 1 2)))
  (test? 10 (- 12 (+ 1 1)))
  (test? 11 (- 12 (* 1 1)))
  (test? 10 (+ (/ 4 2) 8)))

(define-test infix
  (withs (l '(1 1 2 3)
          (a b c d) l)
    (test? true (apply <= l))
    (test? false (apply < l))
    (test? false (apply is l))
    (test? true ((do is) 1 a b))
    (test? false (apply > (rev l)))
    (test? true (apply >= (rev l)))
    (test? true (<= a b c d))
    (test? true (<= a b c d))
    (test? false (< a b c d))
    (test? false (is a b c d))
    (test? true (is 1 a b))
    (test? false (> d c b a))
    (test? true (>= d c b a))))

;(define-test standalone
;  (test? 10 (do (+ illegal) 10))
;  (let x nil
;    (test? 9 (do (list nothing fooey (= x 10)) 9))
;    (test? 10 x))
;  (test? 12 (do (get but zz) 12))
;  (let y nil
;    (let ignore (do (%literal y | = 10;|) 42)
;      (test? 10 y))))

;(define-test string
;  (test? 3 (# "foo"))
;  (test? 3 (# "\"a\""))
;  (test? 'a "a")
;  (test? "a" (char "bar" 1))
;  (let s "a
;b"
;    (test? 3 (# s)))
;  (let s "a
;b
;c"
;    (test? 5 (# s)))
;  (test? 3 (# "a\nb"))
;  (test? 3 (# "a\\b"))
;  (test? "x3" (cat "x" (+ 1 2))))

(define-test atstrings
  (let a 'foo
    (test? "foobar" "@{a}bar")))

(define-test quote
  (test? 7 (quote 7))
  (test? true (quote t))
  (test? false (quote nil))
  (test? (quote a) 'a)
  (test? (quote (quote a)) ''a)
  (test? "a" '"a")
  (test? "\n" (quote "\n"))
  (test? "\r\n" (quote "\r\n"))
  (test? "\\" (quote "\\"))
  (test? '(quote "a") ''"a")
  (test? '|(| '|(|)
  (test? (quote unquote) 'unquote)
  (test? (quote (unquote)) '(unquote))
  (test? (quote (unquote a)) '(unquote a))
  ;(let x '(10 20 a: 33 1a: 44)
  ;  (test? 20 (at x 1))
  ;  (test? 33 (get x 'a))
  ;  (test? 44 (get x '1a)))
  )

(define-test list
  (test? '() (list))
  (test? () (list))
  (test? '(a) (list 'a))
  (test? '(a) (quote (a)))
  (test? '(()) (list (list)))
  (test? 0 (len (list)))
  (test? 2 (len (list 1 2)))
  (test? '(1 2 3) (list 1 2 3))
  ;(test? 17 (get (list foo: 17) 'foo))
  ;(test? 17 (get (list 1 foo: 17) 'foo))
  ;(test? true (get (list :foo) 'foo))
  ;(test? true (get '(:foo) 'foo))
  ;(test? true (get (hd '((:foo))) 'foo))
  ;(test? '(:a) (list :a))
  ;(test? '(b: false) (list b: false))
  ;(test? '(c: 0) (list c: 0))
  )

(define-test quasiquote
  (test? (quote a) (quasiquote a))
  (test? 'a `a)
  (test? () `())
  (test? 2 `,2)
  (test? nil `(,@nil))
  (let a 42
    (test? 42 `,a)
    (test? 42 (quasiquote (unquote a)))
    (test? '(quasiquote (unquote a)) ``,a)
    (test? '(quasiquote (unquote 42)) ``,,a)
    (test? '(quasiquote (quasiquote (unquote (unquote a)))) ```,,a)
    (test? '(quasiquote (quasiquote (unquote (unquote 42)))) ```,,,a)
    (test? '(a (quasiquote (b (unquote c)))) `(a `(b ,c)))
    (test? '(a (quasiquote (b (unquote 42)))) `(a `(b ,,a)))
    (let b 'c
      (test? '(quote c) `',b)
      (test? '(42) `(,a))
      (test? '((42)) `((,a)))
      (test? '(41 (42)) `(41 (,a)))))
  (let c '(1 2 3)
    (test? '((1 2 3)) `(,c))
    (test? '(1 2 3) `(,@c))
    (test? '(0 1 2 3) `(0 ,@c))
    (test? '(0 1 2 3 4) `(0 ,@c 4))
    (test? '(0 (1 2 3) 4) `(0 (,@c) 4))
    (test? '(1 2 3 1 2 3) `(,@c ,@c))
    (test? '((1 2 3) 1 2 3) `((,@c) ,@c)))
  (let a 42
    (test? '(quasiquote ((unquote-splicing (list a)))) ``(,@(list a)))
    (test? '(quasiquote ((unquote-splicing (list 42)))) ``(,@(list ,a))))
  ;(test? true (get `(:foo) 'foo))
  ;(let (a 17
  ;      b '(1 2)
  ;      c (obj a: 10)
  ;      d (list a: 10))
  ;  (test? 17 (get `(foo: ,a) 'foo))
  ;  (test? 2 (# `(foo: ,a ,@b)))
  ;  (test? 17 (get `(foo: ,@a) 'foo))
  ;  (test? '(1 a: 10) `(1 ,@c))
  ;  (test? '(1 a: 10) `(1 ,@d))
  ;  (test? true (get (hd `((:foo))) 'foo))
  ;  (test? true (get (hd `(,(list :foo))) 'foo))
  ;  (test? true (get `(,@(list :foo)) 'foo))
  ;  (test? true (get `(1 2 3 ,@'(:foo)) 'foo)))
  ;(let-macro ((a keys `(obj ,@keys)))
  ;  (test? true (get (a :foo) 'foo))
  ;  (test? 17 (get (a bar: 17) 'bar)))
  ;(let-macro ((a () `(obj baz: (fn () 17))))
  ;  (test? 17 ((get (a) 'baz))))
  )

(define-test quasiexpand
  (withs (x 'x z 'z)
    (test? 'a (macex 'a))
    (test? '(17) (macex '(17)))
    (test? '(1 z) (macex '(1 z)))
    (test? '(quasiquote (1 z)) (macex '`(1 z)))
    (test? '(quasiquote ((unquote 1) (unquote z))) (macex '`(,1 ,z)))
    (test? '(1 z) `(1 z))
    (test? '(1 z) `(,1 ,z))
    (let z '(z)
      (test? '(z) `(,@z)))
    ;(test? '(join (%array 1) z) (macex '`(,1 ,@z)))
    ;(test? '(join (%array 1) x y) (macex '`(,1 ,@x ,@y)))
    ;(test? '(join (%array 1) z (%array 2)) (macex '`(,1 ,@z ,2)))
    ;(test? '(join (%array 1) z (%array "a")) (macex '`(,1 ,@z a)))
    ;(test? '"x" (macex '`x))
    ;(test? '(%array "quasiquote" "x") (macex '``x))
    ;(test? '(%array "quasiquote" (%array "quasiquote" "x")) (macex '```x))
    ;(test? 'x (macex '`,x))
    ;(test? '(%array "quote" x) (macex '`',x))
    ;(test? '(%array "quasiquote" (%array "x")) (macex '``(x)))
    ;(test? '(%array "quasiquote" (%array "unquote" "a")) (macex '``,a))
    ;(test? '(%array "quasiquote" (%array (%array "unquote" "x")))
    ;       (macex '``(,x)))))
    ))

;(define-test calls
;  (let (f (fn () 42)
;        l (list f)
;        t (obj f: f))
;    (f)
;    ((fn ()
;      (test? 42 (f))))
;    (test? 42 ((at l 0)))
;    (test? 42 ((get t 'f)))
;    (test? nil ((fn () (return))))
;    (test? 10 ((fn (x) (- x 2)) 12))))

;(define-test id
;  (let (a 10
;        b (obj x: 20)
;        f (fn () 30))
;    (test? 10 a)
;    (test? 10 (%literal a))
;    (test? 20 (%literal b |.x|))
;    (test? 30 (%literal f |()|))))

(define-test names
  (withs (a! 0
          b? 1
          -% 2
          ** 3
          break 4)
    (test? 0 a!)
    (test? 1 b?)
    (test? 2 -%)
    (test? 3 **)
    (test? 4 break)))

(define-test =
  (test? 1 (= xx 1))
  (test? 1 xx)
  (test? 2 (= yy 1 zz 2))
  (test? 1 yy)
  (test? 2 zz)
  (let a 42
    (= a 'bar)
    (test? 'bar a)
    (let x (= a 10)
      (test? 10 x)
      (test? 10 a))
    (= a false)
    (test? false a)
    (= a)
    (test? nil a)))

;(define-test wipe
;  (let x '(:a :b :c)
;    (wipe (get x 'a))
;    (test? nil (get x 'a))
;    (test? true (get x 'b))
;    (wipe (get x 'c))
;    (test? nil (get x 'c))
;    (test? true (get x 'b))
;    (wipe (get x 'b))
;    (test? nil (get x 'b))
;    (test? () x)))

(define-test do
  (let a 17
    (do (= a 10)
        (test? 10 a))
    (test? 10 (do a))
    (let b (do (= a 2) (+ a 5))
      (test? a 2)
      (test? b 7))
    (do (= a 10)
        (do (= a 20)
            (test? 20 a)))
    (test? 20 (do (= a 10)
                  (do (= a 20) a))))
  (test? '(%do) (macex '(do))))

(define-test if
  (test? '(if a) (macex '(if a)))
  (test? '(if a b) (macex '(if a b)))
  (test? '(if a b c) (macex '(if a b c)))
  (test? '(if a b c d) (macex '(if a b c d)))
  (test? '(if a b c d e) (macex '(if a b c d e)))
  (if true
      (test? true true)
    (test? true false))
  (if false (test? true false)
      false (test? false true)
    (test? true true))
  (if false (test? true false)
      false (test? false true)
      false (test? false true)
    (test? true true))
  (if false (test? true false)
      true (test? true true)
      false (test? false true)
    (test? true true))
  (test? false (if false true false))
  (test? 1 (if true 1 2))
  (test? 1 (if (let a 10 a) 1 2))
  (test? 1 (if true (do1 1) 2))
  (test? 1 (if false 2 (let a 1 a)))
  (test? 1 (if false 2 true (do1 1)))
  (test? 1 (if false 2 false 3 (let a 1 a)))
  (test? 0 (if false 1 0)))

;(define-test case
;  (let x 10
;    (test? 2 (case x 9 9 10 2 4))
;    (test? 2 (case x 9 9 (10) 2 4))
;    (test? 2 (case x 9 9 (10 20) 2 4)))
;  (let x 'z
;    (test? 9 (case x z 9 10))
;    (test? 7 (case x a 1 b 2 7))
;    (test? 2 (case x a 1 (z) 2 7))
;    (test? 2 (case x a 1 (b z) 2 7)))
;  (let (n 0 f (fn () (++ n))) ; no multiple eval
;    (test? 'b (case (f) 0 'a 1 'b 'c)))
;  (test? 'b ((fn () (case 2 0 (do) 1 'a 2 'b)))))

(define-test while
  (let i 0
    (catch:while (< i 5)
      (if (is i 3) (throw) (++ i)))
    (test? 3 i)
    (while (< i 10)
      (++ i))
    (test? 10 i)
    (let a (while (< i 15) (++ i))
      (test? nil a)
      (test? 15 i))
    (let b
        (catch:while (< i 20)
          (if (is i 19)
              (throw)
            (++ i)))
      (test? nil b)
      (test? 19 i))))

;(define-test for
;  (let l ()
;    (for i 5
;      (add l i))
;    (test? '(0 1 2 3 4) l))
;  (test? '(0 1) (withs l () (for i 2 (add l i))))
;  (let (n 0 l '(a b c d e))
;    (for i (# l)
;      (++ n i)
;      (= l '(a b c)))
;    (test? 3 n)))

;(define-test table
;  (test? 10 (get (obj a: 10) 'a))
;  (test? true (get (obj :a) 'a)))

;(define-test empty
;  (test? true (empty? ()))
;  (test? true (empty? (obj)))
;  (test? false (empty? '(1)))
;  (test? false (empty? '(:a)))
;  (test? false (empty? (obj :a)))
;  (test? false (empty? '(b: false))))

;(define-test at
;  (let l '(a b c d)
;    (test? 'a (at l 0))
;    (test? 'b (at l 1))
;    (= (at l 0) 9)
;    (test? 9 (at l 0))
;    (= (at l 3) 10)
;    (test? 10 (at l 3))))

;(define-test get-set
;  (let t (obj)
;    (= (get t 'foo) 'bar)
;    (test? 'bar (get t 'foo))
;    (test? 'bar (get t "foo"))
;    (let k 'foo
;      (test? 'bar (get t k)))
;    (test? 'bar (get t (cat "f" "oo"))))
;  (let (t1 (obj) t2 (obj))
;    (= (get (or nil t1 t2) 'foo) 'bar)
;    (test? 'bar (get t1 'foo))
;    (test? nil (get t2 'foo))))

;(define-test each
;  (let t '(1 2 3 :a b: false)
;    (let (a 0 b 0)
;      (each (k v) t
;        (if (number? k)
;            (++ a)
;          (++ b)))
;      (test? 3 a)
;      (test? 2 b))
;    (let a 0
;      (each x t (++ a))
;      (test? 5 a)))
;  (let t '((1) (2) b: (3))
;    (each x t
;      (test? false (atom? x)))
;    (each (x) t
;      (test? false (atom? x)))
;    (each ((x)) t
;      (test? true (number? x)))))

;(define-test step
;  (let n 0
;    (step x '(1 2 3) (++ n x))
;    (test? 6 n))
;  (let n 0
;    (step (x y) '((1 2) (3 4))
;      (++ n (+ x y)))
;    (test? 10 n))
;  (let xs ()
;    (let l (list (obj a: 1 b: 2) (obj a: 2 b: 4))
;      (step (:a :b) l
;        (add xs (+ a b))))
;    (test? '(3 6) xs))
;  (let l '(a b)
;    (step x l
;      (if (is x 'a) (add l 'c)
;          (is x 'c) (add l 'd)))
;    (test? '(a b c d) l)))

(define-test ++
  (let x 2 (++ x) (test? 3 x))
  (let x 2 (test? 3 (++ x)))
  (let x 2 (test? 4 (++ x 2)))
  (let x 2 (test? 2 (++ x 0))))

(define-test --
  (let x 2 (-- x) (test? 1 x))
  (let x 2 (test? 1 (-- x)))
  (let x 4 (test? 2 (-- x 2)))
  (let x 2 (test? 2 (-- x 0))))

(define-test fn
  (let f (fn (n) (+ n 10))
    (test? 20 (f 10))
    (test? 30 (f 20))
    (test? 40 ((fn (n) (+ n 10)) 30))
    (test? '(2 3 4) (map (fn (x) (+ x 1)) '(1 2 3)))))

;(define-test define
;  (define x 20)
;  (define f () 42)
;  (test? 20 x)
;  (test? 42 (f))
;  ((fn ()
;     (define f () 38)
;     (test? 38 (f))))
;  (test? 42 (f)))

;(define-test return
;  (let a ((fn () 17))
;    (test? 17 a))
;  (let a ((fn () (if true 10 20)))
;    (test? 10 a))
;  (let a ((fn () (while false (blah))))
;    (test? nil a))
;  (let a 11
;    (let b ((fn () (++ a)))
;      (test? 12 b)
;      (test? 12 a)))
;  (test? 1 ((fn () (return (if true (return 1) 2))))))

;(define-test guard
;  (let-macro ((guard1 (x)
;                (let-unique (ok v)
;                  `(let ((,ok ,v) (guard ,x))
;                     (list ,ok (if ,ok ,v (get ,v 'message)))))))
;    (test? (list false "") (guard1 (error)))
;    (test? (list false "") (guard1 (error nil)))
;    (test? (list false "false") (guard1 (error false)))
;    (test? (list false "true") (guard1 (error true)))
;    (test? (list false "42") (guard1 (error 42)))
;    (test? '(true 42) (guard1 42))
;    (test? '(false foo) (guard1 (error "foo")))
;    (test? '(false foo) (guard1 (do (error "foo") (error "baz"))))
;    (test? '(false baz) (guard1 (do (guard1 (error "foo")) (error "baz"))))
;    (test? '(true 42) (guard1 (if true 42 (error "baz"))))
;    (test? '(false baz) (guard1 (if false 42 (error "baz"))))))

(define-test let
  (let a 10
    (test? 10 a))
  (withs (a 10)
    (test? 10 a))
  (withs (a 11
          b 12)
    (test? 11 a)
    (test? 12 b))
  (withs (a 1)
    (test? 1 a)
    (withs (a 2)
      (test? 2 a))
    (test? 1 a))
  (withs (a 1)
    (withs (a 2)
      (withs (a 3)
        (test? a 3))
      (test? a 2))
    (test? a 1))
  (withs (a 20)
    (test? 20 a)
    (withs (a (+ a 7))
      (test? 27 a))
    (withs (a (+ a 10))
      (test? 30 a))
    (test? 20 a))
  (test? 10 (withs (a 10) a))
  (withs (a (withs (b 12) b))
    (test? 12 a))
  (withs (a (withs (a 10) a))
    (test? 10 a))
  (withs (a (+ (withs (a 0)
                 (= a 10)
                 (+ a 2))
              3))
    (test? a 15))
  ((fn (zz)
     (test? 20 zz)
     (withs (zz 21)
       (test? 21 zz))
     (test? 20 zz))
   20)
  (let q 9
    ((fn ()
       (let q 10
         (test? 10 q))
       (test? 9 q))))
  (test? 0 (+ (abs -1)
              (let abs (fn (x) x)
                (abs -1)))))

;(define-test with
;  (test? 10 (withs (x 9) (++ x))))

(define-test whenlet
  (test? nil (whenlet frips (is 'a 'b) 19))
  (test? 19 (whenlet frips 20 (- frips 1)))
  (test? 20 (whenlet (a b) '(19 20) b))
  (test? nil (whenlet (a b) nil b))
  (test? 123 (whenlet a 0 (+ a 123))))

(= zzop 99
   zzap 100)

(withs (zzop 10
        zzap (+ zzop 10)
        (zza zzb) '(1 2 3 a: 10 b: 20))
  (define-test let-toplevel1
    (test? 10 zzop)
    (test? 20 zzap)
    (test? 1 zza)
    (test? 2 zzb)))

(define-test let-toplevel
  (test? 99 zzop)
  (test? 100 zzap))

(define-test reserved
  (withs (end 'zz
          try 'yy
          return 99)
    (test? 'zz end)
    (test? 'yy try)
    (test? '99 return))
  ;(define var (if end return)
  ;  (return (+ if end return)))
  ;(test? 6 (var 1 2 3))
  ;(define 1+ (x)
  ;  (+ x 1))
  ;(test? 6 (1+ 5)))
  )

(define-test destructuring
  (let (a b c) '(1 2 3)
    (test? 1 a)
    (test? 2 b)
    (test? 3 c))
  (let (w (x (y) z)) '(1 (2 (3) 4))
    (test? 1 w)
    (test? 2 x)
    (test? 3 y)
    (test? 4 z))
  (let (a b . c) '(1 2 3 4)
    (test? '(3 4) c))
  (let (w (x . y) . z) '(1 (2 3 4) 5 6 7)
    (test? '(3 4) y)
    (test? '(5 6 7) z))
  ;(let ((:foo) (obj foo: 99))
  ;  (test? 99 foo))
  ;(let ((:foo) (list foo: 99))
  ;  (test? 99 foo))
  ;(let ((foo: a) (obj foo: 99))
  ;  (test? 99 a))
  ;(let ((foo: (a b)) (obj foo: '(98 99)))
  ;  (test? 98 a)
  ;  (test? 99 b))
  ;(let ((:foo bar: (:baz))
  ;      (obj foo: 42 bar: '(99 :baz)))
  ;  (test? 42 foo)
  ;  (test? true baz))
  ;(let ((a (b :foo) :bar)
  ;      (list 10 (list 20 foo: 17) bar: '(1 2 3)))
  ;  (test? 10 a)
  ;  (test? 20 b)
  ;  (test? 17 foo)
  ;  (test? '(1 2 3) bar))
  (let yy (list 1 2 3)
    (let (xx yy . zz) yy
      (test? 1 xx)
      (test? 2 yy)
      (test? '(3) zz))))

;(define-test let-macro
;  (let-macro ((a () 17)
;              (b (a) `(+ ,a 10)))
;    (test? 17 (a))
;    (test? 42 (b 32))
;    (let-macro ((a () 1))
;      (test? 1 (a)))
;    (test? 17 (a)))
;  (let-macro ((a () 18))
;    (let (b (fn () 20))
;      (test? 18 (a))
;      (test? 20 (b))))
;  (let-macro ((a (x)
;                (let (x 10)
;                  (= x 20))
;                `(+ ,x 1)))
;    (test? 2 (a 1))))

;(define-test let-symbol
;  (let-symbol (a 17
;               b (+ 10 7))
;    (test? 17 a)
;    (test? 17 b)
;    (let-symbol (a 1)
;      (test? 1 a))
;    (test? 17 a))
;  (let-symbol (a 18)
;    (let (b 20)
;      (test? 18 a)
;      (test? 20 b))))

;(define-test define-symbol
;  (define-symbol zzz 42)
;  (test? zzz 42))

;(define-test macros-and-symbols
;  (let-symbol (a 1)
;    (let-macro ((a () 2))
;      (test? 2 (a)))
;    (test? 1 a))
;  (let-macro ((a () 2))
;    (let-symbol (a 1)
;      (test? 1 a))
;    (test? 2 (a))))

;(define-test macros-and-let
;  (let a 10
;    (test? a 10)
;    (let-macro ((a () 12))
;      (test? 12 (a)))
;    (test? a 10))
;  (let b 20
;    (test? b 20)
;    (let-symbol (b 22)
;      (test? 22 b))
;    (test? b 20))
;  (let-macro ((c () 30))
;    (test? 30 (c))
;    (let c 32
;      (test? 32 c))
;    (test? 30 (c)))
;  (let-symbol (d 40)
;    (test? 40 d)
;    (let d 42
;      (test? 42 d))
;    (test? 40 d)))

;(define-test let-unique
;  (let-unique (ham chap)
;    (test? '__ham2 ham)
;    (test? '__chap1 chap)
;    (let-unique (ham)
;      (test? '__ham3 ham))))

(define-test literals
  (test? true true)
  (test? false false)
  ;(test? true (< -inf -1e10))
  ;(test? false (< inf -1e10))
  ;(test? true (is nan nan))
  ;(test? true (nan? nan))
  ;(test? true (nan? (- nan)))
  ;(test? true (nan? (* nan 20)))
  ;(test? -inf (- inf))
  ;(test? inf (- -inf))
  (withs (Inf 1 NaN 2 -Inf 'a -NaN 'b)
    (test? Inf 1)
    (test? NaN 2)
    (test? -Inf 'a)
    (test? -NaN 'b)))

;(define-test add
;  (let l ()
;    (add l 'a)
;    (add l 'b)
;    (add l 'c)
;    (test? '(a b c) l)
;    (test? nil (add () 'a))))

;(define-test drop
;  (let l '(a b c)
;    (test? 'c (drop l))
;    (test? 'b (drop l))
;    (test? 'a (drop l))
;    (test? nil (drop l))))

;(define-test last
;  (test? 3 (last '(1 2 3)))
;  (test? nil (last ()))
;  (test? 'c (last '(a b c))))

;(define-test join
;  (test? '(1 2 3) (join '(1 2) '(3)))
;  (test? '(1 2) (join () '(1 2)))
;  (test? () (join () ()))
;  (test? () (join nil nil))
;  (test? () (join nil ()))
;  (test? () (join))
;  (test? () (join ()))
;  (test? '(1) (join '(1) nil))
;  (test? '(a) (join '(a) ()))
;  (test? '(a) (join nil '(a)))
;  (test? '(a) (join '(a)))
;  (test? '(a :b) (join '(a) (obj :b)))
;  (test? '(a b :b) (join '(a) '(b :b)))
;  (test? '(a b: 10) (join '(a :b) (obj b: 10)))
;  (test? '(b: 10) (join (obj :b) '(b: 10)))
;  (let t (join '(a b: 1) '(b c: 2))
;    (test? 1 (get t 'b))
;    (test? 2 (get t 'c))
;    (test? 'b (at t 1))))

;(define-test reverse
;  (test? () (reverse ()))
;  (test? '(3 2 1) (reverse '(1 2 3)))
;  (test? '(3 2 1 :a) (reverse '(1 2 3 :a))))

(define-test map
  (test? () (map (fn (x) x) ()))
  (test? '(1) (map (fn (x) x) '(1)))
  (test? '(2 3 4) (map (fn (x) (+ x 1)) '(1 2 3)))
  ;(test? '(2 3 4 a: 5) (map (fn (x) (+ x 1)) (list 1 2 3 a: 4)))
  ;(test? '(:a) (map (fn (x) x) '(:a)))
  ;(test? '(b: false) (map (fn (x) x) '(b: false)))
  ;(test? '(:a b: false) (map (fn (x) x) '(:a b: false)))
  (let evens (fn (x) (when (is (mod x 2) 0) x))
    (test? '(nil 2 nil 4 nil 6) (map evens '(1 2 3 4 5 6)))
    (test? '(2 4 6) (rem nil (map evens '(1 2 3 4 5 6))))
    ;(test? '(2 4 6 b: 8) (map evens '(1 2 3 4 5 6 a: 7 b: 8)))
    ))

(define-test cut
  (test? '() (cut '()))
  (test? '(a) (cut '(a)))
  (test? '(b c) (cut '(a b c) 1))
  (test? '(b c) (cut '(a b c d) 1 3))
  (test? '(1 2 3) (cut '(1 2 3) 0 10))
  (test? '(2) (cut '(1 2 3) 1 -1))
  ;(test? '(1) (cut '(1 2 3) -4 1))
  ;(test? '(1 2 3) (cut '(1 2 3) -4))
  ;(test? '(2 :a) (cut '(1 2 :a) 1))
  ;(test? '(:a b: 2) (cut '(:a b: 2)))
  (let x '(1 2 3)
    (test? '() (cut x (len x))))
  ;(let x '(1 2 3 :a)
  ;  (test? '(:a) (cut x (# x))))
  )

;(define-test clip
;  (test? "uux" (clip "quux" 1))
;  (test? "uu" (clip "quux" 1 3))
;  (test? "" (clip "quux" 5))
;  (test? "ab" (clip "ab" 0 4))
;  (test? "ab" (clip "ab" -4 4))
;  (test? "a" (clip "ab" -1 1)))

;(define-test search
;  (test? nil (search "" "a"))
;  (test? 0 (search "" ""))
;  (test? 0 (search "a" ""))
;  (test? 0 (search "abc" "a"))
;  (test? 2 (search "abcd" "cd"))
;  (test? nil (search "abcd" "ce"))
;  (test? nil (search "abc" "z")))

;(define-test split
;  (test? () (split "" ""))
;  (test? () (split "" ","))
;  (test? (list "a") (split "a" ","))
;  (test? (list "a" "") (split "a," ","))
;  (test? (list "a" "b") (split "a,b" ","))
;  (test? (list "a" "b" "") (split "a,b," ","))
;  (test? (list "a" "b") (split "azzb" "zz"))
;  (test? (list "a" "b" "") (split "azzbzz" "zz")))

;(define-test reduce
;  (test? 'a (either (reduce (fn (a b) (+ a b)) '()) 'a))
;  (test? 'a (either (reduce (fn (a b) (+ a b)) '(a)) 'a))
;  (test? 6 (reduce (fn (a b) (+ a b)) '(1 2 3)))
;  (test? '(1 (2 3))
;         (reduce
;          (fn (a b) (list a b))
;          '(1 2 3)))
;  (test? '(1 2 3 4 5)
;         (reduce
;          (fn (a b) (join a b))
;          '((1) (2 3) (4 5)))))

(define-test keep
  (test? () (keep (fn (x) x) ()))
  (test? '(0 1 2) (keep (fn (x) x) '(0 1 2)))
  (test? '((1) (2 3)) (keep ~empty '(() (1) () (2 3))))
  (let evens (fn (x) (is (mod x 2) 0))
    (test? '(6) (keep evens '(5 6 7)))
    (test? '(2 4 6) (keep evens '(1 2 3 4 5 6)))
    ;(test? '(2 4 6 b: 8) (keep evens '(1 2 3 4 5 6 a: 7 b: 8)))
    ))

(define-test mem
  (test? '(x y z) (mem 'x '(x y z)))
  (test? '(7) (mem 7 '(5 6 7)))
  (test? nil (mem 'baz '(no can do))))

(define-test find
  (test? nil (find (fn (x) x) ()))
  (test? 7 (find (fn (x) x) '(7)))
  (test? 7 (find (fn (x) (is x 7)) '(2 4 7)))
  ;(test? true (find (fn (x) (is x 7)) '(2 4 foo: 7)))
  ;(test? true (find (fn (x) (is x true)) '(2 4 :bar)))
  (test? 7 (find 7 '(2 4 7)))
  ;(test? true (mem 7 '(2 4 foo: 7)))
  ;(test? true (mem true '(2 4 :bar)))
  )

;(define-test first
;  (test? nil (first (fn (x) x) ()))
;  (test? 7 (first (fn (x) x) '(7)))
;  (test? true (first (fn (x) (is x 7)) '(2 4 7)))
;  (test? 4 (first (fn (x) (and (> x 3) x)) '(1 2 3 4 5 6))))

;(define-test sort
;  (test? '(a b c) (sort '(c a b)))
;  (test? '(3 2 1) (sort '(1 2 3) >)))

;(define-test type
;  (test? true (string? "abc"))
;  (test? false (string? 17))
;  (test? false (string? '(a)))
;  (test? false (string? true))
;  (test? false (string? (obj)))
;  (test? false (number? "abc"))
;  (test? true (number? 17))
;  (test? false (number? '(a)))
;  (test? false (number? true))
;  (test? false (number? (obj)))
;  (test? false (boolean? "abc"))
;  (test? false (boolean? 17))
;  (test? false (boolean? '(a)))
;  (test? true (boolean? true))
;  (test? false (boolean? (obj)))
;  (test? true (atom? nil))
;  (test? true (atom? "abc"))
;  (test? true (atom? 42))
;  (test? true (atom? true))
;  (test? false (atom? (fn ())))
;  (test? false (atom? '(1)))
;  (test? false (atom? (obj)))
;  (test? true (obj? (obj a: 10)))
;  (test? false (obj? null))
;  (test? false (obj? 1))
;  (test? false (obj? 'zz))
;  (test? false (obj? (fn ()))))

;(define-test str
;  (define f (x) x)
;  (let (l () l2 ())
;    (= (get l f) true)
;    (= (get l2 (target js: (str l) lua: l)) true)
;    (let k (target js: f lua: 'function)
;      (test? (cat "(" k ": true)") (str l))
;      (test? (cat "((" k ": true): true)") (str l2)))))

(define-test apply
  (test? 4 (apply (fn (a b) (+ a b)) '(2 2)))
  (test? '(2 2) (apply (fn a a) '(2 2)))
  ;(let t '(1)
  ;  (= (get t 'foo) 17)
  ;  (test? 17 (apply (fn a (get a 'foo)) t)))
  ;(test? 42 (apply (fn (:foo) foo) (list foo: 42)))
  ;(test? 42 (apply (fn ((:foo)) foo) (list (list foo: 42))))
  (test? 116 (apply + 1 5 '(100 10))))

(define-test eval
  (test? 4 (eval '(+ 2 2)))
  (test? 5 (eval '(let a 3 (+ 2 a))))
  ;(test? 9 (eval '(do (define x 7) (+ x 2))))
  (test? 6 (eval '(apply + '(1 2 3)))))

;(define-test call
;  (let f (fn () 42)
;    (test? 42 (call f)))
;  (let fs (list (fn () 1) (fn () 10))
;    (test? '(1 10) (map call fs)))
;  (let f (fn (x y) (+ x y 1))
;    (test? 42 (call f 40 1))))

;(define-test parameters
;  (test? 42 ((fn ((a)) a) '(42)))
;  (let f (fn (a (b c)) (list a b c))
;    (test? '(1 2 3) (f 1 '(2 3))))
;  (let f (fn (a (b . c) . d) (list a b c d))
;    (test? '(1 2 (3 4) (5 6 7)) (f 1 '(2 3 4) 5 6 7)))
;  (let f (fn (a (b . c) . d) (list a b c d))
;    (test? '(1 2 (3 4) (5 6 7)) (apply f '(1 (2 3 4) 5 6 7))))
;  (test? '(3 4) ((fn (a b . c) c) 1 2 3 4))
;  (let f (fn (w (x . y) . z) (list y z))
;    (test? '((3 4) (5 6 7)) (f 1 '(2 3 4) 5 6 7)))
;  (test? 42 ((fn (:foo) foo) foo: 42))
;  (test? 42 (apply (fn (:foo) foo) '(foo: 42)))
;  (test? 42 ((fn ((:foo)) foo) (list foo: 42)))
;  (let f (fn (a bar: b (:foo)) (list a b foo))
;    (test? '(10 20 42) (f 10 bar: 20 (list foo: 42))))
;  (let f (fn (a bar: b (:foo)) (list a b foo))
;    (test? '(10 20 42) (apply f '(10 bar: 20 (foo: 42)))))
;  (test? 1 ((fn (a :b) (+ (or a 0) b)) b: 1))
;  (test? 1 (apply (fn (a :b) (+ (or a 0) b)) (list b: 1)))
;  (let f (fn args args)
;    (test? '(1 2 3) (f 1 2 3)))
;  (let f (fn args args)
;    (test? '(1 2 3) (apply f (list 1 2 3))))
;  (let l ()
;    (define f (:a) (add l a) a)
;    (define g (a b :c) (add l (list a b c)) c)
;    (test? 42 (f a: (g (f a: 10) (f a: 20) c: (f a: 42))))
;    (test? '(10 20 42 (10 20 42) 42) l))
;  (let l nil
;    (define f x (= l x))
;    (define g (a b) (+ a b))
;    (f (g 1 2 foo: 7))
;    (test? '(3) l))
;  (let l nil
;    (define f x (= l x) 10)
;    (define g (a b) (+ (f) a b))
;    (test? 13 (g 1 2 foo: 7))
;    (test? '() l))
;  (let l nil
;    (define f (x . ys) (= l ys))
;    (define g () nil)
;    (f 1 'x g)
;    (test? (list 'x g) l)))

(define-test unicode
  (test? "abc@@example.com" (coerce '(#\a #\b #\c #\@ #\e #\x #\a #\m #\p #\l #\e #\. #\c #\o #\m) 'string))
  (test? "abc@@example.com" (coerce '( 97  98  99  64 101 120  97 109 112 108 101  46  99 111 109) 'string))
  (test? "ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็ ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็ ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็" (coerce (coerce "ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็ ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็ ด้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็็้้้้้้้้็็็็็้้้้้็็็็" 'bytes 'utf8) 'string 'utf8))
  (test? "" (coerce (list) 'string)))

(define-test accum
  (test? '(1 (1 2) (1 2 3)) (accum a (a 1) (a 1 2) (a 1 2 3)))
  (test? '(1 (1 2) (1 2 3)) (accum a (a 1) (a 1 2) (a 1 2 3) (map a (a))))
  (test? '((1 (1 2) (1 2 3))) (accum a (a 1) (a 1 2) (a 1 2 3) (apply a (a))))
  (test? '(42) (accum a (a 1) (a 1 2) (a 1 2 3) (a) (a 42))))

(define-test pr
  (test? "abc" (tostring:pr 'a 'b 'c))
  (test? "a.b.c" (tostring:apply pr '(a b c) sep: "."))
  (test? "abc\n" (tostring:prn 'a 'b 'c))
  (test? "a b c" (tostring:prs 'a 'b 'c))
  (test? 'a (pr file: (outstring) 'a 'b 'c))
  (test? "a c" (tostring:prt 'a nil 'c sep: " "))
  (test? nil (prt file: (outstring) nil 'b 'c)))

run-tests


