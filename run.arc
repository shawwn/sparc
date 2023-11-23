#!/usr/bin/env arc
; Run arc expressions.  1 Jan 23.
;
; Arguments are Arc expressions:
;
;  $ ./run.arc 1 2
;  1
;  2
;
;  $ ./run.arc '(+ "foo" "bar")'
;  foobar
;
; Each value is bound to "that", letting you refer to previous vals:
;
;  $ ./run.arc 1 '(+ 1 that)' '(* 2 that)'
;  1
;  2
;  4
;
; You can omit the outermost parens in each expression:
;
;  $ ./run.arc 1 '+ 1 that' '* 2 that'
;  1
;  2
;  4
;
;  $ ./run.arc 'range 1 10' 'apply + that'
;  (1 2 3 4 5 6 7 8 9)
;  45
;
; Nil values won't be printed:
;
;  $ ./run.arc 1 nil 2 ''
;  1
;  2
;
; You can call racket functions:
;
;  $ ./run.arc 'string-prefix? "foo" "f"'
;  #t
;
; Compile arc.arc to scheme, then open the compiled code in vim:
;
;  $ ./run.arc 'let it "arc.arc" (acompile it) (filechars (+ it ".scm"))' | vim -
;
; Measure startup time:
;
;  $ time ./run.arc
;  ./run.arc  2.81s user 0.09s system 94% cpu 3.066 total

(def readarg (arg)
  (aand (readall:string arg)
        (if (cdr it) it (car it))))

(def run args
  (on arg args
    (unless (is index 0) (prn))
    (= that (eval:readarg arg))
    (if that (pr that))))

run
