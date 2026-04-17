; 
; example usage:
;   (mac xy (x) (dbg if (no x) 'nil `(dbg cons ,(car x) (xy ,(cdr x)))))
;   (xy (1 2 3 4))
;
; useful shorthand for experimenting with marks:
;   (scheval '(begin
;     (define w/immark call-with-immediate-continuation-mark)
;     (add-xdef 'w/immark w/immark)
;     (define marks->list continuation-mark-set->list)
;     (add-xdef 'marks->list marks->list)
;     (define curmarks current-continuation-marks)
;     (add-xdef 'curmarks curmarks)
;     (define (w/mark key val thunk) (with-continuation-mark key val (thunk)))
;     (add-xdef 'w/mark w/mark)
;     (define (marklist key) (marks->list (curmarks) key))
;     (add-xdef 'marklist marklist)))
;
;(w/mark 'arc 'xy (fn () (w/immark 'arc (fn (x) ([prn (list x _)] (marklist 'arc))))))
;(do (mark 'zz 'bar (def bar (n) (if (is n 0) (foo) (bar (- n 1))))) (mark 'zz 'foo (def foo () (marks 'zz))) (bar 2))

(require "colors.arc")

(= *debug nil)

(or= *env (obj))

(def *envwipe ()
  (each k (keys *env)
    (wipe (*env k))))

; `tlread` used to prompt and (read) directly from original-stdin. With
; the cooperative REPL stack in ac.scm, the dispatcher owns stdin and
; delivers parsed forms through a channel — nothing here reads the
; terminal any more.

(def tlerr (c)
  (display-error c)
  c)

(def dbg-stats ()
  (pp (sorted (map [list _!1 _!0] (tablist *fncounts)))))

(def dbg-wipestats ()
  (each k (keys *fncounts)
    (= (*fncounts k) nil)))

(def dbg-exnenv (env c)
  (each x (stacktrace c)
    (if (env x)
        (break (env x)))))

;(when (~bound 'exn*)
(assign exn* nil)

(def dbg-restore (tbl)
  (when tbl
    (*envwipe)
    (maptable (fn (k v) (sref *env v k)) tbl)
    *env))

(def dbg-copy (tbl)
  (with new (table)
    (maptable (fn (k v) (sref new v k)) tbl)))

(def dbg-copyenv ()
  (dbg-copy *env))

(def call-w/dbg (f)
  (if *debug
       (f)
       (let prev (dbg-copyenv)
         (*envwipe)
         (assign *debug t)
         (after
           ;(f)
           (on-err (fn (c) (let name (*fn)
                             (let e (dbg-copyenv)
                               (w/pushnew c exn* 
                                 (dbg-restore e)
                                 (debugger c)))))
                               ;(if (~posmatch "cannot reference undefined identifier" (details c))
                               ;  (debugger c)
                               ;  (tlerr c)))))
                   f)
           (do
             (dbg-restore prev)
             (assign *debug nil))))))

(def pprint (x)
  (#'pretty-print x)
  x)

(mac w/dbg body
  `(call-w/dbg (fn () ,@body)))

(def dbg-pps xs
  (apply string
    (intersperse #\space
      (map [trim:tostring:pprint _] xs))))

(def dbg-slot ((name getter))
  (list name (getter)))

(def dbg-locals (lenv)
  (map dbg-slot lenv))

(def dbg-prn (lenv retexpr)
  (when (> (len exn*) 0)
    (tlerr (car exn*)))
  (pr "locals:")
  (each (name val) (dbg-locals lenv)
    (prn)
    (prblue name)
    (pr " " (dbg-pps val)))
  (prn)
  (prn "type 'h to see this printout")
  (prn "type 'c to continue")
  (when retexpr
    (pr  "type 'v to see value of ") (prnblue (dbg-pps retexpr))))

(assign dbgenv* nil)

(def dbg-eval (locals expr lenv (o o (stdout)) (o i (stdin)))
  (let prev (dbg-copy locals)
    (assign dbgenv* (dbg-copyenv))
    (dbg-restore locals)
    (w/stdout o (w/stdin i (eval expr lenv)))))

(def dbg-prexpr (locals lenv expr o i)
  (with result (dbg-eval locals expr lenv o i)
    (prnred (dbg-pps result))
    (= that result thatexpr expr)))

;(def dbgerr (c)
;  (prn:details c)
;  (map pp (stacktrace c))
;  (map pp (fulltrace c))
;  c)

(assign dbgerr debugger:tlerr)

(def dbg-eval-fn (locals lenv retexpr o i)
  (fn (expr)
    (on-err tlerr
      (fn ()
        (case expr
          ('c eof)
           (repl-quit)
          ('v)
           (dbg-prexpr locals lenv retexpr o i)
          ('h)
           (dbg-prn lenv retexpr)
           (prnred (dbg-pps (dbg-eval locals expr lenv o i))))))))

(def debugger (lenv (o retexpr) (o o (stdout)) (o i (stdin)) (o :prompt "> "))
  (let locals (dbg-copy (or dbgenv* *env))
    (dbg-restore locals)
    (assign dbgenv* nil)
    (assign *debug nil)
    (when (is (type lenv) 'exception)
      (= lenv (dbg-exnenv locals lenv)))
    (when (is (type lenv) 'sym)
      (= lenv (locals lenv)))
    (when (is (type lenv) 'fn)
      (= lenv (lenv)))
    ; (dbg) fired from a request handler inherits current-output-port bound
    ; to the socket. run-repl rebinds to original-stdout* internally, but the
    ; preamble runs before that, so force it to the terminal here.
    (w/stdout original-stdout*
      (dbg-prn lenv retexpr))
    (run-repl "dbg" prompt (dbg-eval-fn locals lenv retexpr o i))
    (dbg-eval locals retexpr lenv o i)))

(mac dbg ((o expr 'nil) :kws)
  `(debugger (lexenv) ',expr ,@kws))

