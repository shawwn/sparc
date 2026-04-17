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

(mac w/stdout-lock body
  `(with-stdout-lock (fn () ,@body)))

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
(assign dbgexpr* nil)

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

(def dbg-slot ((k v))
  (list k (if (isa!fn v) (v) v)))

(def dbg-locals (lenv)
  (map dbg-slot lenv))

(def dbg-prn (lenv retexpr)
  (when (> (len exn*) 0)
    (tlerr (car exn*)))
  ;(iflet stack (stacktrace) ;(at (stacktrace) 0 (pos 'dbg-prn (stacktrace)))
  ;       (prn "debugging at: " (dbg-pps (nthcdr (+ (pos 'debugger stack) 1) stack))))
  (whenlet trace (assoc '*stacktrace lenv)
    (prn "debugging at: " (dbg-pps (dbg-slot trace))))
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

(def dbg-eval (e expr lenv)
  (let prev (dbg-copy e)
    (assign dbgenv* (dbg-copyenv))
    (dbg-restore e)
    (eval expr lenv)))

(def dbg-prexpr (e lenv expr (o printer) (o o (stdout)) (o i (stdin)))
  (with result (w/stdout o (w/stdin i (dbg-eval e expr lenv)))
    (if printer
      (printer expr result)
      (prnred (dbg-pps result)))
    (= thatexpr expr)
    (= that result)))

;(def dbgerr (c)
;  (prn:details c)
;  (map pp (stacktrace c))
;  (map pp (fulltrace c))
;  c)

(assign dbgerr debugger:tlerr)

(assign prnred prn)
(assign prnblue prn)

(def dbg-eval-fn (e lenv retexpr o i)
  (fn (expr)
    (w/stdout-lock
      (on-err tlerr
        (fn ()
          (assign dbgexpr* expr)
          (if (or (is expr ''c) (is expr eof))
              (do (dbg-prexpr e lenv retexpr
                              (fn (expr result)
                                (when expr
                                  (prnblue (dbg-pps expr))
                                  (pr "  returned ")
                                  (prnred (dbg-pps result))))
                              o i)
                  (repl-quit))
              (is expr ''v)
                (dbg-prexpr e lenv retexpr)
              (is expr ''h)
                (dbg-prn lenv retexpr)
              (do (prnred (dbg-pps (dbg-eval e expr lenv)))
                  (if (is dbgexpr* ''c)
                      (do (assign dbgexpr* nil)
                          (dbg-prn lenv retexpr))))))))))

(def debugger (lenv (o retexpr) (o o (stdout)) (o i (stdin)))
  (let e (dbg-copy (or dbgenv* *env))
    (dbg-restore e)
    (assign dbgenv* nil)
    (assign *debug nil)
    (when (is (type lenv) 'exception)
      ;(= lenv (lexenv lenv)))
      (= lenv (dbg-exnenv e lenv)))
    (when (is (type lenv) 'sym)
      (= lenv (e lenv)))
    (when (is (type lenv) 'fn)
      (= lenv (lenv)))
    (w/stdout-lock (dbg-prn lenv retexpr))
    (run-repl "dbg" "> " (dbg-eval-fn e lenv retexpr o i))))

(mac dbg ((o expr 'nil))
  `(debugger (lexenv) ',expr))

