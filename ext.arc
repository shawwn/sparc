(mac w/env body
  `#`(parameterize ((boxed* #,(lexenv))
                    (env* (map car #,(lexenv))))
       #,,@body))

(def try-require (lib)
  (errsafe:seval `(begin (require ,lib) ',lib)))

;; Optional debugger
;;   raco pkg install debug

(when (try-require 'debug/repl)

  ;#'(xdef resume resume)

  (mac debug ()
    `(w/env
       #'(debug-repl)))

  (def debug-demo (x)
    (debug)
    (prn "x is now " x)))

(mac w/values body
 `(call-with-values (fn () ,@body) list))

;arc> (w/values (values 1 2 3))
;'(1 2 3)

(mac assert (test (o msg "Assertion failed") . args)
  `(or ,test
       (err (cat ,msg ":") ',test ,@args)))
