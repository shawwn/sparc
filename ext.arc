(mac w/env body
  `#`(parameterize ((boxed* #,(lexenv))
                    (env* (map car #,(lexenv))))
       #,,@body))

(def try-require (lib)
  (safe:seval `(begin (require ,lib) ',lib)))

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
