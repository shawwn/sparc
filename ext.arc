(mac w/env body
  `#`(parameterize ((boxed* #,(lexenv))
                    (env* (map car #,(lexenv))))
       #,,@body))

;; Optional debugger
;;   raco pkg install debug

(when (errsafe:and #'(require debug/repl) t)
  #'(xdef resume resume)

  (mac debug ()
    `(w/env
       #'(debug-repl)))

  (def debug-demo (x)
    (debug)
    (prn "x is now " x)))

