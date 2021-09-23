(mac w/env body
  `#`(parameterize ((boxed* #,(lexenv))
                    (env* (map car #,(lexenv))))
       #,,@body))

;; Optional debugger
;;   raco pkg install debug

(when (errsafe
        #'(require debug/repl)
        #'(xdef resume resume)
        t)

  (mac debug ()
    `(w/env
       #'(debug-repl)))

  (def debug-demo (x)
    (debug)
    (prn "x is now " x)))

