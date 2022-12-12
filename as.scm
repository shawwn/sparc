#lang racket/load
; racket -f as.scm
; (asv)
; http://localhost:8080

(require racket/runtime-path)

(define-runtime-path arc-scm "./arc.scm")

(load arc-scm)
(require 'arc)
(define args (vector->list (current-command-line-arguments)))

(define (safe-read x)
  (arc-eval `(or (errsafe (coerce ,x 'num)) ,x)))

(if (> (length args) 0)
    (let* ((repl (or (car? args "-i")
                     (car? args "--interact")))
           (args (if repl (cdr args) args))
           (name (car args))
           (args (cdr args)))
      (current-command-line-arguments (list->vector args))
      (let* ((x (arc-eval `(load ,name)))
             (args (map safe-read args)))
        (when (procedure? x) (apply x args))
        (when repl (interact))))
    (interact))

