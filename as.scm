#lang racket/load
; racket -f as.scm
; (asv)
; http://localhost:8080

(require racket/runtime-path)
(define-runtime-path arc-home ".")

(define arc-curdir (current-directory))
(current-directory arc-home)
(require "./arc.scm")
(current-directory arc-curdir)

(define (safe-read x)
  (arc-eval `(or (safe (coerce ,x 'num)) ,x)))

(define (arc-main . args)
  (let* ((repl (or (ar-car? args "-i")
                   (ar-car? args "--interact")))
         (args (if repl (cdr args) args))
         (name (ar-car? args))
         (args (if name (cdr args) args)))
    (current-command-line-arguments (list->vector args))
    (if name
        (let* ((x (arc-eval `(load ,name)))
               (args (map safe-read args)))
          (when (procedure? x) (apply x args))
          (when repl (interact)))
        (interact))))

(apply arc-main (vector->list (current-command-line-arguments)))
