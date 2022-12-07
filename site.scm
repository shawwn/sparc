#lang racket/load

(require racket/runtime-path)

(define-runtime-path arc-scm "./arc.scm")

(load arc-scm)
(require 'arc)

(arc-eval '((load:libpath "news.arc")))
