#lang racket/load
; racket -f as.scm
; (asv)
; http://localhost:8080

(require racket/runtime-path)

(define-runtime-path arc-scm "./arc.scm")

(load arc-scm)
(require 'arc)
(interact)

