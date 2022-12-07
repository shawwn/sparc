#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(define-runtime-path ac-scm "ac.scm")
(define-runtime-path brackets-scm "brackets.scm")
(define-runtime-path arc-arc "arc.arc")

(load ac-scm)
(require 'ac)

(load brackets-scm)
(require 'brackets)
(use-arc-readtable)

(aload arc-arc)
(arc-eval '(load (libpath "libs.arc")))
