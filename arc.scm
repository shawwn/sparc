#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(require "./ac.scm")
(require "./brackets.scm")
(use-arc-readtable)

(void (aload "./arc.arc"))
(void (arc-eval '(load "./libs.arc")))
