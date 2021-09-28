#lang racket/load

(compile-allow-set!-undefined #t)
(compile-enforce-module-constants #f)

(load "ac.scm")
(require 'ac)

(load "brackets.scm")
(require 'brackets)

(use-arc-readtable)

(aload "arc.arc")
(aload "libs.arc") 

