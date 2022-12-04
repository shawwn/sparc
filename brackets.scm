#lang racket/load
; From Eli Barzilay, eli@barzilay.org

;> (require "brackets.scm") 
;> (use-bracket-readtable) 
;> ([+ _ 1] 10) 
;11

; (module brackets racket/base
  
; main reader function for []s
; recursive read starts with default readtable's [ parser,
; but nested reads still use the curent readtable:

; (define (arc-read-square-brackets ch port src line col pos)
;   `(fn (_)
;      ,(read/recursive port #\[ #f)))

(define (arc-read-square-brackets ch port src line col pos)
  (let ((stx (read-syntax/recursive ch port #\[ #f)))
    (datum->syntax #f `(%brackets ,@(syntax-e stx)) stx)))

(define (arc-read-curly-braces ch port src line col pos)
  (let ((stx (read-syntax/recursive ch port #\{ #f)))
    (datum->syntax #f `(%braces ,@(syntax-e stx)) stx)))
  
; a readtable that is just like the builtin except for []s

(define arc-readtable
  (make-readtable #f #\[ 'terminating-macro arc-read-square-brackets
                     #\{ 'terminating-macro arc-read-curly-braces))
  
; call this to set the global readtable

; (provide use-arc-readtable)

(define (use-arc-readtable)
  (current-readtable arc-readtable))
  
; these two implement the required functionality for #reader
    
;(define (*read inp)
;  (parameterize ((current-readtable arc-readtable))
;    (read inp)))

(define (arc-read (port (current-input-port)))
  (parameterize ((current-readtable arc-readtable))
    (read port)))

(define (arc-read-syntax src (port (current-input-port)))
  (parameterize ((current-readtable arc-readtable))
    (read-syntax src port)))

; and the need to be provided as `read' and `read-syntax'

; (provide (rename *read read) (rename *read-syntax read-syntax))

; )

'brackets
