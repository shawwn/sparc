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

; --- number literal parsing at read time ---
; This ensures that bare tokens like 0x42 or 1_000 are read as numbers,
; while pipe-quoted symbols like |0x42| or |1_000| remain symbols
; (Racket's |...| reader bypasses our character handlers entirely).

; Characters that terminate a token during manual scanning.
(define (arc-token-delim? c)
  (or (eof-object? c)
      (char-whitespace? c)
      (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\, #\' #\` #\;))))

; Read remaining token characters from port (initial char already consumed).
(define (arc-read-rest-of-token port)
  (let loop ((chars '()))
    (let ((c (peek-char port)))
      (if (arc-token-delim? c)
          (list->string (reverse chars))
          (begin (read-char port)
                 (loop (cons c chars)))))))

; string-prefix? and string-replace inlined to avoid racket/string dependency.
(define (arc-string-prefix? s prefix)
  (let ((plen (string-length prefix)))
    (and (>= (string-length s) plen)
         (string=? (substring s 0 plen) prefix))))

(define (arc-strip-underscores s)
  (list->string (filter (lambda (c) (not (char=? c #\_))) (string->list s))))

; Parse a non-negative number string, supporting 0x/0b/0o prefix and _ separators.
; Returns a number or #f.
(define (arc-parse-uint s)
  (and (not (arc-string-prefix? s "_"))
       (not (arc-string-prefix? s "+"))
       (not (arc-string-prefix? s "-"))
       (let ((s2 (arc-strip-underscores s)))
         (and (not (equal? s2 ""))
              (cond
                ((or (arc-string-prefix? s2 "0x") (arc-string-prefix? s2 "0X"))
                 (string->number (substring s2 2) 16))
                ((or (arc-string-prefix? s2 "0b") (arc-string-prefix? s2 "0B"))
                 (string->number (substring s2 2) 2))
                ((or (arc-string-prefix? s2 "0o") (arc-string-prefix? s2 "0O"))
                 (string->number (substring s2 2) 8))
                (else
                 (string->number s2)))))))

; Parse a number string with optional leading +/-.
; Returns a number or #f.
(define (arc-parse-number-string s)
  (cond
    ((arc-string-prefix? s "+") (arc-parse-uint (substring s 1)))
    ((arc-string-prefix? s "-")
     (let ((n (arc-parse-uint (substring s 1))))
       (and n (- n))))
    (else (arc-parse-uint s))))

; Readtable handler for digit and sign characters.
; Reads the full token and returns a number if it parses, otherwise a symbol.
(define (arc-read-number-or-symbol ch port src line col pos)
  (let* ((rest (arc-read-rest-of-token port))
         (s (string-append (string ch) rest))
         (n (arc-parse-number-string s)))
    (or n (string->symbol s))))

; a readtable that is just like the builtin except for []s and number literals

(define arc-readtable
  (make-readtable #f
    #\[ 'terminating-macro     arc-read-square-brackets
    #\{ 'terminating-macro     arc-read-curly-braces
    #\0 'non-terminating-macro arc-read-number-or-symbol
    #\1 'non-terminating-macro arc-read-number-or-symbol
    #\2 'non-terminating-macro arc-read-number-or-symbol
    #\3 'non-terminating-macro arc-read-number-or-symbol
    #\4 'non-terminating-macro arc-read-number-or-symbol
    #\5 'non-terminating-macro arc-read-number-or-symbol
    #\6 'non-terminating-macro arc-read-number-or-symbol
    #\7 'non-terminating-macro arc-read-number-or-symbol
    #\8 'non-terminating-macro arc-read-number-or-symbol
    #\9 'non-terminating-macro arc-read-number-or-symbol
    #\+ 'non-terminating-macro arc-read-number-or-symbol
    #\- 'non-terminating-macro arc-read-number-or-symbol))

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
