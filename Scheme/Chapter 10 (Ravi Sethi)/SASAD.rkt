#lang racket
; Returns a list after trimming consecutive elements.

; ---------------- Examples -------------------
; > (sasad '(a b a a c d c d d e e f))
;   '(a b a c d c d e f)
; > (sasad '(a))
;   '(a)
; > (sasad '(a a))
;   '(a)
; > (sasad '())
;   '()
; ---------------------------------------------

(define (sasad lst)
    (define (sasad-help lst acc ch)
      (cond
        ((null? lst) (reverse (cons ch acc)))
        ((eq? (car lst) ch) (sasad-help (cdr lst) acc (car lst)))
        (else (sasad-help (cdr lst) (cons ch acc) (car lst)))
      ))

    (if (null? lst) '()
    (sasad-help (cdr lst) '() (car lst)))
  )