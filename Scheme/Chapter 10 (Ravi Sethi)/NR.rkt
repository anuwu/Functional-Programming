#lang racket
; Returns the elements of a list that are non-repeating
; ---------------------- Examples ----------------------
; > (nr '(a b c d))
;   '(a b c d)
; > (nr '(a))
;   '(a)
; > (nr '(a b b b b b))
;   '(a)
; > (nr '(a b c d e a b b c e e f a a))
;   '(a b c d e a c f)
; > (nr '(a b b c))
;   '(a c)
; -----------------------------------------------------
(define (nr lst)
    (define (nr-help lst acc ch equal)
      (cond
        ((null? lst) (if (= equal '1) (reverse acc) (reverse (cons ch acc))))
        ((eq? (car lst) ch) (nr-help (cdr lst) acc ch '1))
        (else (if (= equal '1) (nr-help (cdr lst) acc (car lst) '0)
                   (nr-help (cdr lst) (cons ch acc) (car lst) '0)
              ))
      )
     )
    (if (null? lst) '() (nr-help (cdr lst) '() (car lst) '0))
 )