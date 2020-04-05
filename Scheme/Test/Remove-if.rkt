#lang racket
; (remove-if1 even? '(1 2 3 4))
(define (remove-if1 fn lst)
    (cond
      ((null? lst) '())
      ((fn (car lst)) (remove-if1 fn (cdr lst)))
      (else (cons (car lst) (remove-if1 fn (cdr lst))))
    )
)

(define (remove-if2 fn lst)
    (define (remif-tail fn lst acc)
      (cond
        ((null? lst) (reverse acc))
        ((fn (car lst)) (remif-tail fn (cdr lst) acc))
        (else (remif-tail fn (cdr lst) (cons (car lst) acc)))
      )
    )
    (remif-tail fn lst '())
)

