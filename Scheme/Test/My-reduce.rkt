#lang racket
; Run as (my-reduce1 mul '(3 4 5 6) 1)
; or (my-reduce1 add '(1 2 3 4 5) 0)

(define (mul x y) (* x y))
(define (add x y) (+ x y))

(define (my-reduce1 fn lst acc)
    (if (null? lst) acc
        (my-reduce1 fn (cdr lst) (fn (car lst) acc))))

(define (my-reduce2 fn lst start)
	(if (null? lst) start (fn (car lst) (my-reduce2 fn (cdr lst) start))))


