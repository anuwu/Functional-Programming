#lang racket
; Run as (my-map timesten '(1 2 3 4))

; --------- Helper definitions -----------
(define my-append1 (lambda (lst num)
   (if (null? lst) (cons num '())
        (cons (car lst) (my-append1 (cdr lst) num)))))

(define timesten (lambda (n) (* n 10)))
; ----------------------------------------

(define (my-map1 fn lst)
    (define (my-map1-tail fn l acc)
      (if (null? l) acc
      (my-map1-tail fn (cdr l) (my-append1 acc (fn (car l))))))
    (my-map1-tail fn lst '()))

(define (my-map2 fn lst)
    (define (my-map2-tail fn acc l)
      (if (null? l) acc
      (my-map2-tail fn (cons (fn (car l)) acc) (cdr l))))
    (reverse(my-map2-tail fn '() lst)))

(define (my-map3 fn list)
	(if (null? list) '()
	(cons (fn (car list)) (my-map3 fn (cdr list)))))