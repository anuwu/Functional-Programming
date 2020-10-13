(define (my-reverse lst)
    (define (reverse-tail l acc)
      (if (null? l) acc
      (reverse-tail (cdr l) (cons (car l) acc))))
    (reverse-tail lst '()))

(define my-append1 (lambda (lst num)
   (if (null? lst) (cons num '())
        (cons (car lst) (my-append1 (cdr lst) num)))))

(define (my-append2 lst num) (my-reverse (cons num (my-reverse lst))))

(define (my-appendList1 lst1 lst2)
    (if (null? lst1) lst2
    (cons (car lst1) (my-appendList1 (cdr lst1) lst2))))

(define (my-appendList2 lst1 lst2)
	(define (app-list-help revlst1 acc)
		(if (null? revlst1) acc
		    (app-list-help (cdr revlst1) (cons (car revlst1) acc))
		))
	(app-list-help (my-reverse lst1) lst2))
