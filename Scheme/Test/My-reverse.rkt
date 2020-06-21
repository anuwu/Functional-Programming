; (my-reverse '(1 2 3 4 5))

(define (my-reverse lst)
    (define (reverse-tail l acc)
      (if (null? l) acc
      (reverse-tail (cdr l) (cons (car l) acc))))
    (reverse-tail lst '()))
