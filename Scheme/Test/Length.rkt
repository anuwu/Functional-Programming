(define (my-length lst)
    (define (length-tail lst count)
      (if (null? lst) count
          (length-tail (cdr lst) (+ count 1))))
      (length-tail lst 0))

(define (my-length1 lst)
	( if (null? lst) 0 (+ 1 (my-length1 (cdr lst)))))

