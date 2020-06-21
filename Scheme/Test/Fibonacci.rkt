(define (fib num)
    (define (fib-tail acc1 acc2 n)
      (if (= n 0) acc2
          (fib-tail acc2 (+ acc1 acc2) (- n 1))))
    (fib-tail 0 1 num))
