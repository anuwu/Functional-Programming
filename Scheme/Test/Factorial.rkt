(define fact (lambda (n)
     (if (= n 0) 1
         (* n (fact (- n 1))))))

(define fact-tail (lambda (n acc)
    ( if (= n 0) acc
       (fact-tail (- n 1) (* n acc)))))

(define factWrap (lambda (num)
    (define tail (lambda (acc n)
      ( if (= n 0)
           acc
           (tail (* acc n) (- n 1)))))
      (tail 1 num)))
