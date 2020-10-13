; Returns the number of times an element occurs consecutively.
; ------------ Examples -------------------
; > (cntr '(a b a a a c c))
;   '((1 a) (1 b) (3 a) (2 c))
; > (cntr '(a))
;   '((1 a))
; > (cntr '(a a))
;   '((2 a))
; > (cntr '(a a b))
;   '((2 a) (1 b))
; -----------------------------------------
(define (cntr lst)
    (define (cntr-help lst acc ch count)
      (cond ((null? lst) (reverse (cons (list (+ count 1) ch) acc)))
            ((eq? ch (car lst)) (cntr-help (cdr lst) acc ch (+ count 1)))
            (else (cntr-help (cdr lst) (cons (list (+ count 1) ch) acc) (car lst) '0))
    ))
    (if (null? lst) '() (cntr-help (cdr lst) '() (car lst) '0))
  )
