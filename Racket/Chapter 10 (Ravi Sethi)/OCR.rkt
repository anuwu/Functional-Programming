; Returns only the repeating elements in a list.

; ------------------- Examples ----------------
; > (ocr '(a b a a a c c))
;   '(a c)
; > (ocr '(a b c d))
;   '()
; > (ocr '(a b a a a c))
;   '(a)
; ----------------------------------------------

(define (ocr lst)
    (define (ocr-help lst acc ch equal)
      (cond ((null? lst) (reverse acc))
            ((eq? ch (car lst)) (if (= equal '1) (ocr-help (cdr lst) acc ch '1)
                                    (ocr-help (cdr lst) (cons ch acc) ch '1)))
            (else (ocr-help (cdr lst) acc (car lst) '0))
      )
    )
   (if (null? lst) '() (ocr-help (cdr lst) '() (car lst) '0))
 )
