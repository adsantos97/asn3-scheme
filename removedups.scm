; remove dups

(define nil '())

(define (remove-dups l)
  (cond ((null? l) nil)
    (else (cons (car l) (remove-dups l)))
  )    
)


; two merger tests
(remove-dups '(a b c))
(remove-dups '(a b a a a c c))


;(define (count-dups l)
;)


; two merger tests
;(count-dups '(a b c))
;(count-dups '(a b a a a c c))
