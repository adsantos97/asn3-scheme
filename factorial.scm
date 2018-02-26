;;; This is my code
;;; Arizza Santos
;;; CS452 ASN3

(define nil '())

(define (factorial x)
    (cond ((<= x 1) 1)
        (else (* x (factorial(- x 1))))
    )
)

(define (factorial2-helper x n)
    (cond ((zero? x) n)
        (else (factorial2-helper (- x 1) (* n x)))
    )
)

(define (factorial2 x) (factorial2-helper x 1))
 
