;;; This is my code
;;; Arizza Santos
;;; CS452 ASN3

(define nil '())
(define dollar '$)
(define unsorted-list '(6 7 4 45 7 76 3 67 7 63 19))
(define sorted-list '(2 6 10 14 55 65 78 99 102))
(define binary-search-tree
  '(10 (8 (4 () (6 () ())) (9 () ()) ) (15 () (18 () ())) ))
(define simple-bts
  '(5 (4 () ()) (6 () ())))

(define (second_empty first second) (equal? second '() ))
(define (cdr_second a b) (cdr b)) 

; purpose: search a data structure for a chosen element
; input: data -> data structure (like a list) to search
;        element -> element to search for
;        current-item -> function that returns the current item at the front
;        done -> function that returns true if search stops with failure
;        found -> function that returns true if current item is the element
;                 search for
;        next -> function that returns the part of the data to be searched next
; output: element if found or nil if not found
(define (search data element current-item done found next)
  (cond
    ((done data data) nil)
    ((found element (current-item data)) (current-item data))
    (else         (search (next data data) element current-item done found next))
  )
)
