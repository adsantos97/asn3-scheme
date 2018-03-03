(define nil '())
; homework write factorial  ... both non-tail-recursive and tail recursive
; homework remove duplicates (see moodle page)

(define (length x)
  (cond ((null? x)   0)
        (else       (+ 1 (length (cdr x))))
  )
)

(define (len2-helper x rsf)
  (cond ((null? x)  rsf)
        (else       (len2-helper  (cdr x)  (+ 1 rsf)))
  )
)

(define (len2 x) (len2-helper x 0))


(define (append a b)
  (cond ((null? a)  b)
        (else      (cons (car a) (append (cdr a) b)))
  )
)

(define (append2-helper a rsf)
  (cond ((null? a)  rsf)
        (else       (append2-helper (cdr a)  (cons (car a) rsf)))
  )
)

(define (append2 a b) (append2-helper (reverse a) b))



; O(n^2)
(define (reverse x)
  (cond ((null? x)  nil)
        (else     (append (reverse (cdr x)) (list (car x))))
  )
)

; i'm tail recursive == last call is the recursive one
; O(n)
(define (rev2-helper a b)
  (cond ((null? a)   b)
        (else        (rev2-helper (cdr a) (cons (car a) b)))
  )
)

; O(n)
(define (rev2 x) (rev2-helper x nil))


(define (last l)
  (cond ((null? l)        nil)
        ((null? (cdr l))) (car l)
		(else             (last (cdr l)))
  )
)
  


; a stack
; push Stack x Item -> Stack
; pop Stack -> Stack
; top Stack -> Item
(define (push s e) (cons e s))
(define (pop s)    (cdr s))
(define (top s)    (car s))
(define empty-stack nil)
(top (push empty-stack 'a))
(pop (push (push empty-stack 4) 'a ))

(define (map f list)
  (cond ((null? list)  nil)
        (else          (cons (f (car list)) (map f (cdr list))))
  )
)

(define (inc x) (+ 1 x))

(define (fix-first bi-func const-value)
  (lambda (x) (bi-func const-value x))
)


(define (inc2 x) (fix-first + 1))
(define (add a b) (+ a b))
(define (inc3 x) ((fix-first add 1) x)   )
;; needs fix-first to be curried:: (define (inc2 x) (fix-first add 1 x)   )


;; "script" from today's class
; 
; > (inc3 4)
; 5
; > ns
; (1 3 5 7)
; > (map inc ns)
; (2 4 6 8)
; 
; 
; > (define (add-x-to-each ns x)  (map  (lambda (foo)  (+ foo x)) ns))
; add-x-to-each
; > (add-x-to-each ns 2)
; (3 5 7 9)
; > (add-x-to-each ns 10)
; (11 13 15 17)
; > (define (add10 x) (+ x 10))
; add10
; > (map add10 ns)
; (11 13 15 17)
; > (define (curry-add x) (lambda (y) (+ x y)))
; curry-add
; > (curry-add 10)
; #<compound value>
; > ( (curry-add 10 )  5)
; 15
; > (map (curry-add 10)  ns)
; (11 13 15 17)
; 
