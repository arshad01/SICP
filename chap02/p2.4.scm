(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

; e.g.
; #;1> (load "p2.4.scm")
; loading p2.4.scm ...

; Note: assignment to imported value binding: cons

; Note: assignment to imported value binding: car

; Note: assignment to imported value binding: cdr
; #;2> (car (cons 2 3))
; 2
; #;3> (cdr (cons 2 3))
; 3
; #;4> (car (cons 2 (cons 3 4)))
; 2
; #;5> (cdr (cons 2 (cons 3 4)))
; #<procedure (? m)>
; #;6>
; In the last case the value returned is the function
; lambda(m) (m 3 4)
; which is (cons 3 4)