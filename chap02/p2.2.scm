;;; Problem 2.2 - Section 2.1.2
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p) 
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

(define (make-segment x1 y1 x2 y2) 
    (cons (make-point x1 y1) (make-point x2 y2)))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (print-segment seg)
    (print-point (start-segment seg))
    (display "----")
    (print-point (end-segment seg))
    (newline))

(define (midpoint-segment seg)
    (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2.0)
                (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2.0)))

;(print-point (midpoint-segment (make-segment 1 2 3 4)))