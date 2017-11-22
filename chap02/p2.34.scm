; p2.34
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define nil (list))

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))

; 1 + 3x + 5x^3 + x^5 at x = 2 = 79

(define l1 (list 1 3 0 5 0 1))

(display "(horner-eval 2 ")
(display l1)
(display ") = ")
(display (horner-eval 2 l1))
(newline)