;;; Problem 2.1 - Section 2.1.1
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (cond (= d 0)
        (error "Denominator cannot be 0")
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g)))))

(define (numer x)
        (car x))

(define (denom x)
        (cdr x))

(define (print-rat x)
        (newline)
        (display (numer x))
        (display "/")
        (display (denom x)))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))


