;;; Problem 2.1 - Section 2.1.1
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (make-rat n d)
    (if (= d 0)
        (error "Denominator cannot be 0")
        (let ((g (gcd (abs n) (abs d)))
              (nn (abs n))
              (dd (abs d)))
            (if (and (< n 0) (< d 0))
                (cons (/ nn g) (/ dd g))
                (if (or (< n 0) (< d 0))
                    (cons (- (/ nn g)) (/ dd g))
                    (cons (/ n g) (/ d g)))))))

(define (numer x)
        (car x))

(define (denom x)
        (cdr x))

(define (print-rat x)
        (display (numer x))
        (display "/")
        (display (denom x))
        (newline))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mult-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

