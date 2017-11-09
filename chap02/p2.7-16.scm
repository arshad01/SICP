;; Problem 2.7 - 2.16

(define (make-interval a b) (cons a b))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

; invert interval
(define (invert-interval x)
    (div-interval (make-interval 1 1) x))

; p2.7
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; p2.8
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (lower-bound y))
                   (- (upper-bound x) (upper-bound y))))

; p2.9
(define (interval-width x)
    (/ (- (upper-bound x) (lower-bound x)) 2))

; p2.10
(define (zero-span? x)
    (cond ((and (<= (lower-bound x) 0) (>= (upper-bound x) 0)) (error "Zero span"))))

; redefine div-interval
(define (div-interval x y)
    (zero-span? y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

; testing
; 6.8 (10%) in parallel with 4.7 (5%)
(display (invert-interval 
    (add-interval (invert-interval (make-interval 6.12 7.48))
                  (invert-interval (make-interval 4.465 4.935)))))

(invert-interval (make-interval 1 1))
(newline)



