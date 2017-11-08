(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (count-factors x b)
    (define (count-factors-i x b acc)
        (if (and (> x 0) (= 0 (remainder x b)))
            (count-factors-i (/ x b) b (+ 1 acc))
            acc))
    (count-factors-i x b 0))

(define (car x)
    (count-factors x 2))

(define (cdr x)
    (count-factors x 3))