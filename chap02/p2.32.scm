; p2.32
(define nil (list))

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define l1 (list 1 2 3))
(define l2 (list 'a 'b 'c))

(display l1)
(display " ----> ")
(display (subsets l1))
(newline)
(display l2)
(display " ----> ")
(display (subsets l2))
(newline)