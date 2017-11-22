; p2.35
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define nil (list))

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) 
                                (if (not (pair? x)) 
                                1
                                (count-leaves x))) t)))

(define t1 (list (list 1 2) (list 3 4)))
(define t2 (list (list (list (list 1 (list 2 3))))))

(display t1)
(display " = ")
(display (count-leaves t1))
(newline)
(display t2)
(display " = ")
(display (count-leaves t2))
(newline)