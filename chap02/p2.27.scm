; p2.27
(define nil (list))

(define (reverse-2 l)
    (define (reverse-i l acc)
        (cond ((null? l) acc)
              ((pair? (car l)) (reverse-i (cdr l) (cons (reverse-i (car l) nil) acc)))
              (else (reverse-i (cdr l) (cons (car l) acc)))))
    (reverse-i l nil))

(define l1 (list 1 2 3 4 5 6))
(define l2 (list 1 (list 2 3 4) 5 6 (list 7 8) 9))
(define l3 (list (list (list (list 1 2 3))) (list 4 5)))
(define l4 (list 1 2 3 4 (list 5 6)))
(define l5 (list (list 1 2 3) (list 4 5 6)))


(define (reverse-2-test lists)
    (if (null? lists)
        (newline)
        (let ()
            (display (car lists))
            (display " ------> ")
            (display (reverse-2 (car lists)))
            (newline)
            (reverse-2-test (cdr lists)))))

(reverse-2-test (list l1 l2 l3 l4 l5))


