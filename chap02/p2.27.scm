; p2.27
(define nil (list))

(define (reverse-1 l)
    (define (reverse-i l acc)
        (if (null? l) 
              acc
              (reverse-i (cdr l) (cons (car l) acc))))
    (reverse-i l (list)))

(define (deep-reverse l)
    (cond ((null? l) l)
          ((pair? (car l)) (cons (deep-reverse (car l)) (deep-reverse (cdr l))))
          (else (cons reverse-1 l))))

(define l1 (list (list 1 2 3) (list 4 5 6)))
(define l2 (list 1 (list 2 3 4) 5 6 (list 7 8) 9))
(define l3 (list (list (list (list 1 2 3))) (list 4 5)))

(display l1)
(display " -----> ")
(display (deep-reverse l1))
(newline)

(display l2)
(display " -----> ")
(display (deep-reverse l2))
(newline)

(display l3)
(display " -----> ")
(display (deep-reverse l3))
(newline)