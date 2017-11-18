; p2.28
(define (fringe x)
    (cond ((null? x) x) 
          ((not (pair? (car x))) (cons (car x) (fringe (cdr x)))) 
          (else (append (fringe (car x)) (fringe (cdr x))))))

(define t1 (list (list 1 2) (list 3 4)))
(define t2 (list t1 t1))
 
(display t1)
(display " ----> ")
(display (fringe t1))
(newline)
(display t2)
(display " ----> ")
(display (fringe t2))
(newline)