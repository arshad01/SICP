; p2.21

(define (square-list-1 items)
    (if (null? items)
        items
        (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
    (map (lambda (x) (* x x)) items))
