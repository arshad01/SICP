; p2.39
(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (fold-right op initial (cdr sequence)))))

(define nil (list))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
        result
        (iter (op result (car rest))
                         (cdr rest))))
    (iter initial sequence))

(define (reverse-right sequence)
    (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-left sequence)
    (fold-left (lambda (x y) (cons y x)) nil sequence))

(display "(reverse-right (list 1 2 3 4)) = ")
(display (reverse-right (list 1 2 3 4)))
(newline)
(display "(reverse-left (list 1 2 3 4)) = ")
(display (reverse-left (list 1 2 3 4)))
(newline)
