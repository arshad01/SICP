; p2.33
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define nil (list))

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length sequence)
    (accumulate (lambda (_ y) (+ 1 y)) 0 sequence))


(define l1 (list 1 2 3))
(define l2 (list 4 5 6))

(display "(map (lambda (x) (* x x) ")
(display l1)
(display "): ----> ")
(display (map (lambda (x) (* x x)) l1))
(newline)

(display "(append ")
(display l1)
(display " ")
(display l2)
(display "): ----> ")
(display (append l1 l2))
(newline)

(display "(length ")
(display l1)
(display "): ----> ")
(display (length l1))
(newline)


