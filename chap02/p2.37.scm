; p2.37
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define nil (list))

(define (accumulate-n op init seqs)
        (if (null? (car seqs))
        nil
        (cons (accumulate op init (map (lambda (x) (car x)) seqs)) 
              (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (r) (dot-product v r)) m))

(define (transpose mat)
    (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
         (map (lambda (r) (matrix-*-vector n r)) m)))


(define m1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v1 (list 2 4 6 8))

(display "(dot-product ")
(display v1)
(display " ")
(display (car m1))
(display ") = ")
(display (dot-product v1 (car m1)))
(newline)

(display "(matrix-*-vector ")
(display m1)
(display " ")
(display v1)
(display ") = ")
(display (matrix-*-vector m1 v1))
(newline)

(display "(transpose ")
(display m1)
(display ") = ")
(display (transpose m1))
(newline)

(display "(matrix-*-matrix ")
(display m1)
(display " ")
(display m1)
(display ") = ")
(display (matrix-*-matrix m1 m1))
(newline)
