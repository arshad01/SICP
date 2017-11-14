; p2.20
; Can also use 'reverse' from problem 2.18
(define (same-parity x . y)
    (define (same-parity-i y acc)
        (cond ((null? y) acc)
              ((eq? (even? x) (even? (car y))) (same-parity-i (cdr y) (cons (car y) acc)))
              (else (same-parity-i (cdr y) acc))))
    (reverse (same-parity-i y (list x))))