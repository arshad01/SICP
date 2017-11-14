; problems 2.17 and 2.18

; p2.17
(define (last-pair l)
    (cond ((null? l) (error "Empty list"))
          ((null? (cdr l)) (car l))
          (else (last-pair (cdr l)))))

; p2.18
(define (reverse l)
    (define (reverse-i l acc)
        (if (null? l) 
              acc
              (reverse-i (cdr l) (cons (car l) acc))))
    (reverse-i l (list))) 
