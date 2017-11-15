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
          ((pair? (car l)) (cons (reverse (car l)) (deep-reverse (cdr l))))
          (else (reverse (cons (car l) (deep-reverse (cdr l)))))))