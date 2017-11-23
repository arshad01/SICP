; p2.41
(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define nil (list))

(define (unique-triples n)
    (flatmap 
        (lambda (i)
            (flatmap (lambda (j) 
                (map (lambda (k) (list i j k))
                    (enumerate-interval 1 (- j 1))))
            (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (make-triple-sum triple)
    (list (car triple) (cadr triple) (caddr triple) (+ (car triple) (cadr triple) (caddr triple))))

(define (sum-triples s n)
    (map make-triple-sum (filter (lambda (x) (= s (+ (car x) (cadr x) (caddr x)))) (unique-triples n))))
