; p2.22
(define (square x) (* x x))
(define nil (list))

(define (square-list-1 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things) (cons (square (car things)) answer))))
    (iter items nil))

; list is reverse because each new element is prepended to the 'answer' list

(define (square-list-2 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer (square (car things))))))
    (iter items nil))

; list contains nested lists because we are using list as first argument to cons
