; p2.19
(define us-coins (list 50 25 10 5 1))
(define us-coins-1 (list 5 1 50 25 10))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
            (+  (cc amount
                    (except-first-denomination coin-values))
                (cc (- amount
                    (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values) (null? coin-values))

(define (first-denomination coin-values) (car coin-values))

(define (except-first-denomination coin-values) (cdr coin-values))

; It does not depend on order because of we are using tree
; recursion and each coin is tested with the
; rest of denominations