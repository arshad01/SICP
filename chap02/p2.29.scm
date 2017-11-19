; p2.29
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

; (a)
(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (car (cdr branch)))

; (b)
; Returns true if a structure is a mobile
(define (mobile? struc)
    (pair? struc))

(define (total-weight mobile)
    (define (total-weight-i mobile acc)
        (cond ((null? mobile) acc)
              ((not (pair? mobile)) (+ mobile acc))
              ((not (pair? (left-branch mobile))) (total-weight-i (right-branch mobile) acc))
              (else (+ (total-weight-i (left-branch mobile) 0) (total-weight-i (right-branch mobile) acc)))))
    (total-weight-i mobile 0))

(define (total-weight-2 mobile)
    (define (total-weight-i mobile acc)
        (cond ((null? mobile) acc)
              ((not (mobile? (branch-structure (left-branch mobile)))) (+ (branch-structure (left-branch mobile)) (total-weight-i (right-branch) acc)))
              (else (+ (total-weight-i (left-branch mobile) 0) (total-weight-i (right-branch mobile) acc)))))
    (total-weight-i mobile 0))

(define m1 (make-mobile (make-branch 1 2) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))
(define m2 (make-mobile (make-branch 1 2) (make-branch 3 4)))
(define m3 (make-mobile (make-branch 1 (make-mobile (make-branch 8 9) (make-branch 10 11))) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))

(display m3)
(display " = ")
(display (total-weight m3))
(newline)

; (c)
;(define (balanced? mobile)
;    (= (* (branch-length (left-branch mobile)) (branch-structure)))