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

; check of branch is a list or just an atom
(define (branch-structure branch)
    (if (pair? branch)
        (car (cdr branch))
        branch))

; (b)
; Returns true if a structure is a mobile
(define (mobile? struc)
    (pair? struc))

;(define (total-weight mobile)
;    (define (total-weight-i mobile acc)
;        (cond ((null? mobile) acc)
;              ((not (pair? mobile)) (+ mobile acc))
;              ((not (pair? (left-branch mobile))) (total-weight-i (right-branch mobile) acc))
;              (else (+ (total-weight-i (left-branch mobile) 0) (total-weight-i (right-branch mobile) acc)))))
;    (total-weight-i mobile 0))

; total-weight
; When traversing the tree always traverse via branch-structure since this where the child mobile is stored
(define (total-weight mobile)
    (define (total-weight-i mobile acc)
        (cond ((null? mobile) acc)
              ((not (pair? mobile)) (+ mobile acc))
              ((not (mobile? (branch-structure (left-branch mobile)))) (+ (branch-structure (left-branch mobile)) 
                                                                          (total-weight-i (branch-structure (right-branch mobile)) acc)))
              ((not (mobile? (branch-structure (right-branch mobile)))) (+ (total-weight-i (branch-structure (left-branch mobile)) acc) 
                                                                           (branch-structure (right-branch mobile))))
              (else (+ (total-weight-i (branch-structure (left-branch mobile)) acc) 
                       (total-weight-i (branch-structure (right-branch mobile)) acc)))))
    (total-weight-i mobile 0))

; (c)
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (define (balanced-i mobile flag)
    (cond ((or (null? mobile) (not (pair? mobile))) flag)
          ((not (mobile? (branch-structure (left-branch mobile))))  (= (torque (left-branch mobile)) (torque (right-branch mobile))))
          ((not (mobile? (branch-structure (right-branch mobile))))  (= (torque (right-branch mobile)) (torque (left-branch mobile))))
          (else (and (balanced-i (branch-structure (left-branch mobile)) flag) 
                     (balanced-i (branch-structure (right-branch mobile)) flag)))))
  (balanced-i mobile #f))

; Testing data
(define m1 (make-mobile (make-branch 1 2) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))
(define m2 (make-mobile (make-branch 1 2) (make-branch 3 4)))
(define m3 (make-mobile (make-branch 1 (make-mobile (make-branch 8 9) (make-branch 10 11))) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))
(define m4 (make-mobile (make-branch 2 (make-mobile (make-branch 1 3) (make-branch 3 1))) (make-branch 2 (make-mobile (make-branch 2 2) (make-branch 1 2)))))
(define m5 (make-mobile (make-branch 2 (make-mobile (make-branch 1 2) (make-branch 1 2))) (make-branch 2 (make-mobile (make-branch 1 2) (make-branch 1 2)))))
(define m6 (make-mobile (make-branch 1 2) (make-branch 1 2)))

; generic tester and printer
(define (fn-test fn lists)
    (if (null? lists)
        (newline)
        (let ()
            (display (car lists))
            (display " ------> ")
            (display (fn (car lists)))
            (newline)
            (fn-test fn (cdr lists)))))

(display "Testing total-weight 1")
(newline)
(fn-test total-weight (list m1 m2 m3 m4 m5 m6))

(display "Testing balanced? 1")
(newline)
(fn-test balanced? (list m1 m2 m3 m4 m5 m6))

; (d)
; No change in any other part of program is required
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(display "Testing total-weight 2")
(newline)
(fn-test total-weight (list m1 m2 m3 m4 m5 m6))

(display "Testing balanced? 2")
(newline)
(fn-test balanced? (list m1 m2 m3 m4 m5 m6))