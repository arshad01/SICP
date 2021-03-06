;; Problem 2.7 - 2.16

(define (make-interval a b) (cons a b))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

; invert interval
(define (invert-interval x)
    (div-interval (make-interval 1 1) x))

; p2.7
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; p2.8
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

; p2.9
(define (interval-width x)
    (/ (- (upper-bound x) (lower-bound x)) 2))

; p2.10
(define (zero-span? x)
    (cond ((= 0 (+ (lower-bound x) (upper-bound x))) (error "Zero span"))))

; redefine div-interval
(define (div-interval x y)
    (zero-span? y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y)))))

; p2.11
; redefine mul-interval

; define interval class for interval <xl, xu>
; ref: http://fab.cba.mit.edu/classes/S62.12/docs/Hickey_interval.pdf
; p = interval limits are positive; i.e 0 <= xl < xu
(define (p? x)
  (and (<= 0 (lower-bound x)) (<= (lower-bound x) (upper-bound x))))
; n = interval limits are negartive; i.e xl <= xu <= 0
(define (n? x)
  (and (<= (lower-bound x) (upper-bound x)) (<= (upper-bound x) 0)))
; m = interval is mixed; i.e xl < 0 < xu
(define (m? x)
  (and (< (lower-bound x) 0) (< 0 (upper-bound x))))

(define (mul-interval x y)
    (let ((xl (lower-bound x))
          (xu (upper-bound x))
          (yl (lower-bound y))
          (yu (upper-bound y)))
      (cond ((and (p? x) (p? y)) (make-interval (* xl yl) (* xu yu)))
            ((and (p? x) (m? y)) (make-interval (* xu yl) (* xu yu)))
            ((and (p? x) (n? y)) (make-interval (* xu yl) (* xl yu)))
            ((and (m? x) (p? y)) (make-interval (* xl yu) (* xu yu)))
            ((and (m? x) (m? y)) (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu))))
            ((and (m? x) (n? y)) (make-interval (* xu yl) (* xl yl)))
            ((and (n? x) (p? y)) (make-interval (* xl yu) (* xu yl)))
            ((and (n? x) (m? y)) (make-interval (* xl yu) (* xl yl)))
            ((and (n? x) (n? y)) (make-interval (* xu yu) (* xl yl))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

; p2.12
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (percent x)
  (let ((c (center x))
        (w (width x)))
       (/ (* 100 w) c)))

; p2.13
; let X = [(a - (a*x)/100), (a + (a*x)/100)]
;     Y = [(b - (b*y)/100), (b + (b*y)/100)]
; X * Y = [(a - (a*x)/100), (a + (a*x)/100)] * [(b - (b*y)/100), (b + (b*y)/100)]
;       = [(a*b - a*b*x/100 - a*b*y/100 + a*b*x*y/100), (a*b + a*b*x/100 + a*b*y/100 + a*b*x*y/100)]
; ignoring factors with x*y
;       = [(a*b - a*b*x/100 - a*b*y/100), (a*b + a*b*x/100 + a*b*y/100)]
;       = [a*b - a*b(x/100 + y/100), a*b + a*b(x/100 + y/100)]
; For small tolerances (e.g.<= 10%), the tolerance of product is (x + y)%

(display "interval=[6+-2%]*[5+-4%]")
(newline)
(display "tolerance actual (%)  = ")
(display (percent (mul-interval (make-center-percent 6.0 2) (make-center-percent 5 4))))
(newline)
(display "tolerance estimate (%)= ")
(display (+ 2 4))
(newline)


; p2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
        (add-interval (div-interval one r1)
                      (div-interval one r2)))))

(display "percent par1=")
(display (percent (par1 (make-center-percent 6 2) (make-center-percent 7 1))))
(newline)
(display "percent par2=")
(display (percent (par2 (make-center-percent 6 2) (make-center-percent 7 1))))
(newline)

; p2.15
; par2 is better because of less rounding error in the floating point values
; par1 multiplies small values which result in further smaller value and
; may result in value underflow.

; p2.16
; Use integer based system. Convert all percentages to inetger to get rid of the decimal


; testing
; 6.8 (10%) in parallel with 4.7 (5%)
;(display (invert-interval 
;    (add-interval (invert-interval (make-interval 6.12 7.48))
;                  (invert-interval (make-interval 4.465 4.935)))))
;(newline)
;(display (percent (make-center-percent 3.5 15)))
;(newline)



