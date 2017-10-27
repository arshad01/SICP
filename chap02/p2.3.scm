;;; Problem 2.3 - Section 2.1.2
(load "p2.2.scm")

; make-rect using the segments in p2.2
(define (make-rect1 p w h)
    (let ((x (x-point p))
         (y (y-point p)))

        (cons (make-segment x y (+ x w) y)
            (cons (make-segment (+ x w) y (+ x w) (+ y h))
                (cons (make-segment (+ x w) (+ y h) x (+ y h))
                      (make-segment x (+ y h) x y))))))

(define (side-1 rect) (car rect))
(define (side-2 rect) (car (cdr rect)))
(define (side-3 rect) (car (cdr (cdr rect))))
(define (side-4 rect) (cdr (cdr (cdr rect))))

(define (width1 rect)
    (let ((x-start (x-point (start-segment (side-1 rect))))
          (x-end   (x-point (end-segment (side-1 rect)))))
        (abs (- x-end x-start))))

(define (height1 rect)
    (let ((y-start (y-point (start-segment (side-2 rect))))
          (y-end   (y-point (end-segment (side-2 rect)))))
        (abs (- y-end y-start))))

(define (perimeter1 rect)
    (let ((w (width1 rect))
          (h (height1 rect)))
        (+ (* 2 w) (* 2 h))))

(define (area1 rect)
    (let ((w (width1 rect))
          (h (height1 rect)))
        (* w h)))

(define (print-rect1 rect)
    (display "side-1:")
    (print-segment (side-1 rect))
    (display "side-2:")
    (print-segment (side-2 rect))
    (display "side-3:")
    (print-segment (side-3 rect))
    (display "side-4:")
    (print-segment (side-4 rect)))