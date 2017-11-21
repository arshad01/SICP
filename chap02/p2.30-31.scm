; p2.30
(define (square x) (* x x))

(define (square-tree-1 tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree-1 (car tree))
                      (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-2 sub-tree)
                (square sub-tree)))
        tree))


(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(display t1)
(display " ------> ")
(display "square-tree-1=")
(display (square-tree-1 t1))
(display ", square-tree-2=")
(display (square-tree-2 t1))
(newline)

; p2.31
(define (tree-map fn tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map fn sub-tree)
                (fn sub-tree)))
        tree))


(display t1)
(display " ------> ")
(display "tree-map=")
(display (tree-map square t1))
(newline)
