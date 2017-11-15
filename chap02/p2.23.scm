; p2.23
(define (for-each proc items)
    (if (null? items)
        #t
        (let ((_ (proc (car items))))
            (for-each proc (cdr items)))))