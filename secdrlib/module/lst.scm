;\Title{lst}
; :
; [omitted]
; :
;\Implementation
(MODULE lst
        (EXPORT every?
                delete!)

        (INIT
          (display "MODULE lst loaded.") (newline))

        ;\Part{every?}
        ; Test whether every element of a list satisfies a given predicate.
        (every?
          (lambda (proc l)
            (or (null? l)
                (and (proc (car l))
                     (every? proc (cdr l))))))

        ;\Part{delete!}
        ; Delete all those elements having the specified quality from the list.
        (delete!
          (lambda (proc l)
            (cond ((null? l)
                   '())
                  ((proc (car l))
                   (delete! proc (cdr l)))
                  (else
                    (set-cdr! l (delete! proc (cdr l)))
                    l)))))
