;Dorai Sitaram
;destructive append

(define append!
  ;;only two arguments
  (lambda (s1 s2)
    (if (null? s1) s2
      (let loop ((r1 s1))
        (if (null? r1) (error 'append! s1 s2)
          (let ((r2 (cdr r1)))
            (if (null? r2)
                (begin
                  (set-cdr! r1 s2)
                  s1)
                (loop r2))))))))
