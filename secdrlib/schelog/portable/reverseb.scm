;Dorai Sitaram
;destructive reverse

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	(let ((d (cdr s)))
	  (set-cdr! s r)
	  (loop d s))))))
