;property lists
;dorai@cs.rice.edu

(define proplist:properties '())

(define get
  (lambda (x p . default)
    (let ((default (if (pair? default) (car default) #f))
	  (c (assq x proplist:properties)))
      (if (not c) default
	(let ((d (assq p (cdr c))))
	  (if (not d) default
	    (cdr d)))))))

(define put
  (lambda (x p v)
    (let ((c (assq x proplist:properties)))
      (if (not c)
	  (set! proplist:properties
	    (cons (cons x (list (cons p v)))
		  proplist:properties))
	  (let* ((c2 (cdr c))
		 (d (assq p c2)))
	    (if (not d)
		(set-cdr! c
		  (cons (cons p v) c2))
		(set-cdr! d v))))
      v)))
