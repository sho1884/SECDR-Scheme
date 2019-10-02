(define (fact n c)
	(if (= n 0) (c 1)
		(fact (- n 1)
			(lambda (a) (c (* n a))))))

;(fact 7 (lambda (x) x))

(define (fact n)
    (let ((ans 1)
          (pitch nil))
        (let ((w (call-with-current-continuation
                    (lambda (-c-)
                        (set! pitch -c-)
                        n))))
            (cond ((= w 0) ans)
                  (else (set! ans (* ans w))
                        (pitch (-1+ w)))))))


(define identity (lambda (x) x))

(define (current-continuation)
    (call-with-current-continuation
    identity))

(define (fact n)
  (let ((ans 1))
    (let ((pitch (current-continuation)))
      (cond ((= n 0) ans)
	    (else (set! ans (* ans n))
		  (set! n (-1+ n))
		  (pitch pitch))))))

