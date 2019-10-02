
;;; catch.scm 
;;;     coded by Shoichi Hayashi (9/23/1991) s-haya@rst.fujixerox.co.jp

(macro catch
  (lambda (l)
    (let ((tag-name (cadr l))(body-exp (cddr l)))
      `(call-with-current-continuation
	(lambda (,tag-name) ,@body-exp)))))

; (catch x x) == (current-continuation)

(define identity (lambda (x) x))

(define (current-continuation)
  (call-with-current-continuation
   identity))

; call-with-current-continuation == call/cc

(define (call/cc proc)
  (catch z (proc z)))

;***** sample *****

(define (fact n)
  (catch ans
	 ((lambda (x looptag)
	    (set! looptag (catch z z))
	    (if (= n 0) (ans x))
	    (set! x (* x n))
	    (set! n (-1+ n))
	    (looptag looptag))
	  1
	  nil)))

(write (fact 5))
