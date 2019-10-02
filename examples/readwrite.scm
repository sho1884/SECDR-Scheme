(define (reader corou)
  (letrec ((mainloop
	    (lambda (obj corou)
	      (if (eq? obj '*)
		  (numloop (read) corou)
		  (if (eq? obj '!)
		      (corou '$done$ nil)
		      (corou obj (lambda (c) (mainloop (read) c)))))))
	   (numloop
	    (lambda (obj corou)
	      (if (eq? obj '*)
		  (mainloop (read) corou)
		  (if (number? obj)
		      (corou obj (lambda (c) (numloop (read) c)))
		      (numloop (read) corou))))))
    (mainloop (read) corou)))

(define (writer corou)
  (letrec ((writeloop
	    (lambda (thing corou)
	      (if (eq? thing '$done$)
		  'done
		  (if (eq? thing '<)
		      (listloop nil corou)
		      (printer thing corou)))))
	   (listloop
	    (lambda (l corou)
	      (corou (lambda (thing c)
		       (if (eq? thing '>)
			   (printer (reverse l) c)
			   (listloop (cons thing l) c))))))
	   (printer
	    (lambda (obj corou)
	      (display obj)
	      (newline)
	      (corou writeloop))))
    (corou writeloop)))
