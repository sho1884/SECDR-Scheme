(define (insert x a)
  (if (null? a)
      (cons x ())
      (amb (cons x a) (cons (car a) (insert x (cdr a))))))

(define (perm b)
  (if (null? b)
      ()
      (insert (car b) (perm (cdr b)))))

(define (choice n)
  (if (= n 1)
      1
      (amb n (choice (-1+ n)))))

(define (attacks i j place)
  (if (null? place)
      #f
      (let ((ii (caar place))
	    (jj (cdar place)))
	(cond ((= i ii) #t)
	      ((= j jj) #t)
	      ((= (+ i j) (+ ii jj)) #t)
	      ((= (- i j) (- ii jj)) #t)
	      (else (attacks i j (cdr place)))))))

(define (addqueen i n place)
  (let ((j (choice n)))
    (if (attacks i j place)
	(fail)
	(let ((newplace (cons (cons i j) place)))
	  (if (= i n)
	      newplace
	      (addqueen (1+ i) n newplace))))))

(define (queen n)
  (addqueen 1 n ()))

(define (test-body)
  (let ((value (choice 10)))
    (write value)
    (newline)
    (fail)))

(define (test)
  (amb-unwind-protect
   (begin
     (display "protected-form")
     (newline)
     (test-body))
   (begin
     (display "cleanup-form")
     (newline))))


(display (setof (perm '(a b c))))
(newline)

(display (if (init-amb) (test) 'end))
(newline)
(fail)
(newline)

(display (setof (queen 8)))
(newline)
