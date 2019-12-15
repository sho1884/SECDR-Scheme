;defmacro.scm
;CL-style defmacro for SCM
;dorai@cs.rice.edu

(define defmacro
  (let ((defmacro-transformer
	  (lambda (name parms . body)
	    `(define ,name
	       (let ((transformer
		       (lambda ,parms
			 ,@body)))
		 (put ',name 'macro transformer)
		 (procedure->memoizing-macro
		   (lambda (e r)
		     (apply transformer (cdr e)))))))))
    (put 'defmacro 'macro defmacro-transformer)
    (procedure->memoizing-macro
      (lambda (e r)
	(apply defmacro-transformer (cdr e))))))

(define macro?
  (lambda (m)
    (get m 'macro)))

(define macroexpand-1
  (lambda (e)
    (if (not (pair? e))	e
      (let ((a (car e)))
	(if (not (symbol? a)) e
	  (let ((m (get a 'macro)))
	    (if (not m) e (apply m (cdr e)))))))))

'(define gentemp
  (let ((n -1))
    (lambda ()
      ;generates an allegedly new symbol.
      ;This is a gross hack since there is no standardized way of
      ;determining if the symbol is already used.
      (set! n (+ n 1))
      (string->symbol (string-append "Temp" (number->string n))))))
