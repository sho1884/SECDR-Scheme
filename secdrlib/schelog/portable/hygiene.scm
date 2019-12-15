;hygiene.scm
;Dorai Sitaram, dorai@cs.rice.edu, 11 Dec. 1993
;A vanilla implementation of a hygiene filter for define-syntax
;(see mbe.scm)

(define hyg:tag-generic
  (lambda (e kk tmps)
    (if (pair? e)
	(let ((a (car e)))
	  (case a
	    ((quote) `(quote ,(hyg:tag-vanilla (cadr e) kk tmps)))
	    ((if begin)
	     `(,a ,@(map (lambda (e1) (hyg:tag-generic e1 kk tmps))
			 (cdr e))))
	    ((set! define)
	     `(,a ,(hyg:tag-vanilla (cadr e) kk tmps)
		  ,@(map (lambda (e1) (hyg:tag-generic e1 kk tmps))
                         (cddr e))))
	    ((lambda) (hyg:tag-lambda (cdr e) kk tmps))
	    ((letrec) (hyg:tag-letrec (cdr e) kk tmps))
	    ((let) (hyg:tag-let (cdr e) kk tmps))
	    ((let*) (hyg:tag-let-star (cdr e) kk tmps))
	    ((do) (hyg:tag-do (cdr e) kk tmps))
	    ((case)
	     `(case ,(hyg:tag-generic (cadr e) kk tmps)
		,@(map
		    (lambda (cl)
		      `(,(hyg:tag-vanilla (car cl) kk tmps)
			,@(map
			    (lambda (e1)
			      (hyg:tag-generic e1 kk tmps))
			    (cdr cl))))
		    (cddr e))))
	    ((cond)
	     `(cond ,@(map
			(lambda (cl)
			  (map (lambda (e1)
				 (hyg:tag-generic e1 kk tmps))
			       cl))
			(cdr e))))
	    (else (map (lambda (e1)
			 (hyg:tag-generic e1 kk tmps))
		       e))))
	(hyg:tag-vanilla e kk tmps))))

(define hyg:tag-vanilla
  (lambda (e kk tmps)
    (cond ((symbol? e)
	   (cond ((memq e kk) e)
		 ((assq e tmps) => cdr)
		 (else e)))
	  ((pair? e)
	   (cons (hyg:tag-vanilla (car e) kk tmps)
             (hyg:tag-vanilla (cdr e) kk tmps)))
	  (else e))))

(define hyg:tag-lambda
  (lambda (e kk tmps)
    (let* ((bvv (car e))
	   (tmps2 (append
		    (map (lambda (v) (cons v (gentemp)))
			 (hyg:flatten bvv))
                    tmps)))
      `(lambda
	 ,(hyg:tag-vanilla bvv kk tmps2)
	 ,@(map
	     (lambda (e1)
	       (hyg:tag-vanilla e1 kk tmps2))
	     (cdr e))))))

(define hyg:flatten
  (lambda (e)
    (let loop ((e e) (r '()))
      (cond ((pair? e) (loop (car e)
                         (loop (cdr e) r)))
	    ((null? e) r)
	    (else (cons e r))))))

(define hyg:tag-letrec
  (lambda (e kk tmps)
    (let* ((varvals (car e))
	   (tmps2 (append
		    (map (lambda (v) (cons v (gentemp)))
			 (map car varvals))
		    tmps)))
      `(letrec ,(map
		  (lambda (varval)
		    `(,(hyg:tag-vanilla (car varval)
			 kk tmps2)
		      ,(hyg:tag-generic (cadr varval)
			 kk tmps2)))
		  varvals)
	 ,@(map (lambda (e1)
		  (hyg:tag-generic e1 kk tmps2))
		(cdr e))))))

(define hyg:tag-let
  (lambda (e kk tmps)
    (let* ((varvals (car e))
	   (tmps2 (append (map (lambda (v) (cons v (gentemp)))
			       (map car varvals))
		    tmps)))
      `(let
	 ,(let loop ((varvals varvals)
		     (i (length varvals)))
	    (if (null? varvals) '()
	      (let ((varval (car varvals))
		    (tmps3 (list-tail tmps2 i)))
		(cons `(,(hyg:tag-vanilla (car varval)
			   kk tmps2)
			,(hyg:tag-generic (cadr varval)
			   kk tmps3))
		  (loop (cdr varvals) (- i 1))))))
	 ,@(map
	     (lambda (e1)
	       (hyg:tag-generic e1 kk tmps2))
	     (cdr e))))))

(define hyg:tag-do
  (lambda (e kk tmps)
    (let* ((varinistps (car e))
	   (tmps2 (append (map (lambda (v) (cons v (gentemp)))
			       (map car varinistps))
		    tmps)))
      `(do
	 ,(let loop ((varinistps varinistps)
		     (i (length varinistps)))
	    (if (null? varinistps) '()
	      (let ((varinistp (car varinistps))
		    (tmps3 (list-tail tmps2 i)))
		(cons `(,(hyg:tag-vanilla (car varinistp)
			   kk tmps2)
			,(hyg:tag-vanilla (cadr varinistp)
			   kk tmps2)
			,@(hyg:tag-generic (cddr varinistp)
			    kk tmps3))
		  (loop (cdr varinistps) (- i 1))))))
	 ,(map (lambda (e1)
		(hyg:tag-generic e1 kk tmps)) (cadr e))
	 ,@(map
	     (lambda (e1)
	       (hyg:tag-generic e1 kk tmps2))
	     (cddr e))))))

(define hyg:tag-let-star
  (lambda (e kk tmps)
    (let* ((varvals (car e))
	   (tmps2 (append (reverse (map (lambda (v) (cons v (gentemp)))
			       (map car varvals)))
		    tmps)))
      `(let*
	 ,(let loop ((varvals varvals)
		     (i (- (length varvals) 1)))
	    (if (null? varvals) '()
	      (let ((varval (car varvals))
		    (tmps3 (list-tail tmps2 i)))
		(cons `(,(hyg:tag-vanilla (car varval)
			   kk tmps3)
			,(hyg:tag-generic (cadr varval)
			   kk (cdr tmps3)))
		  (loop (cdr varvals) (- i 1))))))
	 ,@(map
	     (lambda (e1)
	       (hyg:tag-generic e1 kk tmps2))
	     (cdr e))))))
