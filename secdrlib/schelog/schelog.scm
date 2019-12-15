;schelog.scm
;schelog
;An embedding of Prolog in Scheme
;(c) Dorai Sitaram, dorai@cs.rice.edu, 1989, Rice University
;revised Feb. 1993

;If you use SLIB (the Scheme library maintained by
;Aubrey Jaffer), set the following boolean variable to
;#t, else #f.

(define schelog:using-slib? #f)

(cond (schelog:using-slib?
        (require 'rev2-procedures)
        (require 'common-list-functions)
        (define reverse! nreverse)
        (require 'mbe))
      (else #f ;;make sure that the init file you
        ;;loaded before loading this file defines the
        ;;non-r4rs procedures append! and reverse!, and
        ;;define-syntax, the macro facility for
        ;;defining r4rs high-level macros.  (Only
        ;;top-level macros are needed.) Sample
        ;;implementations of these are provided in the
        ;;Schelog distribution in the subdirectory
        ;;portable/
        ))

(define schelog:call/cc call-with-current-continuation)

;logic variables and their manipulation

(define schelog:*ref* "ref")

(define schelog:*unbound* '_)

(define schelog:make-ref
  ;;makes a fresh unbound ref;
  ;;unbound refs point to themselves
  (lambda opt
    (vector schelog:*ref*
      (if (null? opt) schelog:*unbound*
	(car opt)))))

(define _ schelog:make-ref)

(define schelog:ref?
  (lambda (r)
    (and (vector? r)
	 (eq? (vector-ref r 0) schelog:*ref*))))

(define schelog:deref
  (lambda (r)
    (vector-ref r 1)))

(define schelog:set-ref!
  (lambda (r v)
    (vector-set! r 1 v)))

(define schelog:unbound-ref?
  (lambda (r)
    (eq? (schelog:deref r) schelog:*unbound*)))

(define schelog:unbind-ref!
  (lambda (r)
    (schelog:set-ref! r schelog:*unbound*)))

;frozen logic vars

(define schelog:*frozen* "frozen")

(define schelog:freeze-ref
  (lambda (r)
    (schelog:make-ref (vector schelog:*frozen* r))))

(define schelog:thaw-frozen-ref
  (lambda (r)
    (vector-ref (schelog:deref r) 1)))

(define schelog:frozen-ref?
  (lambda (r)
    (let ((r2 (schelog:deref r)))
      (and (vector? r2)
	   (eq? (vector-ref r2 0) schelog:*frozen*)))))

;deref a structure completely (except the frozen ones, i.e.)

(define schelog:deref*
  (lambda (s)
    (cond ((schelog:ref? s)
	   (if (schelog:frozen-ref? s) s
	     (schelog:deref* (schelog:deref s))))
	  ((pair? s) (cons (schelog:deref* (car s)) (schelog:deref* (cdr s))))
	  ((vector? s)
	   (list->vector (map schelog:deref* (vector->list s))))
	  (else s))))

;letref introduces new logic variables

(define schelog:*logic-variables* '())

(define-syntax letref
  (syntax-rules (schelog:*logic-variables*)
    ((letref (x ...) . e)
     (let ((x (schelog:make-ref)) ...)
       (let ((schelog:*logic-variables*
	       (append! (list x ...) schelog:*logic-variables*)))
	 . e)))))

;the unify predicate

(define schelog:unify
  (lambda (t1 t2)
    (lambda (fk)
      (letrec
	((cleanup-n-fail
	   (lambda (s)
	     (for-each schelog:unbind-ref! s)
	     (fk 'fail)))
	 (unify1
	   (lambda (t1 t2 s)
	     (cond ((eqv? t1 t2) s)
		   ((schelog:ref? t1)
		    (cond ((schelog:unbound-ref? t1)
			   (schelog:set-ref! t1 t2)
			   (cons t1 s))
			  ((schelog:frozen-ref? t1)
			   (cond ((schelog:ref? t2)
				  (cond ((schelog:unbound-ref? t2)
					 (schelog:set-ref! t2 t1)
					 (cons t2 s))
					((schelog:frozen-ref? t2)
					 (cleanup-n-fail s))
					(else
					  (unify1 t1 (schelog:deref t2) s))))
				 (else (cleanup-n-fail s))))
			  (else (unify1 (schelog:deref t1) t2 s))))
		   ((schelog:ref? t2) (unify1 t2 t1 s))
		   ((and (pair? t1) (pair? t2))
		    (unify1 (cdr t1) (cdr t2)
		      (unify1 (car t1) (car t2) s)))
		   ((and (vector? t1) (vector? t2))
		    (unify1 (vector->list t1)
		      (vector->list t2) s))
		   (else
		     (for-each schelog:unbind-ref! s)
		     (fk 'fail))))))
	(let ((s (unify1 t1 t2 '())))
	  (lambda (d)
	    (cleanup-n-fail s)))))))

(define == schelog:unify)

;disjunction

(define-syntax %or
  (syntax-rules ()
    ((%or g ...)
     (lambda (__fk)
       (call-with-current-continuation
	 (lambda (__sk)
	   (call-with-current-continuation
	     (lambda (__fk)
	       (__sk ((schelog:deref* g) __fk))))
	   ...
	   (__fk 'fail)))))))

;conjunction

(define-syntax %and
  (syntax-rules ()
    ((%and g ...)
     (lambda (__fk)
       (let* ((__fk ((schelog:deref* g) __fk))
	      ...)
	 __fk)))))

;cut

(define-syntax %cut
  (syntax-rules (!)
    ((%cut g)
     (lambda (__fk)
       (let ((! (lambda (__fk2) __fk)))
	 ((schelog:deref* g) __fk))))))

;Prolog-like sugar

(define-syntax rel
  (syntax-rules (!)
    ((rel (v ...) ((a ...) subgoal ...) ...)
     (lambda __fmls
       (lambda (__fk)
	 (letref (v ...)
	   (let ((! (lambda (fk1) __fk)))
	     (call-with-current-continuation
	       (lambda (__sk)
		 (call-with-current-continuation
		   (lambda (__fk)
		     (let* ((__fk ((schelog:unify __fmls (list a ...))
				   __fk))
			    (__fk ((schelog:deref* subgoal) __fk))
			    ...)
		       (__sk __fk))))
		 ...
		 (__fk 'fail))))))))))

;the fail and true preds

(define %fail
  (lambda (fk) (fk 'fail)))

(define %true
  (lambda (fk) fk))

;for structures ("functors"), use Scheme's list and vector
;functions and anything that's built using them.

;arithmetic

(define-syntax %is
  (syntax-rules (quote)
    ((%is v e)
     (lambda (__fk)
       ((== v (%is (1) e __fk)) __fk)))

    ((%is (1) (quote x) fk) (quote x))
    ((%is (1) (x ...) fk)
     ((%is (1) x fk) ...))
    ((%is (1) x fk)
     (if (and (schelog:ref? x) (schelog:unbound-ref? x))
	 (fk 'fail) (schelog:deref* x)))))

;defining arithmetic comparison operators

(define schelog:make-binary-arithmetic-relation
  (lambda (f)
    (lambda (x y)
      (%is #t (f x y)))))

(define %eq (schelog:make-binary-arithmetic-relation =))
(define %gt (schelog:make-binary-arithmetic-relation >))
(define %ge (schelog:make-binary-arithmetic-relation >=))
(define %lt (schelog:make-binary-arithmetic-relation <))
(define %le (schelog:make-binary-arithmetic-relation <=))
(define %ne (schelog:make-binary-arithmetic-relation
	      (lambda (m n) (not (= m n)))))

;type predicates

(define schelog:constant?
  (lambda (x)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x) #f)
		 ((schelog:frozen-ref? x) #t)
		 (else (schelog:constant? (schelog:deref x)))))
	  ((pair? x) #f)
	  ((vector? x) #f)
	  (else #t))))

(define schelog:compound?
  (lambda (x)
    (cond ((schelog:ref? x) (cond ((schelog:unbound-ref? x) #f)
			  ((schelog:frozen-ref? x) #f)
			  (else (schelog:compound? (schelog:deref x)))))
	  ((pair? x) #t)
	  ((vector? x) #t)
	  (else #f))))

(define %constant
  (lambda (x)
    (lambda (fk)
      (if (schelog:constant? x) fk (fk 'fail)))))

(define %compound
  (lambda (x)
    (lambda (fk)
      (if (schelog:compound? x) fk (fk 'fail)))))

;metalogical type predicates

(define schelog:var?
  (lambda (x)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x) #t)
		 ((schelog:frozen-ref? x) #f)
		 (else (schelog:var? (schelog:deref x)))))
	  ((pair? x) (or (schelog:var? (car x)) (schelog:var? (cdr x))))
	  ((vector? x) (schelog:var? (vector->list x)))
	  (else #f))))

(define %var
  (lambda (x)
    (lambda (fk) (if (schelog:var? x) fk (fk 'fail)))))

(define %nonvar
  (lambda (x)
    (lambda (fk) (if (schelog:var? x) (fk 'fail) fk))))

; negation of unify

(define schelog:make-negation ;basically inlined cut-fail
  (lambda (p)
    (lambda args
      (lambda (fk)
	(if (call-with-current-continuation
	      (lambda (k)
		((apply p args) (lambda (d) (k #f)))))
	    (fk 'fail)
	    fk)))))

(define %notunify
  (schelog:make-negation schelog:unify))

;identical

(define schelog:ident?
  (lambda (x y)
    (cond ((schelog:ref? x)
	   (cond ((schelog:unbound-ref? x)
		  (cond ((schelog:ref? y)
			 (cond ((schelog:unbound-ref? y) (eq? x y))
			       ((schelog:frozen-ref? y) #f)
			       (else (schelog:ident? x (schelog:deref y)))))
			(else #f)))
		 ((schelog:frozen-ref? x)
		  (cond ((schelog:ref? y)
			 (cond ((schelog:unbound-ref? y) #f)
			       ((schelog:frozen-ref? y) (eq? x y))
			       (else (schelog:ident? x (schelog:deref y)))))
			(else #f)))
		 (else (schelog:ident? (schelog:deref x) y))))
	  ((pair? x)
	   (cond ((schelog:ref? y)
		  (cond ((schelog:unbound-ref? y) #f)
			((schelog:frozen-ref? y) #f)
			(else (schelog:ident? x (schelog:deref y)))))
		 ((pair? y)
		  (and (schelog:ident? (car x) (car y))
		       (schelog:ident? (cdr x) (cdr y))))
		 (else #f)))
	  ((vector? x)
	   (cond ((schelog:ref? y)
		  (cond ((schelog:unbound-ref? y) #f)
			((schelog:frozen-ref? y) #f)
			(else (schelog:ident? x (schelog:deref y)))))
		 ((vector? y)
		  (schelog:ident? (vector->list x)
		    (vector->list y)))
		 (else #f)))
	  (else
	    (cond ((schelog:ref? y)
		   (cond ((schelog:unbound-ref? y) #f)
			 ((schelog:frozen-ref? y) #f)
			 (else (schelog:ident? x (schelog:deref y)))))
		  ((pair? y) #f)
		  ((vector? y) #f)
		  (else (eqv? x y)))))))

(define %ident
  (lambda (x y)
    (lambda (fk) (if (schelog:ident? x y) fk (fk 'fail)))))

(define %notident
  (lambda (x y)
    (lambda (fk) (if (schelog:ident? x y) (fk 'fail) fk))))

;variables as objects

(define schelog:freeze
  (lambda (s)
    (let ((dict '()))
      (let loop ((s s))
	(cond ((schelog:ref? s)
	       (cond ((or (schelog:unbound-ref? s) (schelog:frozen-ref? s))
		      (let ((x (assq s dict)))
			(if x (cdr x)
			    (let ((y (schelog:freeze-ref s)))
			      (set! dict (cons (cons s y) dict))
			      y))))
		     ;((schelog:frozen-ref? s) s) ;?
		     (else (loop (schelog:deref s)))))
	      ((pair? s) (cons (loop (car s)) (loop (cdr s))))
	      ((vector? s)
	       (list->vector (map loop (vector->list s))))
	      (else s))))))

(define schelog:melt
  (lambda (f)
    (cond ((schelog:ref? f)
	   (cond ((schelog:unbound-ref? f) f)
		 ((schelog:frozen-ref? f) (schelog:thaw-frozen-ref f))
		 (else (schelog:melt (schelog:deref f)))))
	  ((pair? f)
	   (cons (schelog:melt (car f)) (schelog:melt (cdr f))))
	  ((vector? f)
	   (list->vector (map schelog:melt (vector->list f))))
	  (else f))))

(define schelog:melt-new
  (lambda (f)
    (let ((dict '()))
      (let loop ((f f))
	(cond ((schelog:ref? f)
	       (cond ((schelog:unbound-ref? f) f)
		     ((schelog:frozen-ref? f)
		      (let ((x (assq f dict)))
			(if x (cdr x)
			    (let ((y (schelog:make-ref)))
			      (set! dict (cons (cons f y) dict))
			      y))))
		     (else (loop (schelog:deref f)))))
	      ((pair? f) (cons (loop (car f)) (loop (cdr f))))
	      ((vector? f)
	       (list->vector (map loop (vector->list f))))
	      (else f))))))

(define schelog:copy
  (lambda (s)
    (schelog:melt-new (schelog:freeze s))))

(define %freeze
  (lambda (s f)
    (lambda (fk)
      ((schelog:unify (schelog:freeze s) f) fk))))

(define %melt
  (lambda (f s)
    (lambda (fk)
      ((schelog:unify (schelog:melt f) s) fk))))

(define %melt-new
  (lambda (f s)
    (lambda (fk)
      ((schelog:unify (schelog:melt-new f) s) fk))))

(define %copy
  (lambda (s c)
    (lambda (fk)
      ((schelog:unify (schelog:copy s) c) fk))))

;negation as failure

(define %not
  (lambda (g)
    (lambda (fk)
      (if (call-with-current-continuation
	    (lambda (k)
	      ((schelog:deref* g) (lambda (d) (k #f)))))
	  (fk 'fail) fk))))

;set predicates

(define schelog:set-cons
  (lambda (e s)
    (if (member e s) s (cons e s))))

(define schelog:find-unbounds
  (lambda (e1 . e2)
    (let ((s '()))
      (let loop ((e e2))
	(cond ((null? e) 'skip)
	      ((pair? e)
	       (loop (car e))
	       (loop (cdr e)))
	      ((schelog:ref? e)
	       (cond ((schelog:unbound-ref? e)
		      (if (memq e s) 'skip
			(set! s (cons e s))))
		     ((schelog:frozen-ref? e) 'skip)
		     (else (loop (schelog:deref e)))))
	      ((vector? e)
	       (let ((n (vector-length e)))
		 (do ((i 0 (+ i 1)))
		     ((>= i n))
		     (loop (vector-ref e i)))))
	      (else 'skip)))
      (let ((r '()))
	(let loop ((e e1))
	  (cond ((null? e) 'skip)
		((pair? e)
		 (loop (car e))
		 (loop (cdr e)))
		((schelog:ref? e)
		 (cond ((schelog:unbound-ref? e)
			(if (or (memq e s) (memq e r)) 'skip
			  (set! r (cons e r))))
		       ((schelog:frozen-ref? e) 'skip)
		       (else (loop (schelog:deref e)))))
		((vector? e)
		 (let ((n (vector-length e)))
		   (do ((i 0 (+ i 1)))
		       ((>= i n))
		       (loop (vector-ref e i)))))
		(else 'skip)))
	r))))

(define schelog:restore-unbounds
  (lambda (s)
    (for-each schelog:unbind-ref! s)))

(define schelog:make-bag-of
  (lambda (kons)
    (lambda (lv fvv goal bag)
      (lambda (fk)
	(let* ((uu (schelog:find-unbounds fvv lv bag))
	       (ans
		 (call-with-current-continuation
		   (lambda (ans-k)
		     (let* ((bag '())
			    (fk-final
			      (lambda (d)
				(let ((s (reverse! bag)))
				  ;merely to gc
				  (set! bag '())
				  (ans-k s))))
			    (fk (goal fk-final)))
		       (set! bag (kons (schelog:deref* lv) bag))
		       (fk 'retry))))))
	  (schelog:restore-unbounds uu)
	  ((schelog:unify ans bag) fk))))))

(define schelog:bag-of (schelog:make-bag-of cons))
(define schelog:set-of (schelog:make-bag-of schelog:set-cons))

(define-syntax %bag-of
  (syntax-rules ()
    ((%bag-of x goal bag)
     (schelog:bag-of x schelog:*logic-variables* goal bag))))

(define-syntax %set-of
  (syntax-rules ()
    ((%set-of x goal set)
     (schelog:set-of x schelog:*logic-variables* goal set))))

'(define-syntax %exists
  (syntax-rules ()
    ((%exists y goal)
     (%and (letref (__tmp)
	     (%bag-of y goal __tmp)) %true))))

;but this is probably cheaper

(define-syntax %exists
  (syntax-rules ()
    ((%exists y goal)
     (let ((y (schelog:copy y))) goal))))

;%bag-of-1, %set-of-1 hold if there's at least one solution

(define-syntax %bag-of-1
  (syntax-rules ()
    ((%bag-of-1 x g ii)
     (%and (%bag-of x g ii)
       (schelog:unify ii (cons (_) (_)))))))

(define-syntax %set-of-1
  (syntax-rules ()
    ((%set-of-1 x g ii)
     (%and (%set-of x g ii)
       (schelog:unify ii (cons (_) (_)))))))

;user interface

;(which (v ...) query) returns #f if query fails and instantiations
;of v ... if query succeeds.  In the latter case, type (more) to
;retry query for more instantiations.

(define schelog:*more-k* 'forward)
(define schelog:*more-fk* 'forward)

(define-syntax which
  (syntax-rules (schelog:*more-k* schelog:*more-fk*)
    ((which (v ...) g)
     (letref (v ...)
       (call-with-current-continuation
         (lambda (__qk)
           (set! schelog:*more-k* __qk)
           (set! schelog:*more-fk*
             ((schelog:deref* g)
              (lambda (d)
                (set! schelog:*more-fk* #f)
                (schelog:*more-k* #f))))
           (schelog:*more-k*
             (map (lambda (nam val) (list nam (schelog:deref* val)))
                  '(v ...)
                  (list v ...)))))))))

(define more
  (lambda ()
    (call-with-current-continuation
      (lambda (k)
	(set! schelog:*more-k* k)
	(if schelog:*more-fk* (schelog:*more-fk* 'more)
	  #f)))))

;end of embedding code.  The following are
;some utilities, written in Schelog

(define %member
  (lambda (x y)
    (letref (xs z zs)
      (%or
	(== y (cons x xs))
	(%and (== y (cons z zs))
	  (%member x zs))))))

(define %if-then-else
  (lambda (p q r)
    (%cut
      (%or
	(%and p ! q)
	r))))

;the above could also have been written in a more
;Prolog-like fashion, viz.

'(define %member
  (rel (x xs y ys)
    ((x (cons x xs)))
    ((x (cons y ys)) (%member x ys))))

'(define %if-then-else
  (rel (p q r)
    ((p q r) p ! q)
    ((p q r) r)))
