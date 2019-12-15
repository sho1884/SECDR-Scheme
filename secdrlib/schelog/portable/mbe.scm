;mbe.scm
;Dorai Sitaram, dorai@cs.rice.edu, 1991; revised Sept. 3, 1992;
;revised Dec. 6, 1993 to r4rs syntax (if not semantics);
;added hygiene filter Dec. 11, 1993

;A vanilla implementation of Macro-by-Example (Eugene
;Kohlbecker, r4rs).  This file requires defmacro.  (If
;your Scheme has some primitive macro defining
;facility, please define defmacro in terms of it before
;loading this file.)

;Use: getting r4rs's high-level
;define-syntax/syntax-rules for your Scheme _cheaply_.
;With this, you
;
;(a) can define macros that use ...  (dot-dot-dot);
;
;(b) needn't worry about a lexical variable in a macro
;definition clashing with a variable from the macro use
;context;
;
;(c) don't suffer the overhead of redefining the repl,
;because you use the underlying defmacro.

;Simply load this file into Scheme to get define-syntax.

;Caveat: These macros are not referentially transparent
;(r4rs, p.  40).  Lexically scoped macros (i.e., let-
;and letrec-syntax) are not supported.  In any case,
;the problem of referential transparency gains
;particular poignancy only when let- and letrec-syntax
;are used.  So: you will not be courting large-scale
;disaster unless you're using system-function names as
;local variables with unintuitive bindings that the
;macro can't use.  However, if you _must_ have the full
;r4rs functionality, look into the more featureful (but
;also more expensive) versions of syntax-rules
;available in slib and nexus.

;Load hygiene filter:
;Insert hygiene.scm's contents or use full pathname if you
;don't have program-vicinity

(load (string-append (program-vicinity) "hygiene.scm"))

;Uncomment following if you don't want hygiene

;(define hyg:tag-generic
;  (lambda (e kk tmps) e))

(define mbe:there-exists?
  (lambda (s p)
    (let loop ((s s))
      (if (null? s) #f
	(or (p (car s)) (loop (cdr s)))))))

(define mbe:for-all?
  (lambda (s p)
    (let loop ((s s))
      (if (null? s) #t
	(and (p (car s)) (loop (cdr s)))))))

(define mbe:sublist
  (lambda (l i f)
    ;finds the sublist of l from index i inclusive to index f exclusive
    (let loop ((l (list-tail l i)) (k i) (r '()))
      (cond ((>= k f) (reverse r))
	    ((pair? l) (loop (cdr l) (+ k 1) (cons (car l) r)))
	    (else (error 'mbe:sublist 'arg-too-short))))))

(define mbe:position
  (lambda (x l)
    ;finds the leftmost index of list l where something equal to x
    ;occurs
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
	    ((equal? (car l) x) i)
	    (else (loop (cdr l) (+ i 1)))))))

(define mbe:matches-pattern?
  (lambda (p e k)
    ;tests if expression e matches pattern p where k is the list of
    ;keywords
    (cond ((mbe:ellipsis? p)
	   (and (or (null? e) (pair? e))
		(let* ((p-head (car p))
		       (p-tail (cddr p))
		       (e-head=e-tail (mbe:split-at-ellipsis e p-tail)))
		  (and e-head=e-tail
		       (let ((e-head (car e-head=e-tail))
			     (e-tail (cdr e-head=e-tail)))
			 (and (mbe:for-all? e-head
				(lambda (x)
				  (mbe:matches-pattern? p-head x k)))
			      (mbe:matches-pattern? p-tail e-tail k)))))))
	  ((pair? p)
	   (and (pair? e)
		(mbe:matches-pattern? (car p) (car e) k)
		(mbe:matches-pattern? (cdr p) (cdr e) k)))
	  ((symbol? p) (if (memq p k) (eq? p e) #t))
	  (else (equal? p e)))))

(define mbe:get-bindings
  (lambda (p e k)
    ;gets the bindings of pattern variables of pattern p for
    ;expression e;
    ;k is the list of keywords
    (cond ((mbe:ellipsis? p)
	   (let* ((p-head (car p))
		  (p-tail (cddr p))
		  (e-head=e-tail (mbe:split-at-ellipsis e p-tail))
		  (e-head (car e-head=e-tail))
		  (e-tail (cdr e-head=e-tail)))
	     (cons (cons (mbe:get-ellipsis-nestings p-head k)
		     (map (lambda (x) (mbe:get-bindings p-head x k))
			  e-head))
	       (mbe:get-bindings p-tail e-tail k))))
	  ((pair? p)
	   (append (mbe:get-bindings (car p) (car e) k)
	     (mbe:get-bindings (cdr p) (cdr e) k)))
	  ((symbol? p)
	   (if (memq p k) '() (list (cons p e))))
	  (else '()))))

(define mbe:expand-pattern
  (lambda (p r k)
    ;expands pattern p using environment r;
    ;k is the list of keywords
    (cond ((mbe:ellipsis? p)
	   (append (let* ((p-head (car p))
			  (nestings (mbe:get-ellipsis-nestings p-head k))
			  (rr (mbe:ellipsis-sub-envs nestings r)))
		     (map (lambda (r1)
			    (mbe:expand-pattern p-head (append r1 r) k))
			  rr))
	     (mbe:expand-pattern (cddr p) r k)))
	  ((pair? p)
	   (cons (mbe:expand-pattern (car p) r k)
	     (mbe:expand-pattern (cdr p) r k)))
	  ((symbol? p)
	   (if (memq p k) p
	     (let ((x (assq p r)))
	       (if x (cdr x) p))))
	  (else p))))

(define mbe:get-ellipsis-nestings
  (lambda (p k)
    ;returns a list that nests a pattern variable as deeply as it
    ;is ellipsed
    (let sub ((p p))
      (cond ((mbe:ellipsis? p) (cons (sub (car p)) (sub (cddr p))))
	    ((pair? p) (append (sub (car p)) (sub (cdr p))))
	    ((symbol? p) (if (memq p k) '() (list p)))
	    (else '())))))

(define mbe:ellipsis-sub-envs
  (lambda (nestings r)
    ;finds the subenvironments in r corresponding to the ellipsed
    ;variables in nestings
    (mbe:there-exists? r
      (lambda (c)
	(if (mbe:contained-in? nestings (car c)) (cdr c) #f)))))

(define mbe:contained-in?
  (lambda (v y)
    ;checks if nestings v and y have an intersection
    (if (or (symbol? v) (symbol? y)) (eq? v y)
      (mbe:there-exists? v
	(lambda (v_i)
	  (mbe:there-exists? y
	    (lambda (y_j)
	      (mbe:contained-in? v_i y_j))))))))

(define mbe:split-at-ellipsis
  (lambda (e p-tail)
    ;split expression e so that its second half matches with
    ;pattern p-tail
    (if (null? p-tail) (cons e '())
      (let ((i (mbe:position (car p-tail) e)))
	(if i (cons (mbe:sublist e 0 i) (list-tail e i))
	    (error 'mbe:split-at-ellipsis 'bad-arg))))))

(define mbe:ellipsis?
  (lambda (x)
    ;tests if x is an ellipsing pattern, i.e., of the form
    ;(blah ... . blah2)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

;define-syntax

(defmacro define-syntax (macro-name syn-rules)
  (if (or (not (pair? syn-rules))
	(not (eq? (car syn-rules) 'syntax-rules)))
      (error 'define-syntax 'not-an-r4rs-high-level-macro
	macro-name syn-rules)
      (let ((keywords (cons macro-name (cadr syn-rules)))
	    (clauses (cddr syn-rules)))
	`(defmacro ,macro-name macro-arg
	   (let ((macro-arg (cons ',macro-name macro-arg))
		 (keywords ',keywords))
	     (cond ,@(map
		       (lambda (clause)
			 (let ((in-pattern (car clause))
			       (out-pattern (cadr clause)))
			   `((mbe:matches-pattern? ',in-pattern macro-arg
			       keywords)
			     (hyg:tag-generic
			       (mbe:expand-pattern ',out-pattern
				 (mbe:get-bindings ',in-pattern macro-arg
				   keywords)
				 keywords)
                               (append!
                                 (hyg:flatten ',in-pattern)
                                 keywords)
                               '()))))
		       clauses)
	       (else (error ',macro-name 'no-matching-clause
		       ',clauses))))))))

;eof
