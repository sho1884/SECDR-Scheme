;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; set predicates

(define %%copy-back
  (lambda (l r)
    (if (eq? l r) #t
      (cond [(unbound-box? l) (set-box! l (derefl r))]
	    [(frozen-box? l) (error 'copy-back "")]
	    [(box? l) (%%copy-back (unbox l) r)]
	    [(pair? l)
	     (if (pair? r)
		 (begin (%%copy-back (car l) (car r))
			(%%copy-back (cdr l) (cdr r)))
		 (error 'copy-back ""))]
	    [else (error 'copy-back "")]))))

(define %%set-back
  (lambda (l r)
    (cond [(eq? l r) #t]
	  [(and (box? r) (not (frozen-box? r)))
	   (%%set-back l (unbox r))]
	  [else
	    (cond [(unbound-box? l) (set-box! l r)]
		  [(frozen-box? l) (error 'set-back "")]
		  [(box? l) (set-box! l r)]
		  [(pair? l) (if (pair? r)
				 (begin (%%set-back (car l) (car r))
					(%%set-back (cdr l) (cdr r)))
				 (error 'set-back ""))]
		  [else (error 'set-back "")])])))

(define *$bagof ; hacky? naaah!!
  (lambda (kons)
    (lambda (t f g s)
      (lambda (fk)
	(let* ([f (remq! t f)]
	       [f-tmp (map %%copy f)]
	       [fk^ #f]
	       [f-prev (map derefl f)]
	       [r (call/cc
		    (lambda (k)
		      (let* ([initial? #t] [s []]
			     [fk* (lambda () (k (begin0 (reverse! s)
						  (set! s []))))]
			     [fk
			       ((%and
				  g (lambda (fk)
				      (let ([f-curr (map derefl f)])
					(cond
					  [initial?
					    (set! initial? #f)
					    (set! f-prev f-curr)
					    fk]
					  [(andmap equal? f-prev f-curr)
					   fk]
					  [else
					    (call/cfc
					      (lambda (fk-new)
						(when fk^ (error 'iio ""))
						(set! fk^ fk-new)
						(fk*)))
					    (set! f-prev f-curr)
					    fk])))) fk*)])
			(set! s (kons (derefl t) s))
			(fk))))])
	  (for-each %%set-back f-tmp f-prev)
	  (for-each %%set-back f f-tmp)
	  ((%unify s r)
	   (lambda ()
	     (if fk^ ((begin0 fk^ (set! fk^ #f))) (fk)))))))))

(define $bagof (*$bagof cons))

(define set-cons (lambda (e s) (if (member e s) s (cons e s))))
(define set-union
  (lambda (s1 s2)
    (if (null? s1) s2
      (set-union (cdr s1) (set-cons (car s1) s2)))))
(define $setof (*$bagof set-cons))

(define atoms-of
  (lambda (x)
    (recur L ([x x] [r ()] [s ()])
      (cond [(pair? x)
	     (record-case x
	       [quote (x) s]
	       [lambda (vv . b) (L b (set-union (atoms-of vv) r) s)]
	       [set! (x a) (L a r (L x r s))]
	       [else (L (cdr x) r (L (car x) r s))])]
	    [(symbol? x) (set-cons x s)]
	    [else s]))))

(define free-vars-among (lambda (s) (cull! %%var s)))

(extend-syntax (%exists)
  [(%exists y g) (let ([y (%%copy y)]) g)])

(extend-syntax (%bagof)
  [(%bagof x g s)
   (with ([atoms-of-g (cons 'list (atoms-of (expand 'g)))])
     ($bagof x (free-vars-among atoms-of-g) g s))])

(extend-syntax (%setof)
  [(%setof x g s)
   (with ([atoms-of-g (cons 'list (atoms-of (expand 'g)))])
     ($setof x (free-vars-among atoms-of-g) g s))])

