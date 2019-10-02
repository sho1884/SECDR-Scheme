;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; logic variables and their manipulation

(define **unbound** (make-temp-symbol "_"))
(define new-box (lambda () (box **unbound**)))
(define unbind-box! (lambda (b) (set-box! b **unbound**) #f))
(define unbound-box?
  (lambda (b) (and (box? b) (eq? (unbox b) **unbound**))))

(define **frozen** (make-temp-symbol "@"))
(define freeze-box (lambda (b) (box (cons **frozen** b))))
(define thaw-box (lambda (b) (cdr (unbox b))))
(define frozen-box?
  (lambda (b) (and (box? b)
		   (let ([c (unbox b)])
		     (and (pair? c) (eq? (car c) **frozen**))))))

(define derefl
  (lambda (x)
    (cond [(unbound-box? x) (unbox x)]
	  [(frozen-box? x) x]
	  [(box? x) (derefl (unbox x))]
	  [(pair? x) (let ([a (car x)] [d (cdr x)])
		       (let ([a1 (derefl a)] [d1 (derefl d)])
			 (if (and (eq? a a1) (eq? d d1)) x
			   (cons a1 d1))))]
	  [else x])))

;;; letref introduces new logic-variables

(extend-syntax (letref)
  [(letref (x ...) e ...) (let ([x (new-box)] ...) e ...)])

;;; call/cfc is to command contns as call/cc is to expression contns

(define call/cfc
  (lambda (f)
    (call/cc (lambda (k) (f (lambda () (k '<call/cfc>)))))))

;;; the unify predicate, called = in Prolog

(define %unify
  (lambda (t1 t2)
    (lambda (fk)
      (letrec
	([unify1 (lambda (t1 t2 s)
		   (cond [(eqv? t1 t2) s]
			 [(box? t1)
			  (if (unbound-box? t1)
			      (begin (set-box! t1 t2) (cons t1 s))
			      (unify1 (unbox t1) t2 s))]
			 [(box? t2) (unify1 t2 t1 s)]
			 [(and (pair? t1) (pair? t2))
			  (unify1 (cdr t1) (cdr t2)
			    (unify1 (car t1) (car t2) s))]
			 [else (for-each unbind-box! s) (fk)]))])
	(let ([s (unify1 t1 t2 [])])
	  (lambda () (for-each unbind-box! s) (fk)))))))

;;; rel introduces relations, which are bunches of related rules

(extend-syntax (rel !) 
  [(rel (b ...) [(a ...) p ...] ...)
   (lambda __fmls
     (lambda (__fk)
       (letref (b ...)
	 (let ([! (lambda (fk1) __fk)])
	   (call/cc
	     (lambda (__sk)
	       (call/cfc
		 (lambda (__fk)
		   (let* ([__fk ((%unify __fmls (list a ...)) __fk)]
			  [__fk ((derefl p) __fk)] ...)
		     (__sk __fk))))
	       ...
	       (__fk)))))))])

;;; the fail and true predicates

(define %fail (lambda (fk) (fk)))
(define %true (lambda (fk) fk))

;;; introduces new structure-builders; cons is built-in and is the same
;;; as Scheme's cons

(extend-syntax (define-functor) 
  [(define-functor functor comp ...)
   (define functor (lambda (comp ...) (cons 'functor (list comp ...))))])

;;; use (_) for creating anonymous variables a la Prolog's _

(define _ new-box)

;;; arithmetic, Prolog's is

(extend-syntax (%is quote) 
  [(%is[1] (quote x) fk) (quote x)]
  [(%is[1] (x ...) fk) ((%is[1] x fk) ...)]
  [(%is[1] x fk) (if (unbound-box? x) (fk) (derefl x))]
  [(%is v e) (lambda (__fk) ((%unify v (%is[1] e __fk)) __fk))])

;;; defining arithmetic comparison operators

(define *bin-arith-rel
  (lambda (f)
    (rel (x y) [(x y) (%is #t (f x y))])))

(define %eq (*bin-arith-rel =))
(define %gt (*bin-arith-rel >))
(define %ge (*bin-arith-rel >=))
(define %lt (*bin-arith-rel <))
(define %le (*bin-arith-rel <=))
(define %ne (*bin-arith-rel (lambda (m n) (not (= m n)))))

;;; type predicates

(define %%constant
  (lambda (x)
    (cond [(unbound-box? x) #f]
	  [(frozen-box? x) #t]
	  [(box? x) (%%constant (unbox x))]
	  [(pair? x) #f]
	  [else #t])))

(define %%compound
  (lambda (x)
    (cond [(unbound-box? x) #f]
	  [(frozen-box? x) #f]
	  [(box? x) (%%compound (unbox x))]
	  [(pair? x) #t]
	  [else #f])))

(define %constant
  (lambda (x)
    (lambda (fk) (if (%%constant x) fk (fk)))))

(define %compound
  (lambda (x)
    (lambda (fk) (if (%%compound x) fk (fk)))))

;;; metalogical type predicates

(define %%var
  (lambda (x)
    (cond [(unbound-box? x) #t]
	  [(frozen-box? x) #f]
	  [(box? x) (%%var (unbox x))]
	  [(pair? x) (or (%%var (car x)) (%%var (cdr x)))]
	  [else #f])))
	    
(define %%nonvar (lambda (x) (not (%%var x))))

(define %var
  (lambda (x)
    (lambda (fk) (if (%%var x) fk (fk)))))

(define %nonvar
  (lambda (x)
    (lambda (fk) (if (%%var x) (fk) fk))))

;;; %unify

(define *negation ; basically inlined cut-fail
  (lambda (p)
    (lambda aa
      (lambda (fk)
	(if (call/cc
	      (lambda (k)
		((apply p aa) (lambda () (k #f))))) (fk) fk)))))

(define %notunify (*negation %unify))

(define %%ident
  (lambda (x y)
    (cond [(unbound-box? x)
	   (cond [(unbound-box? y) (eq? x y)]
		 [(frozen-box? y) #f]
		 [(box? y) (%%ident x (unbox y))]
		 [(pair? y) #f]
		 [else #f])]
	  [(frozen-box? x)
	   (cond [(unbound-box? y) #f]
		 [(frozen-box? y) (eq? x y)]
		 [(box? y) (%%ident x (unbox y))]
		 [(pair? y) #f]
		 [else #f])]
	  [(box? x) (%%ident (unbox x) y)]
	  [(pair? x)
	   (cond [(unbound-box? y) #f]
		 [(frozen-box? y) #f]
		 [(box? y) (%%ident x (unbox y))]
		 [(pair? y) (and (%%ident (car x) (car y))
				 (%%ident (cdr x) (cdr y)))]
		 [else #f])]
	  [else
	    (cond [(unbound-box? y) #f]
		  [(frozen-box? y) #f]
		  [(box? y) (%%ident x (unbox y))]
		  [(pair? y) #f]
		  [else (eqv? x y)])])))

(define %ident
  (lambda (x y)
    (lambda (fk) (if (%%ident x y) fk (fk)))))

(define %notident
  (lambda (x y)
    (lambda (fk) (if (%%ident) (fk) fk))))

;;; variables as objs

(define %%freeze
  (lambda (t)
    (let ([dict []])
      (recur loop ([t t])
	(cond [(unbound-box? t)
	       (let ([x (assq t dict)])
		 (if x (cdr x)
		     (let ([y (freeze-box t)])
		       (set! dict (cons (cons t y) dict))
		       y)))]
	      [(frozen-box? t) t]
	      [(box? t) (loop (unbox t))]
	      [(pair? t) (cons (loop (car t)) (loop (cdr t)))]
	      [else t])))))

(define %%melt
  (lambda (f)
    (cond [(unbound-box? f) f]
	  [(frozen-box? f) (thaw-box f)]
	  [(box? f) (%%melt (unbox f))]
	  [(pair? f) (cons (%%melt (car f)) (%%melt (cdr f)))]
	  [else f])))
    

(define %%melt-new
  (lambda (f)
    (let ([dict []])
      (recur loop ([f f])
	(cond [(unbound-box? f) f]
	      [(frozen-box? f)
	       (let ([x (assq f dict)])
		 (if x (cdr x)
		     (let ([y (new-box)])
		       (set! dict (cons (cons f y) dict))
		       y)))]
	      [(box? f) (loop (unbox f))]
	      [(pair? f) (cons (loop (car f)) (loop (cdr f)))]
	      [else f])))))

(define %%copy (lambda (t) (%%melt-new (%%freeze t))))

(define %freeze
  (lambda (t f)
    (lambda (fk)
      ((%unify
	 f (%%freeze t)) fk))))

(define %melt
  (lambda (f t)
    (lambda (fk)
      ((%unify
	 t (%%melt f)) fk))))

(define %melt-new
  (lambda (f t)
    (lambda (fk)
      ((%unify
	 t (%%melt-new f)) fk))))

'(define %copy
  (rel (term copy frozen)
    [(term copy) (%freeze term frozen) (%melt-new frozen copy)]))

(define %copy
  (lambda (t c)
    (lambda (fk)
      ((%unify c (%%copy t)) fk))))

;;; negation as failure

'(define %not (rel (x) [(x) x ! %fail] [(x)]))

(define %not
  (lambda (g)
    (lambda (fk)
      (if (call/cc
	    (lambda (k)
	      ((derefl g) (lambda () (k #f))))) (fk) fk))))

;;; metavariable facility

(define %call ; for completeness, i guess?
  (lambda (g)
    (lambda (fk)
      ((recur loop ([g g])
	 (cond [(unbound-box? g) (fk)]
	       [(frozen-box? g) (error '%call "~a" g)]
	       [(box? g) (loop (unbox g))]
	       [(pair? g) (error '%call "~a" g)]
	       [(procedure? g) g]
	       [else (error '%call "~a" g)])) fk))))

(define %or
  (lambda gg
    (lambda (fk)
      (call/cc
	(lambda (sk)
	  (recur loop ([gg gg])
	    (if (null? gg) (fk)
	      (begin
		(call/cfc
		  (lambda (fk)
		    (sk ((derefl (car gg)) fk))))
		(loop (cdr gg))))))))))

(define %and
  (lambda gg
    (lambda (fk)
      (recur loop ([gg gg] [fk fk])
	(if (null? gg) fk
	  (loop (cdr gg) ((derefl (car gg)) fk)))))))

;;; usual Prolog interface

(define *prolog-more-prompt* " ;                                      more? ")

(extend-syntax (which)
  [(which (b ...) p ...)
   (letref (b ...)
     (call/cc
       (lambda (__k)
	 (let* ([__fk (lambda () (__k #f))]
		[__fk ((derefl p) __fk)] ...)
	   (for-each (lambda (var val)
		       (printf " ~a = " var)
		       (pretty-print (derefl val)))
	     '(b ...) (list b ...))
	   (display #t)
	   (recur loop ([__fk __fk])
	     (case (prompt-read *prolog-more-prompt*)
	       [(n no) #f]
	       [else (loop (__fk))]))))))])

;;; side-effects: assert[az], retract? not put in. Use Scheme for
;;; nicer side-effect interface

;;; utilities, written in Prolog

(define %member
  (rel (x x... y y...)
    [(x [cons x x...])]
    [(x [cons y y...]) (%member x y...)]))

(define %ifthenelse
  (rel (p q r)
    [(p q r) p ! q]
    [(p q r) r]))
