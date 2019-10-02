
;;; fixedpoint.scm

;;;;    for SECDR Scheme
;;;     Date revised    11-August-1992 by Shoichi Hayashi

; Date: 15 Nov 88 23:03:24 GMT
; From: uoregon!markv@beaver.cs.washington.edu  (Mark VandeWettering)
; Organization: University of Oregon, Computer Science, Eugene OR
; Subject: The Paradoxical Combinator -- Y (LONG)
; 
; Alternatively entitled:
; 	"Y?  Why Not?" :-)
; 
; The discussion that has been going on in regards to the Y combinator as
; the basic operation in implementing recursive functions are interesting.
; The practical tests that people have made have shown that the Y
; combinator is orders of magnitude slower for implementing recursion than
; directly compiling it.
; 
; This is true for Scheme.  I hold that for an interesting set of
; languages, (lazy languages) that this result will not necessarily hold.
; 
; The problem with Y isn't its complexity, it is the fact that it is an
; inherently lazy operation.  Any implementation in Scheme is clouded by
; the fact that Scheme is an applicative order evaluator, while Y prefers
; to be evaluated in normal order.
; 
; 	
 (define Y
   (lambda (g)	
     ((lambda (h) (g (lambda (x) ((h h) x))))
      (lambda (h) (g (lambda (x) ((h h) x)))))))
; 
 (define fact
   (lambda (f)
     (lambda (n)
       (if (= n 1)
 	  1
 	(* n (f (- n 1)))))))
; 
; 
; Evaluating (Y fact) 2 results in the following operations in
; Scheme:
; 
; The argument is (trivially) evaluated, and returns two.
; (Y fact) must be evaluated.  What is it?  Y and fact each evaluate
; to closures.  When applied, Y binds g to fact, and executes the
; body.
; 
; The body is an application of a closure to another closure.  The
; operator binds h to the operand, and executes its body which....
; 
; Evaluates (g (lambda (x) ((h h) x))).  The operand is a closure,
; which gets built and then returns.  g evaluates to fact.  We
; substitute the closure (lambda (x) ((h h) x)) in for the function
; f in the definition of fact, giving...
; 
; (lambda (n)
;   (if (= n 1) 
;       1
;     (* n ((lambda (x) ((h h) x)) (- n 1)))))
; 
; Which we return as the value of (Y fact).  When we apply this to 2, we get
; 
; (* 2 ((lambda (x) ((h h) x)) 1))
; 
; We then have to evaluate
; ((lambda (x) ((h h) x)) 1)
; 
; or 
; ((h h) 1)
; 
; But remembering that h was (lambda (h) (g (lambda (x) ((h h) x)))), 
; we have 
; 
; (((lambda (h) (g (lambda (x) ((h h) x))))
;   (lambda (h) (g (lambda (x) ((h h) x)))))
;  1) ....
; 
; So, we rebind h to be the right stuff, and evaluate the body, which is
; 
; ((g (lambda (x) ((h h) x))) 1)
; 
; Which by the definition of g (still == fact) is just 1.
; 
; (* 2 1) = 2.
; 
; ########################################################################
; 
; Summary:  If you didn't follow this, performing this evaluation
; was cumbersome at best.  As far as compiler or interpreter is
; concerned, the high cost of evaluating this function is related
; to two different aspects:
; 
; It is necessary to create "suspended" values.  These suspended
; values are represented as closures, which are in general heap
; allocated and expensive.
; 
; For every level of recursion, new closures are created (h gets
; rebound above).  While this could probably be optimized out by a
; smart compiler, it does seem like the representation of suspended
; evaluation by lambdas is inefficient.
; 
; 	   
; ########################################################################
; 
; You can try to figure out how all this works.  It is complicated, I
; believe I understand it.  The point in the derivation above is that in
; Scheme, to understand how the implementation of Y works, you have to
; fall back on the evaluation mechanism of Scheme.  Suspended values must
; be represented as closures.  It is the creation of these closures that
; cause the Scheme implementation to be slow.
; 
; If one wishes to abandon Scheme (or at least applicative order
; evaluators of Scheme) one can typically do much better.  My thesis work
; is in graph reduction, and trying to understand better the issues having
; to do with implementation.
; 
; In graph reduction, all data items (evaluated and unevaluated) have the
; same representation: as graphs in the heap.  We choose to evaluate using
; an outermost, leftmost strategy.  This allows the natural definition of
; (Y h) = (h (Y h)) to be used.  An application node of the form:
; 
; 			    @
; 			   / \
; 			  /   \
; 			 Y     h
; 
; can be constructed in the obvious way:
;                           @
; 			   / \
; 			  /   \
; 			 h     @
; 			      /	\
; 			     /   \
; 			    Y     h
; 
; costing one heap allocation per level of recursion, which is
; certainly cheaper than the multiple allocations of scheme
; closures above.  More efficiently, we might choose to implement
; it using a "knot tying" version:
; 
; 
; 			      /\
;                            /  \
; 			    @	 |
; 			   / \ 	/
; 			  /   \/
; 			 h
; 
; Which also works quite well.  Y has been eliminated, and will
; cause no more reductions.  
; 
; The basic idea is somehow that recursion in functional languages
; is analogous to cycles in the graph in a graph reduction engine.
; Therefore, the Y combinator is a specific "textual" indicator of
; the graph.
; 
; The G-machine (excellently described in Peyton Jones' book "The
; Implementation of Functional Programming Languages") also
; described the Y combinator as being efficient.  He chose letrecs
; as being a primitive in the extended lambda calculus.  His
; methodology behind compiling these recursive definitions was
; basically to compile fixed code which directly built these cyclic
; structures, rather than having them built at runtime.
; 
; I think (and my thesis work is evolving into this kind of
; argument) that Y is overlooked for trivial reasons.  Partial
; evaluation and smarter code generation could make an SK based
; compiler generate code which is equal in quality to that produced
; by supercombinator based compilation.
; 
; 
; This is too long already, ciao for now.
; 
; Mark VandeWettering

(write ((Y fact) 10))
(newline)

; From sven%arti12.vub.ac.be@mitvma.mit.EDU Mon Oct 28 14:42:14 1991
; From: sven%arti12.vub.ac.be@mitvma.mit.EDU
; Newsgroups: comp.lang.scheme
; Subject: Y Operator
; Date: 25 Oct 91 02:15:34 GMT
; Organization: The Internet

; Does anyone know of an elgant implementation of the Y
; operator for multiple (mutually and/or self recursive) functions
; in standard Scheme ?

; Let me first set the context:

; standard definition of Y for 1 function is:

; (a) for functions of one argument:

;     (define (Y M)
;       ((lambda (future)
;          (M (lambda (x)
;               ((future future) x))))
;        (lambda (future)
;          (M (lambda (x)
;               ((future future) x))))))

;     or (equivalent)

;     (define (Y maker)
;       (let ((future (lambda (f)
;                       (maker (lambda (x)
;                                ((f f) x))))))
;         (maker (lambda (x)
;                  ((future future) x)))))

;     or (equivalent)

;     (define (Y maker)
;       (let ((G (lambda (future)
;                  (maker (lambda (x) ((future future) x))))))
;         (G G)))

; (b) for functions of any number of arguments:

;     (define (Y M)
;       ((lambda (future)
;          (M (lambda args
;               (apply (future future) args))))
;        (lambda (future)
;          (M (lambda args
;               (apply (future future) args))))))

;     or (equivalent)

;     (define (Y maker)
;       (let ((future (lambda (f)
;                       (maker (lambda args
;                                (apply (f f) args))))))
;         (maker (lambda args
;                  (apply (future future) args)))))

;     or (equivalent)

    (define (Y maker)
      (let ((G (lambda (future)
                 (maker (lambda args (apply (future future) args))))))
        (G G)))


; Now you have the following:

; identity: (Y f) = (f (Y f))

; a simple letrec can be converted to a Y call (version b):

; (define (FAC n)
;   (letrec ((fac-aux (lambda (n result)
;                       (if (zero? n)
;                           result
;                           (fac-aux (- n 1) (* n result))))))
;     (fac-aux n 1)))

(define (FAC n)
  (let ((fac-aux (Y (lambda (self)
                      (lambda (n result)
                        (if (zero? n)
                            result
                            (self (- n 1) (* n result))))))))
    (fac-aux n 1)))

(write (fac 10))
(newline)

; now we have recursion without self-reference,
; and an implementation of letrec without using set!

; I managed to write the Y operator for 2 functions:

(define (Y-2 maker-1 maker-2)
  (let ((future-1 (lambda (x y)
                    (maker-1 (lambda arg
                               (apply (x x y) arg))
			     (lambda arg
			       (apply (y x y) arg)))))
	(future-2 (lambda (x y)
		    (maker-2 (lambda arg
			       (apply (x x y) arg))
			     (lambda arg
			       (apply (y x y) arg))))))
    (list (maker-1 (lambda arg
		     (apply (future-1 future-1 future-2) arg))
		   (lambda arg
		     (apply (future-2 future-1 future-2) arg)))
	  (maker-2 (lambda arg
		     (apply (future-1 future-1 future-2) arg))
		   (lambda arg
		     (apply (future-2 future-1 future-2) arg))))))

; which can be used as follows:

(define (EVEN-OR-ODD n)
  (apply (lambda (even? odd?)
	   (if (even? n) 'even 'odd))
	 (y-2 (lambda (even? odd?)
		(lambda (n)
		  (if (zero? n) #t (odd? (- n 1)))))
	      (lambda (even? odd?)
		(lambda (n)
		  (if (zero? n) #f (even? (- n 1))))))))

(write (even-or-odd 5))
(newline)
(write (even-or-odd 6))
(newline)

; (the apply here is in fact a let, with even? equal
;  to the first result of the y-2 call, and odd? equal
;  to the second; remember that standard scheme does not
;  return multiple values)

; The question now is: How is Y-m written (the Y operator
; taking any number of arguments) ?

;   (define (Y-m . makers) ...)

; (that is Y-m written as a function returning a fixpoint,
;  not a macro that constructs Y-m)

; All comments are welcome, in the mean time we are
; trying to find it ourselves.

; sven (sven@arti.vub.ac.be)

; From jrossie@copper.ucs.indiana.edu Tue Oct 29 09:45:31 1991
; From: jrossie@copper.ucs.indiana.edu (Jon Rossie)
; Newsgroups: comp.lang.scheme
; Subject: Re: Y Operator
; Date: 25 Oct 91 17:11:48 GMT
; Organization: Indiana University
; Nntp-Posting-Host: copper.ucs.indiana.edu

; > The question now is: How is Y-m written (the Y operator
; > taking any number of arguments) ?
; > 
; >   (define (Y-m . makers) ...)
; > 
; > (that is Y-m written as a function returning a fixpoint,
; >  not a macro that constructs Y-m)
; > 
; > All comments are welcome, in the mean time we are
; > trying to find it ourselves.
; > 
; > sven (sven@arti.vub.ac.be)
; > 

; ;; I think this does what you want:

; ;; we'll need a Y for multiple-arity procedures:

(define Y
  (lambda (M)
    ((lambda (future)
       (M (lambda x
 	    (apply (future future) x))))
     (lambda (future)
       (M (lambda x
 	    (apply (future future) x)))))))

;; this makes a list of procedures

(define make-r*
  (Y (lambda (fn)
       (lambda (m-list m*)
	 (if (null? m-list)
 	     '()
 	     (cons (lambda a
 		     (apply (apply (car m-list) (fn m* m*)) a))
 		   (fn (cdr m-list) m*)))))))

;; this is the multiple-arity Y operator

(define fixn
  (lambda m*
    (apply (car m*) (make-r* m* m*))))  

; example: (note -- the first proc is the one that is returned)

(define ya-even?
  (fixn
   (lambda (ev? od?)
     (lambda (n)
       (if (zero? n)
 	   #t
 	   (od? (-1+ n)))))
   (lambda (ev? od?)
     (lambda (n)
       (if (zero? n)
 	   #f
 	   (ev? (-1+ n)))))))

(write (ya-even? 5))
(newline)
(write (ya-even? 6))
(newline)

; From sven%arti12.vub.ac.be@mitvma.mit.EDU Tue Oct 29 09:47:09 1991
; From: sven%arti12.vub.ac.be@mitvma.mit.EDU
; Newsgroups: comp.lang.scheme
; Subject: Y Operator
; Date: 28 Oct 91 17:49:50 GMT
; Organization: The Internet

; Concerning the Multi-Y-Operator,
; I think I found a solution that satisfies my goals:

; With some very nice and elegant (no typo this time)
; code from Jon Rossie (Indiana University):

; (define MAKE-R*
;   (Y (lambda (fn)
;        (lambda (m-list m*)
;          (if (null? m-list)
;              '()
;              (cons (lambda a
;                      (apply (apply (car m-list) (fn m* m*)) a))
;                    (fn (cdr m-list) m*)))))))

; rewritten but equivalent,
; (notice that m-list changes while m* is constant)

(define MAKE-R*
  (Y (lambda (fn)
       (lambda (m-list m*)
	 (if (null? m-list)
	     '()
	     (cons (lambda args
		     (apply (apply (car m-list) (fn m* m*)) args))
		   (MAKE-R* (cdr m-list) m*)))))))

; Jon's Y-m, returning only the first procedure:

(define FIX-N-1
  (lambda m*
    (apply (car m*) (make-r* m* m*))))

; mine, returning all procedures in a list:

(define Y-M
  (lambda m*
    (let ((futures (make-r* m* m*)))
      (map (lambda (m)
	     (apply m futures))
	   m*))))

; this can be used as follows (with a VERY HAIRY fibonacci):

; to check the results:

(define (SIMPLE-FIB n)
  (if (< n 2)
      1
      (+ (simple-fib (- n 1))
	 (simple-fib (- n 2)))))

; with LETREC:

(define (COMPLICATED-FIB n)
  (letrec ((even? (lambda (n)
                    (if (zero? n) #t (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (zero? n) #f (even? (- n 1)))))
           (fib-even (lambda (n)
                       (if (zero? n)
                           1
                           (+ (fib-odd (- n 1))
                              (fib-even (- n 2))))))
           (fib-odd (lambda (n)
                      (if (= n 1)
                          1
                          (+ (fib-even (- n 1))
                             (fib-odd (- n 2)))))))
    (if (even? n)
        (fib-even n)
        (fib-odd n))))

; with Y-m:

(define (COMPLICATED-FIB-X n)
  (apply (lambda (even? odd? fib-even fib-odd)
           (if (even? n)
               (fib-even n)
               (fib-odd n)))
         (Y-m (lambda (even? odd? fib-even fib-odd)
                (lambda (n)
                  (if (zero? n) #t (odd? (- n 1)))))
              (lambda (even? odd? fib-even fib-odd)
                (lambda (n)
                  (if (zero? n) #f (even? (- n 1)))))
              (lambda (even? odd? fib-even fib-odd)
                (lambda (n)
                  (if (zero? n)
                      1
                      (+ (fib-odd (- n 1))
                         (fib-even (- n 2))))))
              (lambda (even? odd? fib-even fib-odd)
                (lambda (n)
                  (if (= n 1)
                      1
                      (+ (fib-even (- n 1))
                         (fib-odd (- n 2)))))))))

(write (simple-fib 10))
(newline)
(write (complicated-fib 10))
(newline)
(write (complicated-fib-x 10))
(newline)

; My interest in the multi-Y-operator is in the context of
; Scheme implementations. I always found it a contradiction that:
;         (1) letrec is not a `primitive expression'
;             (although many compilers consider letrec a primitive)
;         (2) recursion is the only iteration
;             (so should be as efficient)
;         (3) RRS gives an implementation of letrec with set!
;             (difficult to compile, non-functional
;              while letrec IS functional)
; I knew that the Y operator solved these problems,
; now that I have Y-m I think the problem is solved.

; By the way, code with Y SHOULD be as efficient as LETREC,
; since recursion is the primitive, not LETREC (this is an opinion,
; not a fact found in existing implementations, it should
; be interesting if someone checked this out).

; Thanks Jon, and the others who took the trouble of sending
; some reply.

; sven (sven@arti.vub.ac.be)
