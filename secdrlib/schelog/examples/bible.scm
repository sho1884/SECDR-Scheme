;bible.scm
;Schelog
;An embedding of Prolog in Scheme
;(c) Dorai Sitaram, 1989, Rice U.

;"Biblical" database from Sterling & Shapiro, p. 267,
;Ch. 17, "Second-order programming".  Illustrates %bag-of, 
;%set-of.

(define %father
  (rel ()
    (('terach 'abraham)) (('terach 'nachor)) (('terach 'haran))
    (('abraham 'isaac)) (('haran 'lot)) (('haran 'milcah))
    (('haran 'yiscah))))

(define %mother
  (rel () (('sarah 'isaac))))

(define %male
  (rel ()
    (('terach)) (('abraham)) (('isaac)) (('lot)) (('haran)) (('nachor))))

(define %female
  (rel ()
    (('sarah)) (('milcah)) (('yiscah))))
  
'(define %children 
  (letrec ((children-aux
	     (rel (x a cc c)
	       ((x a cc)
		(%father x c) (%not (%member c a)) !
		(children-aux x (cons c a) cc))
	       ((x cc cc)))))
    (rel (x cc)
      ((x cc) (children-aux x () cc)))))

;As S & S say: the above answers queries such as (which (xx)
;(%children 'terach xx)).  However, it traverses the search
;tree afresh each time a solution is added to the
;accumulator.  Also, it is not very general: a query such as
;(which (x cc) (%children x cc)) gives ((x terach) (cc
;(haran nachor abraham))) without alternatives because of
;the !.

(define %children
  (rel (x kids c)
    ((kids) (%bag-of c (%exists x (%father x c)) kids))))

(define dad-kids-tst
  (lambda ()
    (letref (dad kids x)
      (which (dad-kids)
	(%set-of (list dad kids)
	  (%set-of x (%father dad x) kids)
	  dad-kids)))))
