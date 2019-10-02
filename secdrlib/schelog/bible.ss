;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; "biblical" database from Sterling&Shapiro, p. 267,
;;; Chap. 17, Second-order programming. Illustrates %bagof, %setof.

(define %father
  (rel ()
    [('terach 'abraham)] [('terach 'nachor)] [('terach 'haran)]
    [('abraham 'isaac)] [('haran 'lot)] [('haran 'milcah)]
    [('haran 'yiscah)]))

(define %mother
  (rel () [('sarah 'isaac)]))

(define %male
  (rel ()
    [('terach)] [('abraham)] [('isaac)] [('lot)] [('haran)] [('nachor)]))

(define %female
  (rel ()
    [('sarah)] [('milcah)] [('yiscah)]))
  
'(define %children ; difficult to read and possibly problematic
  (letrec ([children-aux
	     (rel (x a cc c)
	       [(x a cc)
		(%father x c) (%not (%member c a)) !
		(children-aux x (cons c a) cc)]
	       [(x cc cc)])])
    (rel (x cc)
      [(x cc) (children-aux x [] cc)])))

(define %children
  (rel (x kids c)
    [(kids) (%bagof c (%exists x (%father x c)) kids)]))

(define dad-kids-tst
  (lambda ()
    (letref (dad kids x)
      (which (dad-kids)
	(%setof [list dad kids]
	  (%setof x (%father dad x) kids)
	  dad-kids)))))
