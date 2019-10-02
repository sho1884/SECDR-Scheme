;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; A list of trivial programs in Prolog, just so you can get used
;;; to schelog syntax.

(define %length
  (rel (h t n m)
    [([] 0)]
    [((cons h t) n) (%length t m) (%is n (1+ m))]))

(define %delete
  (rel (x y z w)
    [(x [] [])]
    [(x (cons x w) y) (%delete x w y)]
    [(x (cons z w) (cons z y)) (non (eq x z)) (%delete x w y)]))

(define %filter
  (rel (x y z w)
    [([] [])]
    [((cons x y) (cons x z)) (%delete x y w) (%filter w z)]))

'(define %count
  (rel (x n y)
    [(x n) (%filter x y) (%length y n)]))

(define %count
  (letrec ([countaux
	     (rel (m n mm x y z)
	       [([] m m)]
	       [((cons x y) m n)
		(%delete x y z) (%is mm (1+ m)) (countaux z mm n)])])
    (rel (x n)
      [(x n) (countaux x 0 n)])))

(define %append
  (rel (x y z w)
    [([] x x)]
    [((cons x y) z (cons x w)) (%append y z w)]))

'(define %reverse
  (rel (x y z yy)
    [([] [])]
    [((cons x y) z) (%reverse y yy) (%append yy (list x) z)]))

(define %reverse
  (letrec ([revaux
	     (rel (x y z w)
	       [([] y y)]
	       [((cons x y) z w) (revaux y (cons x z) w)])])
    (rel (x y)
      [(x y) (revaux x [] y)])))

'(define %fact
  (rel (n n! n-1 n-1!)
    [(0 1)]
    [(n n!) (%is n-1 (1- n)) (%fact n-1 n-1!) (%is n! (* n n-1!))]))

(define %fact
  (letrec ([factaux
	     (rel (n! m x m-1 xx)
	       [(0 n! n!)]
	       [(m x n!) (%is m-1 (1- m)) (%is xx (* x m))
		(factaux m-1 xx n!)])])
    (rel (n n!)
      [(n n!) (factaux n 1 n!)])))
