;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; this is a very trivial program, corresponds to the facts:
;;;     city(amsterdam).
;;;     city(brussels).
;;;     country(holland).
;;;     country(belgium).

(define city
  (rel ()
    [('amsterdam)] [('brussels)]))

(define country
  (rel ()
    [('holland)] [('belgium)]))

;;; Typical easy queries:
;;;  (which (x) (city x)) succeeds twice
;;;  (which (x) (country x)) succeeds twice
;;;  (which () (city 'amsterdam)) succeeds
;;;  (which () (country 'amsterdam)) fails
