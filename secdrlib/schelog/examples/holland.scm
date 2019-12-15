;holland.scm
;schelog
;an embedding of Prolog in Scheme
;(c) Dorai Sitaram, 1989, Rice University

;This is a very trivial program comprising the facts:
;
;    city(amsterdam).
;    city(brussels).
;    country(holland).
;    country(belgium).

(define city
  (lambda (x)
    (%or (== x 'amsterdam)
	 (== x 'brussels))))

(define country
  (lambda (x)
    (%or (== x 'holland)
	 (== x 'belgium))))

;For a more Prolog-style syntax, you can rewrite the same thing,
;using the `rel' macro, as the following:

'(define city
  (rel ()
    (('amsterdam))
    (('brussels))))

'(define country
  (rel ()
    (('holland))
    (('belgium))))

;Typical easy queries:
;
; (which (x) (city x)) succeeds twice
; (which (x) (country x)) succeeds twice
; (which () (city 'amsterdam)) succeeds
; (which () (country 'amsterdam)) fails
