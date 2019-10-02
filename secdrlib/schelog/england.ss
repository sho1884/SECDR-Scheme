;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; The following is a simple database about a certain family in England.
;;; Should be a piece of cake, but given here so that you can hone
;;; your ability to read the syntax.

(define male
  (rel ()
    [('philip)] [('charles)] [('andrew)] [('edward)]
    [('mark)] [('william)] [('harry)] [('peter)]))

(define female
  (rel ()
    [('elizabeth)] [('anne)] [('diana)] [('sarah)] [('zara)]))

(define husbandof
  (rel ()
    [('philip 'elizabeth)] [('charles 'diana)]
    [('mark 'anne)] [('andrew 'sarah)]))

(define wifeof
  (rel (w h) 
    [(w h) (husbandof h w)]))

(define marriedto
  (rel (x y)
    [(x y) (husbandof x y)]
    [(x y) (wifeof x y)]))

(define fatherof
  (rel ()
   [('philip 'charles)] [('philip 'anne)] [('philip 'andrew)]
   [('philip 'edward)] [('charles 'william)] [('charles 'harry)]
   [('mark 'peter)] [('mark 'zara)]))

(define motherof
  (rel (m c f)
    [(m c) (wifeof m f) (fatherof f c)]))

(define childof
  (rel (c p)
    [(c p) (fatherof p c)]
    [(c p) (motherof p c)]))

(define parentof
  (rel (p c)
    [(p c) (childof c p)]))

(define brotherof
  (rel (b x f)
    [(b x) (male b) (fatherof f b) (fatherof f x) (%notunify b x)]))
