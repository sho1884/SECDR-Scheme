; From kold@occs.cs.oberlin.edu Wed Aug 12 10:35:36 1992
; From: kold@occs.cs.oberlin.edu (Kennis Koldewyn)
; Newsgroups: comp.lang.scheme
; Subject: Re: How to define mult function of Church Numerals by lambda calculus?
; Date: 11 Aug 92 17:56:59 GMT
; Organization: Oberlin College Computer Science
; In-Reply-To: Jesse's message of Mon, 10 Aug 1992 15:52:37 GMT
; X-Posted-From: occs.cs.oberlin.edu
; X-Posted-Through: sol.ctr.columbia.edu


;      Jesse (cyen@cs.unt.edu) writes:

; > The Church Numerals are defined:
; > 0'=lambda f.lambda x.x
; > 1'=lambda f.lambda x.f x
; > 2'=lambda f.lambda x.f (f x)
; > ......
; > 
; > n'=lambda f.lambda x.f^n x
; > 
; > Thus adding two Church Numerals:
; > 
; > add=lambda m.lambda n.lambda f.lambda x.m f (n f x)
; > 
; > m,n are Church Numerals
; > 
; > My problem is: how to define mult, which multiplies two Church Numerals m,n.
; > 
; > All I know is to add(n,n) up to m times, suppose n<m. (to prevent that m=0')

;      There's another way to do it which is deceptively simple.  The
; multiplication function is simply \a.\b.\c.a (b c) where a and b are
; the two input Church numerals (read "\" as "lambda").  Here's a quick
; example to demonstrate how it works:

; Suppose we want to multiply two by three.  Let the multiplication
; function MULT = \a.\b.\c.a (b c), and the Church numerals
; TWO = \x.\y.x (x y) and THREE = \z.\w.z (z (z w)).  Then,

; (MULT TWO) THREE ==>
; [ \a.\b.\c.a (b c) TWO ] THREE ==>
; [ \a.\b.\c.a (b c) \x.\y.x (x y) ] THREE ==>
; [ \a.\b.\c.a (b c) \x.\y.x (x y) ] \z.\w.z (z (z w)) ==>
; [ \b.\c. [ \x.\y.x (x y) (b c) ] ] \z.\w.z (z (z w)) ==>
; \c. [ \x.\y.x (x y) [ \z.\w.z (z (z w)) c ] ] ==>
; \c. [ \x.\y.x (x y) \w. c (c (c w)) ] ==>
; \c.\y. [ \w. c (c (c w)) [ \w. c (c (c w)) y ] ] ==>
; \c.\y. [ \w. c (c (c w)) c (c (c y)) ] ==>
; \c.\y. c (c (c (c (c (c y)))))

; and we end up with the Church numeral for six.  The way that I think
; about it is that we are "inserting" the second Church numeral into
; every x in the first Church numeral (which is \x.\y.x (x (x ... (x y)))).
; Thus, the resulting Church numeral has n*m x's, where n was the
; original number of x's and m is the "size" of the second Church
; numeral.  Not quite accurate, but a good intuitive way of looking at
; it.
;      A nice way to implement this in Scheme is as follows...

; First we define a few Church numerals to play with:

(define zero (lambda (x) (lambda (y) y)))
(define one (lambda (x) (lambda (y) (x y))))
(define two (lambda (x) (lambda (y) (x (x y)))))
(define three (lambda (x) (lambda (y) (x (x (x y))))))

; and then we define the arithmetic operations:

(define church-add
  (lambda (num1 num2)
    (((lambda (x)
      (lambda (y)
        ((num1 x) ((num2 x) y)))) 1+) 0)))

(define church-mul
  (lambda (num1 num2)
    (((lambda (x)
      (num1 (num2 x))) 1+) 0)))

; Notice that Scheme will replace all of the x's in the result (which
; looks like \x.\y.x (x (x ... (x y)))) with the function 1+, and
; replace the y with 0 so that the output is a number that we can read.  
; For example:

; > (church-mul two three)
; 6
; >

(write (church-add two three))
(newline)
(write (church-mul two three))
(newline)

;      Hope this answers the question clearly.  Extra credit:  Just for
; fun, see if you can write the function for exponentiation.  Hint:  it's
; even simpler than the multiplication function!

;                                         - Kennis Koldewyn

; --
; +--------------------------+--------------------------------------------------+
; |     Kennis Koldewyn      | Without computers, it would be virtually         |
; | ------------------------ | impossible for us to accomploiwur xow;gkc,mf(&(  |
; | kold@occs.cs.oberlin.edu |                       - Dave Berry               |
; +--------------------------+--------------------------------------------------+

