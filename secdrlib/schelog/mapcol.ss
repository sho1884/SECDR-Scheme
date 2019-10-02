;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; Map coloring, example from Sterling&Shapiro, p. 212.

(define %member
  (rel (X Xs Y Ys)
    [(X [cons X Xs])]
    [(X [cons Y Ys]) (%member X Ys)]))

(define %members
  (rel (X Xs Ys)
    [([cons X Xs] Ys) (%member X Ys) (%members Xs Ys)]
    [([] Ys)]))

(define %select
  (rel (X Xs Y Ys Zs)
    [(X [cons X Xs] Xs)]
    [(X [cons Y Ys] [cons Y Zs])
     (%select X Ys Zs)]))

(define-functor region name color neighbors)

(define %color-map
  (rel (Region Regions Colors)
    [([cons Region Regions] Colors)
     (%color-region Region Colors) (%color-map Regions Colors)]
    [([] Colors)]))

(define %color-region
  (rel (Name Color Neighbors Colors Colors1)
    [([region Name Color Neighbors] Colors)
     (%select Color Colors Colors1)
     (%members Neighbors Colors1)]))

(define %test-color
  (rel (Name Map Colors)
    [(Name Map)
     (%map Name Map)
     (%colors Colors)
     (%color-map Map Colors)]))

(define %map
  (rel (A B C D E F P H WG L I S)
    [('test [list
	      [region 'a A [list B C D]]
	      [region 'b B [list A C E]]
	      [region 'c C [list A B D E F]]
	      [region 'd D [list A C F]]
	      [region 'e E [list B C F]]
	      [region 'f F [list C D E]]])]
    [('western-europe
       [list
	 [region 'portugal P [list E]]
	 [region 'spain E [list F P]]
	 [region 'france F [list E I S B WG L]]
	 [region 'belgium B [list F H L WG]]
	 [region 'holland H [list B WG]]
	 [region 'west-germany WG [list F A S H B L]]
	 [region 'luxembourg L [list F B WG]]
	 [region 'italy I [list F A S]]
	 [region 'switzerland S [list F I A WG]]
	 [region 'austria A [list I S WG]]])]))

(define %colors
  (rel (X)
    [('[red yellow blue white])]))

;;; ask (which (M) (%test-color 'test M)) or
;;; ask (which (M) (%test-color 'western-europe M)) for the
;;; respective (non-unique) colorings.

