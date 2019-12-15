;mapcol.scm
;schelog
;An embedding of Prolog in Scheme
;Dorai Sitaram, dorai@cs.rice.edu, 1989

;map coloring, example from Sterling & Shapiro, p. 212

;(%member x y) holds if x is in y

(define %member
  (rel (X Xs Y Ys)
    ((X (cons X Xs)))
    ((X (cons Y Ys)) (%member X Ys))))

;(%members x y) holds if x is a subset of y

(define %members
  (rel (X Xs Ys)
    (((cons X Xs) Ys) (%member X Ys) (%members Xs Ys))
    ((() Ys))))

;(%select x y z) holds if z is y with one less occurrence of x

(define %select
  (rel (X Xs Y Ys Zs)
    ((X (cons X Xs) Xs))
    ((X (cons Y Ys) (cons Y Zs))
     (%select X Ys Zs))))

;region is a structure-builder

(define region
  (lambda (name color neighbors)
    (list 'region name color neighbors)))

(define %color-map
  (rel (Region Regions Colors)
    (((cons Region Regions) Colors)
     (%color-region Region Colors) (%color-map Regions Colors))
    ((() Colors))))

(define %color-region
  (rel (Name Color Neighbors Colors Colors1)
    (((region Name Color Neighbors) Colors)
     (%select Color Colors Colors1)
     (%members Neighbors Colors1))))

(define %test-color
  (rel (Name Map Colors)
    ((Name Map)
     (%map Name Map)
     (%colors Colors)
     (%color-map Map Colors))))

(define %map
  (rel (A B C D E F P H WG L I S)
    (('test (list
	      (region 'a A (list B C D))
	      (region 'b B (list A C E))
	      (region 'c C (list A B D E F))
	      (region 'd D (list A C F))
	      (region 'e E (list B C F))
	      (region 'f F (list C D E)))))
    (('western-europe
       (list
	 (region 'portugal P (list E))
	 (region 'spain E (list F P))
	 (region 'france F (list E I S B WG L))
	 (region 'belgium B (list F H L WG))
	 (region 'holland H (list B WG))
	 (region 'west-germany WG (list F A S H B L))
	 (region 'luxembourg L (list F B WG))
	 (region 'italy I (list F A S))
	 (region 'switzerland S (list F I A WG))
	 (region 'austria A (list I S WG)))))))

(define %colors
  (rel ()
    (('(red yellow blue white)))))

;ask (which (M) (%test-color 'test M)) or
;ask (which (M) (%test-color 'western-europe M)) for the
;respective (non-unique) colorings.

