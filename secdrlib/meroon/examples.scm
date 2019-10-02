(define-class Point Object ((= x) (= y)))
(define-class Polygon Object ((* point)))
(define-class Colored-Polygon Polygon (color) :immutable :careless)
(define-class Nicknamed-Colored-Polygon Colored-Polygon ((* nickname)))
(define-class Polygon Object ((* point)) :prototype)
(define-class Colored-Polygon Polygon (color) :immutable :prototype)
(define-class Nicknamed-Colored-Polygon Colored-Polygon ((* nickname)))
(define testobj (allocate-Point))
(object? testobj)
(point? testobj)
(polygon? testobj)
(define testobj2 (allocate-Polygon 3))
(define testobj3 (allocate-Nicknamed-Colored-Polygon 4 2))
(define testobj4
  (make-Nicknamed-Colored-Polygon
   2 (make-Point 22 44)(make-Point 51 90)
   'blue 3 'Joe 'Jack 'Jill))
(define ncp testobj4)
(Nicknamed-Colored-Polygon-point ncp 1)
(polygon-point ncp 0)
(Colored-Polygon-color ncp)
(Nicknamed-Colored-Polygon-nickname ncp 2)
(set-Nicknamed-Colored-Polygon-point! ncp 1 (allocate-Point))
(set-Polygon-point! ncp 0 (Colored-Polygon-point ncp 1))
(set-Nicknamed-Colored-Polygon-nickname! ncp 2 'Dick)
(Nicknamed-Colored-Polygon-nickname-length ncp)
(Colored-Polygon-point-length ncp)
(show ncp)
(show-hierarchy)
(show-generic 'show)
(show-meroon)
(unveil (symbol->class 'poly-field))
(define-class Counting-Class Class (counter))
(define-method (initialize! (o Counting-Class))
  (call-next-method)
  (set-counting-class-counter! o 0)
  (let ((original-maker (class-maker o)))
    (set-counting-class-maker!
     o (lambda args
         (set-counting-class-counter! o (+ 1 (counting-class-counter o)))
         (apply original-maker args) ) ) )
  o )
(define-class Counted-Point Point () :metaclass Counting-Class)
(Counting-Class-counter (symbol->class 'Counted-Point))
(let ((pt (make-counted-point 22 33)))
  (list (counted-point-x pt)(point-y pt)
        (Counting-Class-counter (symbol->class 'Counted-Point)) ) )
