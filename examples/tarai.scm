;;  named lambda version

(define (tarai x y z)
  (if (> x y)
    (tarai (tarai (- x 1) y z) (tarai (- y 1) z x) (tarai (- z 1) x y))
    y))

;;  (anonymous) lambda version

(define tarai*
  (lambda (x y z)
    (if (> x y)
      (tarai* (tarai* (- x 1) y z) (tarai* (- y 1) z x) (tarai* (- z 1) x y))
      y)))

