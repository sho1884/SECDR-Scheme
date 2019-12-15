
; This is a quick hack to test the graphics primitives.
; The SLIB scheme library is needed for random.
; IMHO, the syntax of `do' in scheme is horrible!
; - sjm

(define (grtest)
  (require 'random) ; needs SLIB
  (graphics-mode!)

  (display "testing draw-to") (newline)
  (clear-graphics!)
  (goto-center!)
  (do ((x 0 (+ x 3)))
      ((> x (max-x)) 0)
    (set-color! (remainder (/ x 3) (max-color)))
    (draw-to x 0)
    (draw-to x (max-y))
  )

  (do ((y 0 (+ y 3)))
      ((> y (max-y)) 0)
    (set-color! (remainder (/ y 3) (max-color)))
    (goto-center!)
    (draw-to! 0 y)
    (goto-center!)
    (draw-to! (max-x) y)
  )

  (goto-nw!)
  (do ((x 0 (+ x 2)))
      ((> x (max-x)) 0)
    (set-color! (remainder (/ x 2) (max-color)))
    (draw-to x (max-y))
  )
  (do ((y (+ (max-y) 1) (- y 2)))
      ((< y 0) 0)
    (set-color! (remainder (/ y 2) (max-color)))
    (draw-to (max-x) y)
  )

  (display "testing set-dot!") (newline)
  (clear-graphics!)
  (do ((x 0 (+ x 1)))
      ((= x 100) 0)
    (set-dot! (+ (random (max-x)) 1) (+ (random (max-y)) 1)
	      (+ (random (max-color)) 1))
  )

  (display "testing draw with turn-to!") (newline)
  (clear-graphics!)
  (goto-center!)
  (do ((x 0 (+ x 1)))
      ((= x 100) 0)
    (set-color! (+ (random (max-color)) 1))
    (turn-to! (random 360))
    (draw (random 50))
  )

  (display "testing draw with turn-right") (newline)
  (clear-graphics!)
  (goto-center!)
  (do ((x 0 (+ x 1)))
      ((= x 100) 0)
    (set-color! (+ (random (max-color)) 1))
    (turn-right (random 90))
    (draw (random 50))
  )

  (display "testing draw with turn-left") (newline)
  (clear-graphics!)
  (goto-center!)
  (do ((x 0 (+ x 1)))
      ((= x 100) 0)
    (set-color! (+ (random (max-color)) 1))
    (turn-left (random 90))
    (draw (random 50))
  )

  (text-mode!)
)

