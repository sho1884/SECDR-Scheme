;;
;; colorbar.scm
;;

(define (colorbar)
  (graphics-mode!)
  (clear-graphics!)
  (do ((color 0 (+ color 1)))
      ((> color (max-color)) 0)
    (set-color! color)
    (move-to! (* (+ 1 color) 10) 0)
    (draw-to (* (+ 1 color) 10) (max-y)))

  (do ((color 0 (+ color 1)))
      ((> color (max-color)) 0)
    (display (get-dot (* (+ 1 color) 10) (- (max-y) 10)))
    (display " "))
  (newline))
