(define (nand x y)
	(cons-stream (logic-nand (head x) (head y))
		(nand (tail x)
			  (tail y))))

(define (logic-nand x y)
	(if (= 0 x)
		1
		(if (= 0 y)
			1
			0)))

(define (rs-ff s r)
	(letrec ((t1 (cons-stream 0 (nand t2 r)))
			 (t2 (cons-stream 1 (nand s t1))))
		(display "Make RS-FlipFlop Stream...")
		(newline)
		(cons t2 t1)))

(define (integersfrom n)
	(cons-stream n (integersfrom (1+ n))))

(macro lazy-pop (lambda (l)
	(let ((n (cadr l)))
		`(let ((ans (head ,n)))
			(set! ,n (tail ,n))
			ans))))

(define (odd-numbers n)
	(cons-stream n (odd-numbers (+ n 2))))

(define (d-add1 x)
	(delay (cons (1+ (car (force x))) (d-add1 (cdr (force x))))))

(define (d-integers)
	(cons 1 (d-add1 (delay (d-integers)))))

;(define x)

;(set! x (integersfrom 0))

;(car x)

;(delay? (cdr x))

;(set! x (tail x))

;(set! x (odd-numbers 1))

;(lazy-pop x)

;(lazy-pop x)

;(lazy-pop x)

;(set! x (d-integers))

;(lazy-pop x)

;(lazy-pop x)

;(lazy-pop x)

(define (clock tau ctime)
	(if (< (remainder ctime tau) (* 2 (/ tau 3)))
		(cons-stream 1 (clock tau (1+ ctime)))
		(cons-stream 0 (clock tau (1+ ctime)))))

(define (probe signal signal-name time-limit)
	(display signal-name)
	(display ": ")
	(do ((ctime 0 (1+ ctime)))
		((<= time-limit ctime) (begin (display 'halt)(newline)))
		(if (= 0 (lazy-pop signal))
			(display '_)
			(display '-))))

(define (sim time-limit)
	(letrec ((s1 (clock 12 0))
			 (s2 (clock 12 6))
			 (s (rs-ff s1 s2))
			 (q (car s))
			 (~q (cdr s)))
		(probe s1 "Set  " time-limit)
		(probe s2 "Reset" time-limit)
		(probe q  "Out  " time-limit)
		(probe ~q "~Out " time-limit)))

(sim 60)

