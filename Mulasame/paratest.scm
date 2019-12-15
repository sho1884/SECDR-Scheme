;;
;; paratest.scm for Mulasame
;;
;;========================================================================
;;
;;   Copyright(C) 1993, 1994 Shoichi Hayashi.
;;
;; RCS info: $Header: /home/argama/s-haya/Scheme/Mulasame/RCS/paratest.scm,v 1.2 1994/09/14 08:11:57 s-haya Exp s-haya $
;;

(define (wait-loop)
  (let ((counter 15000))
    (while (> counter 0)(set! counter (-1+ counter)))))

; -----

(define (counter cpu)
  (let ((value 0))
    (letrec ((loop (lambda ()(set! value (1+ value))(loop))))
      (migrate loop cpu)
      (lambda (msg)
	(cond ((eq? msg 'value) value)
	      (else (error "unknown message" msg)))))))

(define cpu (gencpu))

(define c1 (counter cpu))

(display "(c1 'value) ==> ")
(display (c1 'value))
(newline)
(newline)

(display "(wakeup-cpu cpu)")
(newline)
(newline)

(wakeup-cpu cpu)

(wait-loop)

(display "(c1 'value) ==> ")
(display (c1 'value))
(newline)
(newline)

(wait-loop)

(display "(c1 'value) ==> ")
(display (c1 'value))
(newline)
(newline)

(exbegin
 (define -*-process-*- nil)
 (define -*-wait-process-*- nil)
 (define -*-sem-wait-process-*- nil)
 (define -*-process-*- nil)
 (%check-scheduler%))

; -----

(define (integersfrom n)
  (fcons-stream n (integersfrom (1+ n))))

(macro stream-pop
  (lambda (l)
    (let ((n (cadr l)))
      `(let ((ans (head ,n)))
	 (set! ,n (tail ,n))
	 ans))))

(display "(define x (integersfrom 1))")
(newline)
(newline)

(define x (integersfrom 1))

(display "x ==> ")
(display x)
(newline)
(newline)

(display "(stream-pop x) ==> ")
(display (stream-pop x))
(newline)
(newline)

(display "x ==> ")
(display x)
(newline)
(newline)

(display "(stream-pop x) ==> ")
(display (stream-pop x))
(newline)
(newline)

(display "x ==> ")
(display x)
(newline)
(newline)

(display "(stream-pop x) ==> ")
(display (stream-pop x))
(newline)
(newline)

(exbegin
 (define -*-process-*- nil)
 (define -*-wait-process-*- nil)
 (define -*-sem-wait-process-*- nil)
 (define -*-process-*- nil)
 (%check-scheduler%))

; -----

(define (logic-nand x y)
  (if (= 0 x)
      1
      (if (= 0 y)
	  1
	  0)))

(define (nand x y)
  (fcons-stream (logic-nand (head x) (head y))
		(nand (tail x)
		      (tail y))))

(define (rs-ff s r)
  (letrec ((t1 (fcons-stream 0 (nand t2 r)))
	   (t2 (fcons-stream 1 (nand s t1))))
    (cons t2 t1)))

(define (clock tau ctime)
  (if (< (remainder ctime tau) (* 2 (/ tau 3)))
      (fcons-stream 1 (clock tau (1+ ctime)))
      (fcons-stream 0 (clock tau (1+ ctime)))))

(define (probe signal signal-name time-limit)
  (display signal-name)
  (display ": ")
  (do ((ctime 0 (1+ ctime)))
      ((<= time-limit ctime) (begin (display 'halt)(newline)))
    (if (= 0 (stream-pop signal))
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

(display "(sim 60)")
(newline)
(newline)

(sim 60)

(exbegin
 (define -*-process-*- nil)
 (define -*-wait-process-*- nil)
 (define -*-sem-wait-process-*- nil)
 (define -*-process-*- nil)
 (%check-scheduler%))
(newline)

; -----

(define (tarai x y z)
  (if (> x y)
      (tarai (tarai (- x 1) y z)
	     (tarai (- y 1) z x)
	     (tarai (- z 1) x y))
      y))

(display "(define a
  (race
   (cobegin (tarai 6 3 0)(tarai 4 2 0)(tarai 10 5 0)(tarai 8 4 0))))")
(newline)
(newline)

(define a
  (race
   (cobegin (tarai 6 3 0)(tarai 4 2 0)(tarai 10 5 0)(tarai 8 4 0))))

(display "a ==> ")
(display a)
(newline)
(newline)

(display "(stream-pop a) ==> ")
(display (stream-pop a))
(newline)
(newline)

(display "a ==> ")
(display a)
(newline)
(newline)

(display "(stream-pop a) ==> ")
(display (stream-pop a))
(newline)
(newline)

(display "a ==> ")
(display a)
(newline)
(newline)

(display "(stream-pop a) ==> ")
(display (stream-pop a))
(newline)
(newline)

(display "a ==> ")
(display a)
(newline)
(newline)

(display "(stream-pop a) ==> ")
(display (stream-pop a))
(newline)
(newline)

(display "a ==> ")
(display a)
(newline)
(newline)

(exbegin
 (define -*-process-*- nil)
 (define -*-wait-process-*- nil)
 (define -*-sem-wait-process-*- nil)
 (define -*-process-*- nil)
 (%check-scheduler%))

; -----

(define (dining-philosophers)
  (define (ngensemaphore i n)
    (if (= 0 n) nil (cons (gensemaphore i) (ngensemaphore i (-1+ n)))))
  (let ((room (gensemaphore 4))
	(fork (list->vector (ngensemaphore 1 5)))
	(think (lambda (i)
		  (iostream
		    (display "Philosopher")
		    (display i)
		    (display " starts thinking.")
		    (newline))
		  (tarai 6 3 0)
		  (iostream
		    (display "Philosopher")
		    (display i)
		    (display " stops thinking.")
		    (newline))))
	(eat (lambda (i)
	       (iostream
	         (display "Philosopher")
	         (display i)
	         (display " starts eating.")
	         (newline))
	       (tarai 8 4 0)
	       (iostream
	         (display "Philosopher")
	         (display i)
	         (display " stops eating.")
	         (newline)))))
    (letrec ((philosopher (lambda (i)
			    (think i)
			    (wait room)
			    (wait (vector-ref fork i))
			    (wait (vector-ref fork (modulo (1+ i) 5)))
			    (eat i)
			    (signal (vector-ref fork i))
			    (signal (vector-ref fork (modulo (1+ i) 5)))
			    (signal room)
			    (philosopher i))))
      (global-define start-eating
	(lambda ()
	  (wait room)
	  (iostream (display "You've gone into the room.")(newline))
	  (wait (vector-ref fork 0))
	  (iostream (display "You've taken your own fork.")(newline))
	  (wait (vector-ref fork 1))
	  (iostream (display "You've taken the fork on the right side.")(newline)
		    (display "You can eat.")(newline)) #t))
      (global-define stop-eating
	(lambda ()
	  (signal (vector-ref fork 0))
	  (iostream (display "You've released your own fork.")(newline))
	  (signal (vector-ref fork 1))
	  (iostream (display "You've released the fork on the right side.")(newline))
	  (signal room)
	  (iostream (display "You've gone out of the room.")(newline)) #t))
      (cobegin
;       (philosopher 0)
       (philosopher 1)
       (philosopher 2)
       (philosopher 3)
       (philosopher 4)))))

(display "(dining-philosophers)")
(newline)
(newline)

(dining-philosophers)

(wait-loop)

(iostream
 (display "(start-eating)")
 (newline)
 (newline))

(start-eating)

(wait-loop)

(iostream
 (display "(report)")
 (newline)
 (newline))

(report)

(iostream
 (display "(stop-eating)")
 (newline)
 (newline))

(stop-eating)

(wait-loop)

(exbegin
 (define -*-process-*- nil)
 (define -*-wait-process-*- nil)
 (define -*-sem-wait-process-*- nil)
 (define -*-process-*- nil)
 (%check-scheduler%))
(newline)
(display "END of test.")
(newline)

