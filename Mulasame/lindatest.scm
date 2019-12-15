;;
;; lindatest.scm
;;

;;;;    A Test Program for Linda Pakage for Mulasame (Parallel SECDR-Scheme)
;;;
;;;     Date written    28-Mar-1994 by Shoichi Hayashi
;;;
;;========================================================================
;;
;;   Copyright(C) 1994 Shoichi Hayashi.
;;
;; RCS info: $Header: /home/argama/s-haya/Scheme/Mulasame/RCS/lindatest.scm,v 1.2 1994/09/14 08:11:57 s-haya Exp s-haya $
;;

(require 'linda)

(let ((previous-number 0))
  (global-define ex-test
    (lambda (number in-arg out-arg)
      (let ((ans (> number previous-number)))
        (set! previous-number number)
        ans))))


; ---- test program -----

(define (linda:test)

  (define (foo)(list 'a 5 6))

  (linda:out '(a 1 2))
  (linda:out '(b 3 4))
  (display (linda:rd '(a x x)))(newline)
  (display (linda:rdp '(a x x)))(newline)
  (display (linda:in '(a x x)))(newline)
  (display (linda:inp '(a x x)))(newline)
  (display (linda:eval (foo)))(newline)
  (display (linda:rd '(a x x)))(newline)
  (display (linda:rdp '(a x x)))(newline)
  (display (linda:in '(a x x)))(newline)
  (display (linda:inp '(a x x)))(newline)
)

(define (linda:test2)

  (define (producer)
    (linda:out '(b 1 2))
    (linda:out '(a 1 2))
    (linda:out '(c 3 4))
    (linda:out '(a 3 4))
    (linda:out '(a 5 6))
    (linda:eval '(d 7 8))
    (linda:eval '(a 7 8)))
  
  (define (consumer n)

    (define (console-out tp)
      (iostream
       (display "Consumer")
       (display n)
       (display ": ")
       (display tp)
       (newline)))

    (console-out (linda:rdp '(a x x)))
    (console-out (linda:inp '(a x x)))
    (console-out (linda:in '(a x x)))
    (console-out (linda:rdp '(a x x)))
    (console-out (linda:inp '(a x x)))
    (console-out (linda:in '(a x x)))
    )

  (begin
    (cobegin
     (consumer 1)
     (consumer 2)
     (producer))))

(linda:test)
(linda:report)
(newline)
(linda:test2)
(linda:report)
