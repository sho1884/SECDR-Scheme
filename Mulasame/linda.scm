;;
;; linda.scm
;;

;;;;    A Linda Pakage for Mulasame (Parallel SECDR-Scheme)
;;;
;;;     Date revised    22-Sep-1994 by Shoichi Hayashi
;;;     Date revised    13-May-1994 by Shoichi Hayashi
;;;     Date revised    25-Apr-1994 by Shoichi Hayashi
;;;     Date written    28-Mar-1994 by Shoichi Hayashi
;;;
;;========================================================================
;;
;;   Copyright(C) 1994 Shoichi Hayashi.
;;
;; RCS info: $Header: /home/argama/s-haya/Mulasame/RCS/linda.scm,v 1.2 1994/09/22 07:59:18 s-haya Exp s-haya $
;;

(require 'mizutamari)

(define (send agent . tuple)
  (let ((ans (make-place-holder)))
    (apply most-general-send `(,ans ,agent . ,tuple))
    (touch ans)))

(define linda:-*-tuple-space-*- (mizutamari:new-collaboration-space))

(define (linda:out tuple)
  (if (not (pair? tuple))
      (error "linda:out -- argument must be pair "))
  (send linda:-*-tuple-space-*- 'out tuple #f #f)
  #v)

(define (linda:in tuple)
  (if (not (pair? tuple))
      (error "linda:in -- argument must be pair "))
  (touch (send linda:-*-tuple-space-*- 'in tuple)))

(define (linda:rd tuple)
  (if (not (pair? tuple))
      (error "linda:rd -- argument must be pair "))
  (touch (send linda:-*-tuple-space-*- 'in tuple #f)))

(define (linda:inp tuple)
  (if (not (pair? tuple))
      (error "linda:inp -- argument must be pair "))
  (send linda:-*-tuple-space-*- 'in tuple #t #f))

(define (linda:rdp tuple)
  (if (not (pair? tuple))
      (error "linda:rdp -- argument must be pair "))
  (send linda:-*-tuple-space-*- 'in tuple #f #f))

(macro linda:eval
  (lambda (l)
    `(let* ((cpu (gencpu)))
       (migrate
        (lambda ()
	  (let ((item ,@(cdr l)))
	    (if (not (pair? item))
		(error "linda:eval -- argument must be pair "))
	    (send linda:-*-tuple-space-*- 'out item #f #f)))
        cpu)
       (wakeup-cpu cpu)
       #v)))

(define (linda:report)
  (send linda:-*-tuple-space-*- 'print))
