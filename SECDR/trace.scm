;;
;; trace.scm
;;

;;;;    A Simple Trace package for SECD-Scheme
;;
;;      Date written    15-Dec-1989 by Akira Kida (kida)
;;      Date revised    18-Dec-1989 by Akira Kida (kida)
;;      Date revised    30-Dec-1989 by Akira Kida (kida)
;;      -- rewrite for SECD-Scheme --
;;      Date revised    14-Jan-1990 by Atsushi Moriwaki
;;      -- rewrite for SECDR-Scheme --
;;      Date revised    28-May-1992 by Shoichi Hayashi
;;      Date revised    20-Nov-1992 by Shoichi Hayashi
;;      Date revised    26-Jan-1993 by Shoichi Hayashi
;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/trace.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

(define *trace-indent* 0)   ;  holds indent level during trace
(define *trace-limit* 0)    ;  zero ==> no limit

;;; Change Error Hook
(set! -*-error-hook-*-
  (let ((old-error-hook -*-error-hook-*-))
    (lambda () (set! *trace-indent* 0) (old-error-hook))))

;;;  run-time support procedure for trace
(define (@trace-procedure-call proc name args)
  (if (and (positive? *trace-limit*)
           (<= *trace-limit* *trace-indent*))
    (apply proc args)
    (let ((form (cons name args)) (t-indent *trace-indent*))
      (spaces (* *trace-indent* 2))
      (set! *trace-indent* (+ *trace-indent* 1))
      (uniq-write form)
      (newline)
      (let ((result (apply proc args)))
        (set! *trace-indent* t-indent)
        (if (negative? *trace-indent*)
          (set! *trace-indent* 0))
        (spaces (* *trace-indent* 2))
        (uniq-write form)(display " ==> ")
	(uniq-write result)(newline)
        result))))

;;; set/unset trace patch for a procedure
(define (set-trace name set)
  (if (not (procedure? (get name '%symbol-value%)))
    (error "Cannot set trace : " name))
  (if set
    (if (not (get name '%trace-backup%))
      (let ((proc (get name '%symbol-value%)))
        (put name '%trace-backup% proc)
        (put name '%symbol-value%
          (lambda args (@trace-procedure-call proc name args)))))  
    (if (get name '%trace-backup%)
      (begin
        (put name '%symbol-value% (get name '%trace-backup%))
        (put name '%trace-backup% #f))))
  name)

;;; Trace package user interface
;;;
;;; (trace sym1 sym2 ...)       ; trace enable sym1 sym2 ...
;;; (untrace sym1 sym2 ...)     ; trace disable sym1 sym2 ...
;;;

;;; set trace patch for prcedures
(define (trace . symbols)
  (map (lambda (f) (set-trace f #t)) symbols))

;;; unset trace patch for procedures
(define (untrace . symbols)
  (map (lambda (f) (set-trace f #f)) symbols))

