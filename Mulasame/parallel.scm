;;
;; parallel.scm
;;

;;;;    A Parallel Pakage for Mulasame (Parallel SECDR-Scheme)
;;;     Date written    1-Jan-1994 by Shoichi Hayashi
;;;
;;
;;========================================================================
;;
;;   Copyright(C) 1993, 1994 Shoichi Hayashi.
;;
;; RCS info: $Header: /home/argama/s-haya/Scheme/Mulasame/RCS/parallel.scm,v 1.8 1994/09/14 08:11:57 s-haya Exp s-haya $
;;

;(define -*-backup-cpu-*- #t)

(define (ngencpu n)
  (if (= 0 n) nil (cons (gencpu) (ngencpu (-1+ n)))))

(define console-semaphore (gensemaphore))

(macro iostream
  (lambda (l)
    `(begin (wait console-semaphore)
	    ,@(cdr l)
	    (signal console-semaphore))))

;(macro exbegin
;  (lambda (l)
;    `(begin (%task-switch-off%)
;	    ,@(cdr l)
;	    (%task-switch-on%))))

(macro exbegin
  (lambda (l)
    `(begin (%task-switch-off%)
	    (let ((ans (begin ,@(cdr l))))
	      (%task-switch-on%)
	      ans))))

(define (%my-error-hook%)
  (define (error-loop-help)
    (display "-------------------------------")(newline)
    (display " K: KILL all sub-process")(newline)
    (display " S: display S_register")(newline)
    (display " E: display E_register")(newline)
    (display " C: display C_register")(newline)
    (display " D: display D_register")(newline)
    (display " R: display R_register")(newline)
    (display " A: disAssemble C_register")(newline)
    (display " !: set! R_register")(newline)
    (display " &: continue (quit this mode)")(newline)
    (display " T: abort to scheme Top level")(newline)
    (display "-------------------------------")(newline))
  (define (error-loop)
    (display "List of Commands: (K, S, E, C, D, R, A, !, &, T or Help) ? ")
;    (if (eq? (software-type) 'MACOS)
;        (begin (display "input next line!")(newline)))
    (let ((indata (read))(com #f))
      (if (eq? indata #eof) (begin (%reset-master%) #v)
          (begin
	        (if (symbol? indata)
	            (set! com (string-ref (symbol->string indata) 0))
	            (set! com #\#))
            (cond ((char-ci=? com #\s)
                   (display " S: ")
                   (write (list-ref -*-error-register-*- 0))
                   (newline)(error-loop))
                  ((char-ci=? com #\e)
                   (display " E: ")
                   (write (list-ref -*-error-register-*- 1))
                   (newline)(error-loop))
                  ((char-ci=? com #\k)
                   (display " Error CPU:")(write -*-error-cpu-*-)(newline)
                   (display " KILL: all sub-process : ")
		   (write -*-process-*-)(newline)
		   (display "-*-process-*- -> -*-backup-process-*-")(newline)
                   (global-define -*-backup-process-*- -*-process-*-)
		   (set! -*-process-*- nil)
		   (global-define console-semaphore (gensemaphore))
		   (display " Console-Semaphore Reset!")(newline)
		   (newline)(error-loop))
                  ((char-ci=? com #\c)
                   (display " C: ")
                   (write (list-ref -*-error-register-*- 2))
                   (newline)(error-loop))
                  ((char-ci=? com #\a)
                   (display " disAsmC: ")
                   (disassemble (list-ref -*-error-register-*- 2))
                   (newline)(error-loop))
                  ((char-ci=? com #\d)
                   (display " D: ")
                   (write (list-ref -*-error-register-*- 3))
                   (newline)(error-loop))
                  ((char-ci=? com #\r)
                   (display " R: ")
                   (write (list-ref -*-error-register-*- 4))
                   (newline)(error-loop))
                  ((char-ci=? com #\!)
                   (display " R? ")
;                  (if (eq? (software-type) 'MACOS)
;                      (begin (display "input next line!")(newline)))
                   (set-cdr! (cdddr -*-error-register-*-) (cons (read) '()))
                   (%list->master% -*-error-register-*-)
                   (display " R: ")
                   (write (list-ref -*-error-register-*- 4))
                   (newline)(error-loop))
                  ((char-ci=? com #\&)
                   #v)
                  ((char-ci=? com #\t)
                   (%reset-master%)
                   #v)
                  (else
                   (error-loop-help)
                   (error-loop)))))))
  (set! -*-error-register-*- (%master->list%))
;  (error-loop-help)
  (error-loop))

(define -*-error-hook-*- %my-error-hook%)

;;
;; -- future --
;;
;; (future <exp>)
;;

(macro future
  (lambda (l)
    (let ((rate 1))
      (if (not (null? (cddr l)))
	  (set! rate (caddr l)))
      `(let* ((cpu (gencpu))
	      (ans (migrate (lambda () ,(cadr l)) cpu)))
	 (if (number? ,rate)
	     (begin
	       (%cpu-step-set!% cpu
		 (inexact->exact (round (* ,rate (%cpu-step% cpu)))))
	       (wakeup-cpu cpu)
	       ans)
	     (error "future -- Cannot set CPU-Step : " ,rate))))))

;(macro future
;  (lambda (l)
;    `(let* ((cpu (gencpu))
;	    (ans (migrate (lambda () ,@(cdr l)) cpu)))
;       (%cpu-step-set!% cpu 500)
;       (wakeup-cpu cpu)
;       ans)))

;(macro future
;  (lambda (l)
;    `(let* ((cpu (gencpu))
;	    (ans (make-place-holder))
;	    (dummy (migrate (lambda ()
;			      (place-holder-set! ans ,@(cdr l))) cpu)))
;       (wakeup-cpu cpu)
;       ans)))

(define (touch proc)
  (%ph-object% (%touch% proc)))

(macro fcons-stream (lambda (l)
    `(cons ,(cadr l) (future ,(caddr l)))))

(define (tail stream)
  (if (place-holder? (cdr stream))
      (touch (cdr stream))
      (force (cdr stream))))

(define (report)
  (exbegin
   (display "======================")(newline)
   (display " terminate cpu:")(display (length -*-terminate-*-))(newline)
   (display " active    cpu:")(display (length -*-process-*-))(newline)
   (display " wait      cpu:")(display (length -*-wait-process-*-))(newline)
   (display " sem wait  cpu:")(display (length -*-sem-wait-process-*-))(newline)
   (display " error     cpu:")(display (length -*-error-cpu-*-))(newline)
   (display "======================")(newline)))

(extend-syntax (cobegin)
   ((cobegin x) (cons (future x) nil))
   ((cobegin x y ...)
    (cons (future x) (cobegin y ...))))

(define (race l)
  (if (null? l)
      nil
      (do ((previous nil procp)(procp l (cdr procp)))
	  ((or (null? procp)(not (future? (car procp))))
	   (cond ((null? procp)
		  (%task-switch%) ; for optimize!
		  (race l))
		 ((null? previous)
		  (cons (%ph-object% (car l))(future (race (cdr l)))))
		 (else (set-cdr! previous (cdr procp))
		       (cons (%ph-object% (car procp)) (future (race l)))))))))

; for Stream

(define (map proc arg . rest)
  (define (map1 proc list)
    (if (pair? list)
      (cons (proc (car list)) (map1 proc (tail list)))
      '()))
  (define (maps proc dlist)
    (if (every? pair? dlist)
      (cons (apply proc (map1 car dlist))
            (maps proc (map1 tail dlist)))
      '()))
  (if (pair? rest)
    (maps proc (cons arg rest))
    (map1 proc arg)))

(define (for-each proc . args)
  (let for-each-loop ((dlist args))
    (if (every? pair? dlist)
      (begin (apply proc (map car dlist))
             (for-each-loop (map tail dlist)))
      #v)))

(define print-stream
  (let ()
    (define (iter s)
      (if (null? s)
	  (display "}")
	  (begin (display " ")
		 (write (head s))
		 (iter (tail s)))))
    (lambda (s)
      (newline)
      (display "{")
      (if (null? s)
	  (display "}")
	  (begin (write (head s))
		 (iter (tail s)))))))
