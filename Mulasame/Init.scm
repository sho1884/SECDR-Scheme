;;
;; Mulasame Init.scm
;;
;;========================================================================
;;
;;   Copyright(C) 1992, 1993 Shoichi Hayashi.
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/Init.scm,v 0.8 1994/09/14 09:18:21 s-haya Exp s-haya $
;;

(define (scheme-implementation-type) 'SECDR)
(define MULASAME_BOOT_PATH (getenv "MULASAME_BOOT_PATH"))
(define SECDR_LIBRARY_PATH (getenv "SECDR_LIBRARY_PATH")) ; secdrlib
(define SCHEME_LIBRARY_PATH (getenv "SCHEME_LIBRARY_PATH")) ; slib

(if (not MULASAME_BOOT_PATH)
  (case (software-type)
    ((UNIX)  (set! MULASAME_BOOT_PATH "/usr/PDS/lang/Scheme/Mulasame/"))
    ((MSDOS) (set! MULASAME_BOOT_PATH "A:\\Scheme\\Mulasame\\"))
    ((MACOS) (set! MULASAME_BOOT_PATH "SAZABI:Scheme:Mulasame:"))))
(if (not SECDR_LIBRARY_PATH)
  (case (software-type)
    ((UNIX)  (set! SECDR_LIBRARY_PATH "/usr/PDS/lang/Scheme/secdrlib/bin/"))
    ((MSDOS) (set! SECDR_LIBRARY_PATH "A:\\Scheme\\secdrlib\\bin\\"))
    ((MACOS) (set! SECDR_LIBRARY_PATH "SAZABI:Scheme:secdrlib:bin:"))))
(if (not SCHEME_LIBRARY_PATH)
  (begin
    (case (software-type)
      ((UNIX)  (set! SCHEME_LIBRARY_PATH "/usr/PDS/lang/Scheme/slib/"))
      ((MSDOS) (set! SCHEME_LIBRARY_PATH "A:\\Scheme\\slib\\"))
      ((MACOS) (set! SCHEME_LIBRARY_PATH "SAZABI:Scheme:slib:")))
    (let ((oldgetenv getenv))
      (set! getenv
        (lambda (envname)
          (if (string=? envname "MULASAME_BOOT_PATH")
            SCHEME_LIBRARY_PATH
            (oldgetenv envname)))))))

(define (terms)
  (list-file (string-append MULASAME_BOOT_PATH "COPYING")))

(define (list-file file)
  (call-with-input-file file
    (lambda (inport)
      (do ((c (read-char inport) (read-char inport)))
	  ((eof-object? c))
	(write-char c)))))
    
(define -*-error-register-*- '(()()()()()))

(define (%my-error-hook%)
  (define (error-loop-help)
    (display "-------------------------------")(newline)
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
    (display "List of Commands: (S, E, C, D, R, A, !, &, T or Help) ? ")
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
                   (if (eq? (software-type) 'MACOS)
                       (begin (display "input next line!")(newline)))
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

(new-segment 50)

(define scm-load load)
(define load ex-load)
(define (lib-load libname opt1 opt2)
  (ex-load (string-append SECDR_LIBRARY_PATH libname) opt1 opt2))

(if (eq? (process-type) 'mulasame)
    (load (string-append MULASAME_BOOT_PATH "parallel") #f #t))

(define (require . dummy)
  (display "Warning: The Scheme Library `slib' not found!")
  (newline)
  #f)

(if (file-exists? (string-append SCHEME_LIBRARY_PATH "require.scm"))
    (load (string-append MULASAME_BOOT_PATH "secdrscm.init") #f #t))

(define (date)
  (system "date"))

(define internal-time-units-per-second (get-internal-time-units))

(macro time (lambda (l) `(time-fun (lambda () ,(cadr l)))))

(define (time-fun exp)
  (gc-verbose-off)(gc)(gc-verbose-on)
  (display "; Performance:")(newline)
  (define stime (get-internal-run-time))
  (define ans (exp))
  (define etime (get-internal-run-time))
  (gc-verbose-off)
  (display "; Evaluation took ")
  (display (exact->inexact (/ (- etime stime) (get-internal-time-units))))
  (display " Seconds.")
  (newline)
  ans)


(define append!
  (lambda args
    (cond ((null? args) '())
	  ((null? (cdr args)) (car args))
	  ((null? (car args)) (cadr args))
	  (else
	   (set-cdr! (last-pair (car args))
		     (apply append! (cdr args)))
	   (car args)))))

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
	(let ((d (cdr s)))
	  (set-cdr! s r)
	  (loop d s))))))

(extend-syntax (define-syntax syntax-rules)
  ((define-syntax name (syntax-rules kwds . clauses))
   (extend-syntax (name . kwds) . clauses)))

(define #\return (integer->char 13))
(define exit quit)

(let ((oldrequire require)(loaded nil))
  (set! require
	(lambda args
	  (let ((name (car args)))
	    (cond ((memq name loaded)
		   (display loaded)(display " already loaded!")(newline) #f)
		  ((eq? name 'mizutamari)
		   (load (string-append MULASAME_BOOT_PATH "mizutamari"))
		   (set! loaded (cons name loaded)))
		  ((eq? name 'linda)
		   (load (string-append MULASAME_BOOT_PATH "linda"))
		   (set! loaded (cons name loaded)))
		  ((eq? name 'amb)
		   (lib-load "amb")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'pcall)
		   (lib-load "pcall")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'module)
		   (lib-load "module")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'schelog)
		   (lib-load "schelog")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'methods)
		   (lib-load "methods")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'meroon)
		   (lib-load "meroon")
		   (set! loaded (cons name loaded)))
		  ((eq? name 'tiny-clos)
		   (lib-load "tiny-clos")
		   (set! loaded (cons name loaded)))
		  (else (apply oldrequire args)))))))

(if (eq? (software-type) 'MACOS)
    (begin
      (transcript-on (string-append MULASAME_BOOT_PATH "MulasameMain.log"))
      (sub-transcript-on (string-append MULASAME_BOOT_PATH "MulasameSub.log"))))

(define (run)(%task-switch%)(run))
