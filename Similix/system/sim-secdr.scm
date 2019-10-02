; Similix load file for SECDR-Scheme
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;----------------------------------------------------------------------------

;****************************************************************************
;**** At this point: insert path name of the path where this file is located
(define **Similix-path** "/usr/PDS/lang/Scheme/Similix/system/")
;			  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;****************************************************************************



;============================================================================
;======== Scheme system dependent definitions begin here: ===================

(new-segment 200)

(define **Similix-source-suffix** ".scm")
(define **Similix-compiled-suffix** ".bin")
(define **Similix-compiled-sim-suffix** ".bin")


;****************************************************************************
;**** At this point: insert path name of the Slib path;
;**** the variable **scm-lib** is (only) used later in this file;
(define **scm-lib** SCHEME_LIBRARY_PATH)
;****************************************************************************

(define _sim-flush-output-port force-output)

(define (_sim-remove-file filename)
  (system (string-append "rm -f " filename)))

(define (_sim-compile-file filename)
  (define (separate-filename str)
    (let ((length (string-length str)))
      (if (and (> length 3)
	       (or (string-ci=? (substring str (- length 4) length) ".scm")
		   (string-ci=? (substring str (- length 4) length) ".sim")))
	  (cons (substring str 0 (- length 4))
		(substring str (- length 4) length))
	  (cons str ".scm"))))
  (let* ((sepa-name (separate-filename filename))
	 (complete-name (string-append (car sepa-name)(cdr sepa-name)))
	 (name (car sepa-name)))
    (compile-file complete-name (string-append name ".bin"))))

(define _sim-garbage-collect gc)

(define (_sim-ntimes suspension n)
  (_sim-garbage-collect)
  (let ((t1 (get-internal-run-time)))
    (let loop ((i 1))
      (if (< i n)
	  (begin (suspension)
		 (loop (+ i 1)))))
    (let ((result (suspension)))
      (display "Run time: ")
      (display (/ (- (get-internal-run-time) t1)
		  internal-time-units-per-second))
      (display " seconds")
      (newline)
      result)))

(require 'pretty-print)
(define _sim-pretty-print pretty-print)

(require 'format) ; defines format
(define _sim-error
  (lambda (f . args)
    (display "ERROR in ")
    (display f)
    (display ": ")
    (display (apply format (cons #f args))) ; format is loaded above
    (newline)
    (error)))

(define (makeSimilix)
  (let ((**Current-path** (pwd)))
    (cd **Similix-path**)
    (_sim-compile-file "abssyn")
    (_sim-compile-file "bt-eod")
    (_sim-compile-file "constr")
    (_sim-compile-file "front")
    (_sim-compile-file "lam-lift")
    (_sim-compile-file "langext")
    (_sim-compile-file "loadsysf")
    (_sim-compile-file "miscspec")
    (_sim-compile-file "oc")
    (_sim-compile-file "post")
    (_sim-compile-file "rl")
    (_sim-compile-file "runtime")
    (_sim-compile-file "sp")
    (_sim-compile-file "util")
    (_sim-compile-file "cogen.sim")
    (_sim-compile-file "spec.sim")
    (_sim-compile-file "sim-secdr")
    (cd **Current-path**)))

(if (file-exists? (string-append **Similix-path** "sim-secdr.bin"))
    #t (makeSimilix))

;======== Scheme system dependent definitions end here ======================
;============================================================================


;(load (string-append **Similix-path** "loadsysf" **Similix-compiled-suffix**))
(load (string-append **Similix-path** "loadsysf"))

;----------------------------------------------------------------------------
