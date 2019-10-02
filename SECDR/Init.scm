;;
;; SECDR Scheme Init.scm
;;
;;========================================================================
;;
;;   Copyright(C) 1992, 1993 Shoichi Hayashi.
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;   The author can be reached at s-haya@rst.fujixerox.co.jp or
;;   Shoichi Hayashi,
;;   SYSTEMS & COMMUNICATIONS LAB.  FUJI XEROX Co.,LTD.
;;   134 GODOCHO, HODOGAYA-KU, YOKOHAMA-SHI, KANAGAWA, 240, JAPAN
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/Init.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

(define (scheme-implementation-type) 'SECDR)
(define SECDR_BOOT_PATH (getenv "SECDR_BOOT_PATH"))
(define SECDR_LIBRARY_PATH (getenv "SECDR_LIBRARY_PATH")) ; secdrlib
(define SCHEME_LIBRARY_PATH (getenv "SCHEME_LIBRARY_PATH")) ; slib

(if (not SECDR_BOOT_PATH)
  (case (software-type)
    ((UNIX)  (set! SECDR_BOOT_PATH "/usr/PDS/lang/Scheme/SECDR/"))
    ((MSDOS) (set! SECDR_BOOT_PATH "A:\\Scheme\\SECDR\\"))
    ((MACOS) (set! SECDR_BOOT_PATH "SAZABI:Scheme:SECDR:"))))
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
          (if (string=? envname "SCHEME_BOOT_PATH")
            SCHEME_LIBRARY_PATH
            (oldgetenv envname)))))))

(define (terms)
  (list-file (string-append SECDR_BOOT_PATH "COPYING")))

(define (list-file file)
  (call-with-input-file file
    (lambda (inport)
      (do ((c (read-char inport) (read-char inport)))
	  ((eof-object? c))
	(write-char c)))))
    
(new-segment 50)

(define scm-load load)
(define load ex-load)
(define (lib-load libname opt1 opt2)
  (ex-load (string-append SECDR_LIBRARY_PATH libname) opt1 opt2))

(load (string-append (getenv "SCHEME_LIBRARY_PATH") "secdrscm.init") #f #t)

(lib-load "amb" #f #t)
(lib-load "pcall" #f #t)
;(lib-load "module" #f #t)
;(lib-load "schelog" #f #t)
;(lib-load "methods" #f #t)
;(lib-load "meroon" #f #t)
(lib-load "tiny-clos" #f #t)

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
    (if (eq? (software-type) 'MACOS)
        (begin (display "input next line!")(newline)))
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

(define append!
  (lambda args
    (cond ((null? args) '())
	  ((null? (cdr args)) (car args))
	  ((null? (car args)) (cadr args))
	  (else
	   (set-cdr! (last-pair (car args))
		     (apply append! (cdr args)))
	   (car args)))))

(define #\return (integer->char 13))
(define exit quit)

;(require 'macro)
;(define -*-eval-hook-*- macro:eval)
