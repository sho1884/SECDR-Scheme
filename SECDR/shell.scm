;;
;; shell.scm
;;

;;;; A Shell Pakage for SECDR Scheme
;;;
;;;  Date written    11-Feb-1990 by Atsushi Moriwaki
;;;  Date revised    4-June-1992 by Shoichi Hayashi
;;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/shell.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

;; The following are depend on your system.
(define my-ls "ls -F ")
(define my-rm "rm ")
;(define my-cd "cd ")
(define my-editor "emacs ")

(define (ls . rex)
  (if (null? rex)
      (system my-ls)
      (system (string-append my-ls (car rex)))))

(define (rm file)
  (system (string-append my-rm file)))

;(define (cd directory)
;  (system (string-append my-cd directory)))

(define (cd . directory)
  (if (null? directory)
      (chdir (getenv "HOME"))
      (chdir (car directory))))

(define (ed file)
  (system (string-append my-editor file))
  (display "Load?(Y/N) ")
  (let ((ans (read)))
  	(if (or (eq? ans 'Y) (eq? ans 'y))
		(load file))))

