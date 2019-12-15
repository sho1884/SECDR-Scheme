;;
;; errhook.scm
;;
;;========================================================================
;;  Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/errhook.scm,v 0.2 1994/09/14 02:55:46 s-haya Exp s-haya $
;;

;;;; A Error Hook Pakage for SECDR Scheme
;;;
;;;  Date written    06-Mar-1990 by Atsushi Moriwaki
;;;  Date revised    05-Mar-1993 by Shoichi Hayashi
;;;

(define -*-error-hook-*- (lambda () (%reset-master%)))

