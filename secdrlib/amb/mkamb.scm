;;
;; mkamb.scm
;;
;;========================================================================
;;  Copyright(C) 1991,1992,1993 Shoichi Hayashi.
;;

(load "stack.scm")
(load "amb.scm")

(compile-file "stack.scm"
              "amb.scm"
;; Compiled code is written in "amb.bin".
	          "amb.bin")
