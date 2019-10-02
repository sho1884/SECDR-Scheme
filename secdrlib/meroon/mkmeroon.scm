;;
;; mkmeroon.scm
;;
;;========================================================================
;;  Copyright(C) 1992,1993 Shoichi Hayashi.
;;

(load "meroon.secdr")
(load "meroon.scm")

(compile-file "meroon.secdr"
              "meroon.scm"
;; Compiled code is written in "meroon.bin".
	          "meroon.bin")
