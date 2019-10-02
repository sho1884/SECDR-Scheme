;;
;; mkclos.scm
;;
;;========================================================================
;;  Copyright(C) 1992,1993 Shoichi Hayashi.
;;

(load "support.scm")
(load "tiny-clos.scm")

(compile-file "support.scm"
              "tiny-clos.scm"
;; Compiled code is written in "tiny-clos.bin".
	          "tiny-clos.bin")
