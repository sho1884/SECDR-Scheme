;;
;; mkmethod.scm
;;
;;========================================================================
;;

(load "methods.scm")

(compile-file "methods.scm"
;; Compiled code is written in "methods.bin".
	          "methods.bin")
