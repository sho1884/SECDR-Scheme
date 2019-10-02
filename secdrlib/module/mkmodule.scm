;;
;; mkmodule.scm
;;
;;========================================================================
;;

(load "module.scm")

(compile-file "module.scm"
;; Compiled code is written in "module.bin".
	          "module.bin")
