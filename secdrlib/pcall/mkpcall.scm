;;
;; mkpcall.scm
;;
;;========================================================================
;;  Copyright(C) 1993 Shoichi Hayashi.
;;

(load "splitter.scm")
(load "susp.scm")
(load "threads.scm")

(compile-file "splitter.scm"
              "susp.scm"
              "threads.scm"
;; Compiled code is written in "pcall.bin".
	          "pcall.bin")
