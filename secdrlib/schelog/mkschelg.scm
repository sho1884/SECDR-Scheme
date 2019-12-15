;;
;; mkschelg.scm
;;

;;
;; Makefile of schelog for SECDR Scheme
;;
;; To make schelog.bin, excute (load "mkschelg.scm").
;;

(load "schelog.scm")

(compile-file "schelog.scm" "schelog.bin")

;;
;; For loading schelog.bin, input command (bin-load "schelog.bin").
;;