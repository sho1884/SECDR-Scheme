;;
;; mkboot.scm
;;
;;========================================================================
;;  Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/mkboot.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

(new-segment 20)

;; Fisrt load *.scm in order to work macro correctly.

(load "errhook.scm")
(load "evalhook.scm")
(load "compiler.scm")
(load "macros.scm")
(load "basic.scm")
(load "pprint.scm")
(load "uniqprnt.scm")
(load "trace.scm")
(load "shell.scm")
(load "extend.scm")
(load "disasm.scm")
(compile-file "errhook.scm"
              "evalhook.scm"
              "compiler.scm"
              "macros.scm"
              "basic.scm"
              "pprint.scm"
              "uniqprnt.scm"
              "trace.scm"
              "shell.scm"
              "extend.scm"
              "disasm.scm"
;; Compiled code is written in "boot.bin".
			  "boot.bin")

