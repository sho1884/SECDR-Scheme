;;
;; mkboot.scm
;;
;;========================================================================
;;  Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/mkboot.scm,v 0.2 1994/09/14 02:55:46 s-haya Exp s-haya $
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

