;;
;; mklib.scm
;;
;;========================================================================
;;  Copyright(C) 1991,1992,1993 Shoichi Hayashi.
;;

(define %SECDRLIB_PATH%
  (case (software-type)
    ((UNIX) "../secdrlib/")
    ((MSDOS) "..\\secdrlib\\")
    ((MACOS) "::secdrlib:")))

(define %DELIM%
  (case (software-type)
    ((UNIX) "/")
    ((MSDOS) "\\")
    ((MACOS) ":")))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "amb" %DELIM% "stack.scm"))
(load (string-append %SECDRLIB_PATH% "amb" %DELIM% "amb.scm"))

(compile-file (string-append %SECDRLIB_PATH% "amb" %DELIM% "stack.scm")
              (string-append %SECDRLIB_PATH% "amb" %DELIM% "amb.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "amb.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "pcall" %DELIM% "splitter.scm"))
(load (string-append %SECDRLIB_PATH% "pcall" %DELIM% "susp.scm"))
(load (string-append %SECDRLIB_PATH% "pcall" %DELIM% "threads.scm"))

(compile-file (string-append %SECDRLIB_PATH% "pcall" %DELIM% "splitter.scm")
              (string-append %SECDRLIB_PATH% "pcall" %DELIM% "susp.scm")
              (string-append %SECDRLIB_PATH% "pcall" %DELIM% "threads.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "pcall.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "module" %DELIM% "module.scm"))

(compile-file (string-append %SECDRLIB_PATH% "module" %DELIM% "module.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "module.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "meroon" %DELIM% "meroon.secdr"))
(load (string-append %SECDRLIB_PATH% "meroon" %DELIM% "meroon.scm"))

(compile-file (string-append %SECDRLIB_PATH% "meroon" %DELIM% "meroon.secdr")
              (string-append %SECDRLIB_PATH% "meroon" %DELIM% "meroon.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "meroon.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "tiny-clos" %DELIM% "support.scm"))
(load (string-append %SECDRLIB_PATH% "tiny-clos" %DELIM% "tiny-clos.scm"))

(compile-file (string-append %SECDRLIB_PATH% "tiny-clos" %DELIM% "support.scm")
              (string-append %SECDRLIB_PATH% "tiny-clos" %DELIM% "tiny-clos.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "tiny-clos.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "methods" %DELIM% "methods.scm"))

(compile-file (string-append %SECDRLIB_PATH% "methods" %DELIM% "methods.scm")
	          (string-append %SECDRLIB_PATH% "bin" %DELIM% "methods.bin"))

;;
;;========================================================================
;;

(load (string-append %SECDRLIB_PATH% "schelog" %DELIM% "schelog.scm"))
;(load (string-append %SECDRLIB_PATH% "schelog" %DELIM% "bagof.scm"))

(compile-file (string-append %SECDRLIB_PATH% "schelog" %DELIM% "schelog.scm")
;              (string-append %SECDRLIB_PATH% "schelog" %DELIM% "bagof.scm")
                   (string-append %SECDRLIB_PATH% "bin" %DELIM% "schelog.bin"))
