(require 'dynamic-wind)

(for-each
 (lambda (filename)
   (compile-file (string-append SCHEME_LIBRARY_PATH filename ".scm")
		 (string-append SCHEME_LIBRARY_PATH filename ".bin")))
 '("alist"
   "array"
   "charplot"
;  "collect"
   "comlist"
   "debug"
   "defmacro"
   "dwindtst"
   "dynamic"
   "dynwind"
;  "fluidlet"
   "format"
   "formatfl"
   "formatst"
   "genwrite"
   "hash"
   "hashtab"
   "line-io"
   "logical"
;  "macrotst"
   "macwork"
   "modular"
   "mulapply"
   "mularg"
   "mwdenote"
   "mwexpand"
   "mwsynrul"
   "mwtest"
   "obj2str"
   "plottest"
   "pp"
   "ppfile"
   "prime"
   "priorque"
   "process"
   "promise"
   "queue"
   "r4rsyn"
   "randinex"
   "random"
   "ratize"
   "rb-tree"
   "rbt-test"
   "record"
   "repl"
   "require"
   "sc-macro"
   "sc2"
   "sc3"
   "sc4-sc3"
   "sc4opt"
   "sca-expp"
   "sca-glob"
   "sca-init"
;  "sca-macr"
   "sca-outp"
   "scmactst"
   "sort"
   "stdio"
   "str-case"
   "strport"
;  "struct"
;  "structst"
;  "structure"
   "synchk"
   "synclo"
   "synrul"
   "tek40"
   "tek41"
   "test"
   "trnscrpt"
   "values"
   "withfile"
;  "yasos"
))