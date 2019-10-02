;\Title{MODULE}
;$Id: module.s 1.1 92/06/11 16:13:19 kobler Stab $
;$Log:	module.s $
;Revision 1.1  92/06/11  16:13:19  kobler
;Initial revision
;
;\Description
; A modul comprises constants, data types, variables, and procedures. Some
; of these parts are made available to other modules by exporting their names.
; However, the internal structure, the implementation of the module, is
; hidden from other modules.
;
;\Import
;---
;
;\Export
;(MODULE <module>
;  <EXPORT>
;  <IMPORT>
;  <INIT>
;  <definitions>)
; Define a module. A module exports all those objects associated with the
; names specified in the <EXPORT> section, and it is based on those
; imported by the <IMPORT> section. <INIT> allows to do some initializations
; at load time. The <definitions> build the module's body.
;
; <EXPORT>
;   syntax: (EXPORT <name1> <name2> ...)
;     The module <module> provides the objects associated with <name1>,
;     <name2>, etc. Renaming is done by replacing <namei> with
;     (<namei> AS <newnamei>).
; <IMPORT>
;  syntax: (IMPORT <imodule>)
;    Import all names exported by module <imodule>.
;  syntax: (IMPORT QUALIFIED <imodule>)
;    Import all names exported by module <imodule>. All names are `qualified'
;    (i.e.\ preceeded) by `<imodule>:'.
;  syntax: (FROM <imodule> IMPORT <iname1> ... [QUALIFIED <iqname1> ...])
;    Import the specified names (unqualified/qualified) from module <imodule>.
;    Again, it is possible to rename a <i(q)name> by replacing it with
;    (<i(q)name> AS <new i(q)name>).
; <INIT>
;  A sequence of expressions to be performed when loading the module.
; <definitions>
;  The body of the module. Think of these definitions as if they were
;  part of a big letrec, i.e.\ each definition may refer to another
;  definition of <definitions>.
;
; Another macro is provided: (EXPORTED BY <module>). It returns the
; list of names exported by <module>.
;
; Additionally there is a macro that allows to import an object from a
; module WITHOUT automatic definition of a name for it:
; (IMPORT <name> FROM <module>)
;
;
;\Implementation
; Return a list of all names a module exports.
;
(extend-syntax (EXPORTED BY)
  ((EXPORTED BY <module>)
   (map car <module>)))

; The special form {\tt IMPORT} retrieves the object associated with
; \Meta{name} from module \Meta{module}. If no object is associated
; \ScmFALSE\ will be returned.
;
(define import-from
  (lambda (name modul)
    (let ((probe (assq name modul)))
      (if probe
          (cdr probe)
          (error "Name not exported by module" (list name modul))))))

(extend-syntax (IMPORT FROM QUALIFIED)
  ((IMPORT <name> FROM <module>)
   (import-from '<name> <module>))
  ((IMPORT QUALIFIED <module>)
   (with (((<names> ...) (EXPORTED BY (eval '<module>))))
         (FROM <module> IMPORT QUALIFIED <names> ...)))
  ((IMPORT <module>)
   (with (((<names> ...) (EXPORTED BY (eval '<module>))))
         (FROM <module> IMPORT <names> ...))))

; Make available a {\it module's names\/} by importing and subsequently
; defining them in the current environment. Use this syntax only
; in contexts where `internal definitions' are allowed (see \RfourRS).
; Additionally, it is possible to determine an individual name for
; an imported service.
;
(let
  ;
  ; Qualifying a \Meta{name} exported by a \Meta{module} means to prefix
  ; the \Meta{name} with the \Meta{module}'s name followed
  ; by a `:'.
  ;
  ((make-qualified-name
     (lambda (m n)
       (string->symbol
         (string-append (symbol->string m) ":" (symbol->string n))))))

  ; Import a list of objects from a \Meta{module} defining \Meta{names}
  ; the objects are to be refered to. Various ways of determing a \Meta{name}
  ; are available (see explanation of the patterns below).
  ;
  (extend-syntax (FROM IMPORT QUALIFIED AS)

    ; Qualified and modified name.
    ((FROM <module> IMPORT QUALIFIED (<name> AS <newName>))
     (with ((<qualifiedNewName> (make-qualified-name (quote <module>)
                                                     (quote <newName>))))
           (define <qualifiedNewName> (IMPORT <name> FROM <module>))))

    ; Qualified name.
    ((FROM <module> IMPORT QUALIFIED <name>)
     (with ((<qualifiedName> (make-qualified-name (quote <module>)
                                                  (quote <name>))))
           (define <qualifiedName> (IMPORT <name> FROM <module>))))

    ; Modified name.
    ((FROM <module> IMPORT (<name> AS <newName>))
     (define <newName> (IMPORT <name> FROM <module>)))

    ; Original name.
    ((FROM <module> IMPORT <name>)
     (define <name> (IMPORT <name> FROM <module>)))

    ; Import a sequence of qualified names.
    ((FROM <module> IMPORT QUALIFIED <name> <names> ...)
     (begin
       (FROM <module> IMPORT QUALIFIED <name>)
       (FROM <module> IMPORT QUALIFIED <names> ...)))

    ; Import a list of unqualified names (maybe followed by qualified ones).
    ((FROM <module> IMPORT <name> <names> ...)
     (begin
       (FROM <module> IMPORT <name>)
       (FROM <module> IMPORT <names> ...))))

) ;< end of \Code{let}

; Extend the {\tt (export \dots)} syntax.
;
(extend-syntax (EXTEND-EXPORT AS)
  ((EXTEND-EXPORT)
   '())
  ((EXTEND-EXPORT (<name> AS <newName>) <names> ...)
   (cons (cons (quote <newName>) <name>)
         (EXTEND-EXPORT <names> ...)))
  ((EXTEND-EXPORT <name> <names> ...)
   (cons (cons (quote <name>) <name>)
         (EXTEND-EXPORT <names> ...))))

; Extend a module definition's body.
;
(extend-syntax (EXTEND-MODULE EXPORT FROM IMPORT INIT)
  ; The body imports something from another module \dots
  ((EXTEND-MODULE
     (EXPORT <eName1> ...)
     (IMPORT <iModule> ...)
     <body> ...)
   (let ()
     (IMPORT <iModule> ...)
     (EXTEND-MODULE (EXPORT <eName1> ...)
                    <body> ...)))

  ((EXTEND-MODULE
     (EXPORT <eName1> ...)
     (FROM <iModule> IMPORT <iName1> ...)
     <body> ...)
   (let ()
     (FROM <iModule> IMPORT <iName1> ...)
     (EXTEND-MODULE (EXPORT <eName1> ...)
                    <body> ...)))

  ; There is nothing to import \dots
  ((EXTEND-MODULE
     (EXPORT <eName1> ...)
     (INIT <init> ...)
     <body> ...)
   (letrec
     (<body> ...)
     <init> ...
     (EXTEND-EXPORT <eName1> ...)))

  ; There is nothing to import and no initialization \dots
  ((EXTEND-MODULE
     (EXPORT <eName1> ...)
     <body> ...)
   (letrec
     (<body> ...)
     (EXTEND-EXPORT <eName1> ...))))

; Define a new module. The module's name is first class and contains the
; exported names with the associated values (i.e.\ a simple association list).
;
(extend-syntax (MODULE EXPORT)
  ((MODULE <name>
           (EXPORT <eName1> ...)
           <body> ...)
   (define <name>
     (EXTEND-MODULE (EXPORT <eName1> ...)
                    <body> ...))))
