;;
;;      -- rewrite for SECDR-Scheme --
;;      Date revised    28-May-1992 by Shoichi Hayashi
;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/extend.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;
; This code, contributed by Jeff De Vries, has been modified to remove the
; assumption that #f is the same as the empty list.  Three changes were
; required: changing a null? to a not in add-car, add-cdr, and gen.
;
; From: jdevries@ads.arpa (Jeff De Vries)
; Subject: Extend-Syntax for Everybody!
; Date: 15 Nov 87 20:54:13 GMT
; 
; By popular demand I have decided to go ahead and post the code for the
; MacScheme version of extend-syntax.  I had (have?) over 40 requests,
; (some of which I can't seem to respond to due to always getting bounced),
; plus other indicators that some people are just waiting for me to post it.
; To those of you who have no interest in this, I apologize for the long
; posting, (just hit your 'junk' key, if you have one).  
; 
; But first, a few words:
; 
; The theoretical work and basic design behind extend-syntax was the work of 
; Eugene Kohlbecker.  It was part of his Ph.D. dissertation, "Syntactic 
; Extensions in the Programming Language LISP", (Indiana University, 1986).  
; The enhanced version of the code that I used for the MacScheme version was 
; written by R. Kent Dybvig, and made available by him.
; 
; A more complete description of Kent's book is:
;  The Scheme Programming Language
;  R. Kent Dybvig
;  Prentice-Hall, Englewood Cliffs, New Jersey, 07632 (1987)
;  Library of Congress Catalog Card Number 86-63489
; 
; If you are using a version of Scheme other than MacScheme, you should be
; able to convert this to whatever you are using.  The main thing to change
; is the way macros are defined.  There are two macros, (extend-syntax and
; extend-syntax/code), plus the macro defining form embedded inside of
; extend-syntax.  You may have to add (or delete) a support function or two.
; 
; ENJOY!!! :-)
; 
; Jeff
; 
; ------------------------distribution starts here------------------------
; Here is the code for extend-syntax.  It includes the code for:
;  when
;  unless
;  andmap
;  syntax-match?
;  extend-syntax
;  extend-syntax/code
; 
; To load it, just enter
;  (load "extend.sch")
; 
; It takes a while to load and will print out:
;  when
;  unless
;  andmap
;  syntax-match?
;  extend-syntax/code
; (note: extend-syntax gets compiled even though its name doesn't get
;  printed.  It doesn't get printed because it's inside the LET)
; 
; After you load it, you may want to do a (dumpheap)  See the MacScheme
; manual for details.
; 
; The documentation for extend-syntax is in "The Scheme Programming
; Language" by R. Kent Dybvig.  Buy the book.  (No, I don't get any
; kickbacks).  extend-syntax/code returns the source for the
; lambda expression that would have been bound to the macro, which is
; helpful during debugging and for getting a feel for how extend-syntax
; works.  You might try (pretty-print (extend-syntax/code --- etc. if
; you want to be able to read it easily.  Note that the output isn't
; directly useable because of gensym'ed variables and how MacScheme
; prints quasiquotes, etc.  Use extend-syntax to make the macros.
; 
; If you have any comments or problems, feel free to contact me.  I won't
; promise anything, but I'll give it a look.  If you port the code to another
; version of Scheme, I would be interested in hearing about it.
; 
; Jeff De Vries
; (ARPA: jdevries@ads.arpa)
; DISCLAIMER: All the usual stuff...
; 
; -----------------------------snip here---------------------------------
;;; extend.scm
;;; Copyright (C) 1987 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful noncommercial purpose, and to redistribute
;;; this software is granted subject to the restriction that all copies
;;; made of this software must include this copyright notice in full.
;;; Cadence makes no warranties or representations of any kind, either
;;; express or implied, including but not limited to implied warranties
;;; of merchantability or fitness for any particular purpose.

;;; The basic design of extend-syntax is due to Eugene Kohlbecker.  See
;;; "E. Kohlbecker: Syntactic Extensions in the Programming Language Lisp",
;;; Ph.D.  Dissertation, Indiana University, 1986."  The structure of "with"
;;; pattern/value clauses, the method for compiling extend-syntax into
;;; Scheme code, and the actual implementation are due to Kent Dybvig.

;;; Made available courtesy R. Kent Dybvig
;;; MacScheme conversion by Jeff De Vries
;;; note: requires the use of MacScheme Version 1.2 or greater

;;;
;;; Rewrite for SECDR Scheme by Atsushi Moriwaki. 05-Mar-1990
;;;

;;; the following routines are provided for compatibility with TSPL:
;; In SECDR Scheme, when and unless are defined in macros.scm
;(macro when
;       (lambda (args)
;         `(if ,(cadr args)
;              (begin ,@(cddr args))
;              #f)))
;(macro unless
;       (lambda (args)
;         `(if ,(cadr args)
;              #t
;              (begin ,@(cddr args)))))

(define (andmap p . args)
  ;; use "first-finish" rule
  (let andmap ((args args) (value #t))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (and value (andmap (map cdr args) value))))))

;;; syntax-match? is used by extend-syntax to choose among clauses and
;;; to check for syntactic errors.  It is also available to the user.
(define syntax-match?
  (lambda (keys pat exp)
    (cond
     ((symbol? pat) (if (memq pat keys) (eq? exp pat) #t))
     ((pair? pat)
      (if (equal? (cdr pat) '(...))
          (let f ((lst exp))
            (or (null? lst)
                (and (pair? lst)
                     (syntax-match? keys (car pat) (car lst))
                     (f (cdr lst)))))
          (and (pair? exp)
               (syntax-match? keys (car pat) (car exp))
               (syntax-match? keys (cdr pat) (cdr exp)))))
     (else (equal? exp pat)))))

;;; The main code!
(let ()
  (define id
    (lambda (name access control)
      (list name access control)))
  (define id-name car)
  (define id-access cadr)
  (define id-control caddr)
  
  (define loop
    (lambda ()
      (box '())))
  (define loop-ids unbox)
  (define loop-ids! set-box!)
  
  (define c...rs
    `((car caar . cdar)
      (cdr cadr . cddr)
      (caar caaar . cdaar)
      (cadr caadr . cdadr)
      (cdar cadar . cddar)
      (cddr caddr . cdddr)
      (caaar caaaar . cdaaar)
      (caadr caaadr . cdaadr)
      (cadar caadar . cdadar)
      (caddr caaddr . cdaddr)
      (cdaar cadaar . cddaar)
      (cdadr cadadr . cddadr)
      (cddar caddar . cdddar)
      (cdddr cadddr . cddddr)))
  
  (define add-car
    (lambda (access)
      (let ((x (and (pair? access) (assq (car access) c...rs))))
        (if (not x)
            `(car ,access)
            `(,(cadr x) ,@(cdr access))))))
  
  (define add-cdr
    (lambda (access)
      (let ((x (and (pair? access) (assq (car access) c...rs))))
        (if (not x)
            `(cdr ,access)
            `(,(cddr x) ,@(cdr access))))))
  
  (define parse
    (lambda (keys pat acc cntl ids)
      (cond
       ((symbol? pat)
        (if (memq pat keys)
            ids
            (cons (id pat acc cntl) ids)))
       ((pair? pat)
        (if (equal? (cdr pat) '(...))
            (let ((x (gensym)))
              (parse keys (car pat) x (id x acc cntl) ids))
            (parse keys
                   (car pat)
                   (add-car acc)
                   cntl
                   (parse keys (cdr pat) (add-cdr acc) cntl ids))))
       (else ids))))
  
  (define gen
    (lambda (keys exp ids loops)
      (cond
       ((symbol? exp)
        (let ((id (lookup exp ids)))
          (if (not id)
              exp
              (begin
               (add-control! (id-control id) loops)
               (list 'unquote (id-access id))))))
       ((pair? exp)
        (cond
         ((eq? (car exp) 'with)
          (unless (syntax-match? '(with) '(with ((p x) ...) e) exp)
                  (error 'extend-syntax "invalid 'with' form " exp))
          (list 'unquote
                (gen-with
                 keys
                 (map car (cadr exp))
                 (map cadr (cadr exp))
                 (caddr exp)
                 ids
                 loops)))
         ((and (pair? (cdr exp)) (eq? (cadr exp) '...))
          (let ((x (loop)))
            (make-loop
             x
             (gen keys (car exp) ids (cons x loops))
             (gen keys (cddr exp) ids loops))))
         (else
          (let ((a (gen keys (car exp) ids loops))
                (d (gen keys (cdr exp) ids loops)))
            (if (and (pair? d) (eq? (car d) 'unquote))
                (list a (list 'unquote-splicing (cadr d)))
                (cons a d))))))
       (else exp))))
  
  (define gen-with
    (lambda (keys pats exps body ids loops)
      (if (null? pats)
          (make-quasi (gen keys body ids loops))
          (let ((p (car pats)) (e (car exps)) (g (gensym)))
            `(let ((,g ,(gen-quotes keys e ids loops)))
                  (if (syntax-match? '() ',p ,g)
                      ,(gen-with
                        keys
                        (cdr pats)
                        (cdr exps)
                        body
                        (parse '() p g '() ids)
                        loops)
                      (error ',(car keys)
                             "does not fit 'with' pattern "
                             ,g
                              ',p)))))))
  
  (define gen-quotes
    (lambda (keys exp ids loops)
      (cond
       ((syntax-match? '(quote) '(quote x) exp)
        (make-quasi (gen keys (cadr exp) ids loops)))
       ((pair? exp)
        (cons (gen-quotes keys (car exp) ids loops)
              (gen-quotes keys (cdr exp) ids loops)))
       (else exp))))
  
  (define lookup
    (lambda (sym ids)
      (let loop ((ls ids))
        (cond
         ((null? ls) #f)
         ((eq? (id-name (car ls)) sym) (car ls))
         (else (loop (cdr ls)))))))
  
  (define add-control!
    (lambda (id loops)
      (unless (null? id)
              (when (null? loops)
                    (error 'extend-syntax "missing ellipsis in expansion"))
              (let ((x (loop-ids (car loops))))
                (unless (memq id x)
                        (loop-ids! (car loops) (cons id x))))
              (add-control! (id-control id) (cdr loops)))))
  
  (define make-loop
    (lambda (loop body tail)
      (let ((ids (loop-ids loop)))
        (when (null? ids)
              (error 'extend-syntax "extra ellipsis in expansion"))
        (cond
         ((equal? body (list 'unquote (id-name (car ids))))
          (if (null? tail)
              (list 'unquote (id-access (car ids)))
              (cons (list 'unquote-splicing (id-access (car ids)))
                    tail)))
         ((and (null? (cdr ids))
               (syntax-match? '(unquote) '(unquote (f x)) body)
               (eq? (cadadr body) (id-name (car ids))))
          (let ((x `(map ,(caadr body) ,(id-access (car ids)))))
            (if (null? tail)
                (list 'unquote x)
                (cons (list 'unquote-splicing x) tail))))
         (else
          (let ((x `(map (lambda ,(map id-name ids) ,(make-quasi body))
                         ,@(map id-access ids))))
            (if (null? tail)
                (list 'unquote x)
                (cons (list 'unquote-splicing x) tail))))))))
  
  (define make-quasi
    (lambda (exp)
      (if (and (pair? exp) (eq? (car exp) 'unquote))
          (cadr exp)
          (list 'quasiquote exp))))
  
  (define make-clause
    (lambda (keys cl x)
      (cond
       ((syntax-match? '() '(pat fender exp) cl)
        (let ((pat (car cl)) (fender (cadr cl)) (exp (caddr cl)))
          (let ((ids (parse keys pat x '() '())))
            `((and (syntax-match? ',keys ',pat ,x)
                   ,(gen-quotes keys fender ids '()))
              ,(make-quasi (gen keys exp ids '()))))))
       ((syntax-match? '() '(pat exp) cl)
        (let ((pat (car cl)) (exp (cadr cl)))
          (let ((ids (parse keys pat x '() '())))
            `((syntax-match? ',keys ',pat ,x)
              ,(make-quasi (gen keys exp ids '()))))))
       (else
        (error 'extend-syntax "invalid clause " cl)))))
  
  (define make-syntax
    (let ((x (gensym "x")))
      (lambda (keys clauses)
        `(lambda (,x)
                 (cond
                  ,@(map (lambda (cl) (make-clause keys cl x)) clauses)
                    (else
                     (error ',(car keys) "invalid syntax " ,x)))))))
  
  (macro extend-syntax
         (lambda (x)
           (cond
            ((and
              (syntax-match?
               '(extend-syntax)
               '(extend-syntax (key1 key2 ...) clause ...)
               x)
              (andmap symbol? `(,(caadr x) ,@(cdadr x))))
             (let
               ((f (make-syntax `(,(caadr x) ,@(cdadr x)) (cddr x))))
               (if (syntax-match? '() 'proc f)
                   `(macro ,(caadr x) ,f)
                   (error 'extend-syntax
                          "does not fit 'with' pattern "
                          f
                          'proc))))
            (else (error 'extend-syntax "invalid syntax " x)))))
  
  (macro extend-syntax/code
         (lambda (x)
           (cond
            ((and
              (syntax-match?
               '(extend-syntax/code)
               '(extend-syntax/code (key1 key2 ...) clause ...)
               x)
              (andmap symbol? `(,(caadr x) ,@(cdadr x))))
             (let
               ((f (make-syntax `(,(caadr x) ,@(cdadr x)) (cddr x))))
               (if (syntax-match? '() 'proc f)
                   `',f
                      (error 'extend-syntax/code
                             "does not fit 'with' pattern "
                             f
                             'proc))))
            (else (error 'extend-syntax/code "invalid syntax " x)))))
  
  ) ;;; end of let
;;; end extend.scm
