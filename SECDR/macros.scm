;;
;; macros.scm
;;

;;;; A Macro-Special Form Pakage for SECDR Scheme
;;;
;;;   Date written    06-Mar-1990 by Atsushi Moriwaki
;;;   Date revised    11-Mar-1993 by Shoichi Hayashi
;;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/macros.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

;;
;; -- macro --
;;
;; (macro <name> <defintion>)
;; = (begin
;;     (put '<name> '%macro-special-form% <definition>)
;;     '<name>)
;;
(begin
  (put 'macro '%macro-special-form%
    (lambda (l)
      `(begin (put ',(cadr l) '%macro-special-form% ,(caddr l))
              ',(cadr l))))
  'macro)

;;
;; -- sequence --
;;
;; (sequence <sequenece>)
;; = ((lambda () <sequence>))
;;
(macro sequence
  (lambda (l)
    `((lambda () ,@(cdr l)))))

;;
;; -- let --
;;
;; (let ((<var1> <init1>) ...) <body>)
;; = ((lambda (<var1> ...) <body>) <init1> ...)
;;
;; (let <name> ((<var1> <init1>) ...) <body>)
;; = (((lambda (<name>) (set! <name> (lambda (<var1> ...) <body>))) '())
;;    <init1> ...)
;;
(macro let
  (lambda (l)
    (if (symbol? (cadr l)) ; named let
        `(((lambda (,(cadr l))
            (set! ,(cadr l)
                  (lambda ,(map (lambda (a) (car a)) (caddr l))
                          ,@(cdddr l))))
              '())
            ,@(map (lambda (a) (cadr a)) (caddr l)))
        `((lambda ,(map (lambda (a) (car a)) (cadr l)) ,@(cddr l))
           ,@(map (lambda (a) (cadr a)) (cadr l))))))

;;
;; -- letrec --
;;
;; (letrec ((<var1> <init1>) ...) <body>)
;; = ((lambda (<var1> ...) (set! <var1> <init1>) ... <body>) '() ...)
;;
(macro letrec
  (lambda (s)
    (define (mk-body l)
        (if (null? l)
          (cddr s)
          (cons `(set! ,(caar l) ,(cadar l))
                (mk-body (cdr l)))))
    `((lambda ,(map (lambda (a) (car a)) (cadr s)) ,@(mk-body (cadr s)))
       ,@(map (lambda (a) ''()) (cadr s)))))

;;
;; -- let* --
;;
;; (let* () <body>)
;; = ((lambda () <body>))
;; (let* ((<var1> <init1>) (<var2> <init2>) ...) <body>)
;; = ((lambda (<var1>) (let* ((<var2> <init2>) ...) <body>)) <init1>)
;;
(macro let*
  (lambda (l)
    (let ((init (cadr l))
          (body (cddr l)))
      (if (null? init)
        `((lambda () ,@body))
        `((lambda (,(caar init)) (let* ,(cdr init) ,@body))
          ,(cadar init))))))

;;
;; -- cond --
;;
;; (cond (<test> <sequenece>) <cause2> ...)
;; = (if <test> (begin <sequence>) (cond <cause2> ...))
;; (cond (<test>) <cause2> ...)
;; = (or <test> (cond <cause2> ...)
;; (cond (<test> => <recipient>) <cause2> ...)
;; = (let ((test-result <test>)
;;         (thunk2 (lambda () <recipient>))
;;         (thunk3 (lambda () (cond <cause2> ...))))
;;     (if test-result
;;       ((thunk2) test-result)
;;       (thunk3)))
;; (cond) = '()
;;
(macro cond
  (lambda (l)
    (cond
      ((null? (cdr l)) ''())
      ((pair? (cadr l))
        (let ((cause1 (cadr l)))
          (if (null? (cdr cause1))
            `(or ,(car cause1) (cond ,@(cddr l)))
             (if (eq? (cadr cause1) '=>)
               `(let ((test-result ,(car cause1))
                      (thunk2 (lambda () ,@(cddr cause1)))
                      (thunk3 (lambda () (cond ,@(cddr l)))))
                  (if test-result
                    ((thunk2) test-result)
                    (thunk3)))
               `(if ,(car cause1)
                  (begin ,@(cdr cause1))
                  (cond ,@(cddr l)))))))
      (#t (error "Syntax error in cond")))))

;;
;; -- and --
;;
;; (and) = #t
;; (and <test>) = <test>
;; (and <test1> <test2> ...)
;; = ((lambda (x thunk) (if x (thunk) x))
;;     <test1>
;;     (lambda () (and <test2> ...)))
;;
(macro and
  (lambda (l)
    (cond ((null? (cdr l)) #t)
          ((null? (cddr l)) (cadr l))
          (#t
            `((lambda (x thunk) (if x (thunk) x))
               ,(cadr l)
               (lambda () (and ,@(cddr l))))))))

;;
;; -- or --
;;
;; (or) = #f
;; (or <test>) = <test>
;; (or <test1> <test2> ...)
;; = ((lambda (x thunk) (if x x (thunk)))
;;     <test1>
;;     (lambda () (or <test2> ...)))
;;
(macro or
  (lambda (l)
    (cond ((null? (cdr l)) #f)
          ((null? (cddr l)) (cadr l))
          (#t
            `((lambda (x thunk) (if x x (thunk)))
                ,(cadr l)
                (lambda () (or ,@(cddr l))))))))

;;
;; -- case --
;;
;; (case <key> ((d1 ...) <sequence>) ...)
;; = (let ((key <key>))
;;     (cond ((memv key '(d1 ...)) <sequence>)
;;           ...))
;;
(macro case
  (lambda (l)
    `(let ((key ,(cadr l)))
      (cond
        ,@(map
            (lambda (a)
              (if (or (eq? (car a) 'else)
                      (eq? (car a) '#t))
                `(#t ,@(cdr a))
                `((memv key ',(car a)) ,@(cdr a))))
            (cddr l))))))

;;
;; -- do --
;;
;; (do ((<var1> <init1> <step1>) ...)
;;   (<test> <sequence>)
;;   <command1> ...)
;; = (let <loop> ((<var1> <init1>) ...)
;;     (if <test>
;;       (begin <sequence>)
;;       (begin <command1>
;;              ...
;;              (<loop> <step1> ...))))
;;
(macro do (lambda (do-macro)
  (apply (lambda (vars endtest . body)
           (let ((do-loop (gensym "loop")))
             `(let ,do-loop
                    ,(map (lambda (x) (list (car x) (cadr x))) vars)
                (if ,(car endtest)
                  (begin ,@(cdr endtest))
                  (begin ,@body
                         (,do-loop
                           ,@(map (lambda (x)
                                    (if (pair? (cddr x))
                                      (caddr x)
                                      (car x)))
                                  vars)))))))
         (cdr do-macro))))

;;
;; -- quasiquote --
;;
;; The following quasiquote macro is due to Eric S. Tiedemann.
;;
(macro quasiquote
 (lambda (l)
   (define (mcons f l r)
     (if (and (pair? r)
              (eq? (car r) 'quote)
              (eq? (cadr r) (cdr f))
              (pair? l)
              (eq? (car l) 'quote)
              (eq? (cadr l) (car f)))
         (list 'quote f)
         (list 'cons l r)))
   (define (mappend f l r)
     (if (or (null? (cdr f))
             (and (pair? r)
                  (eq? (car r) 'quote)
                  (eq? (cadr r) '())))
         l
         (list 'append l r)))
   (define (mvec form v v1)
     (if (and (pair? v1) (eq? (car v1) 'quote) (eq? (cadr v1) v))
         form
         (list 'list->vector v1)))
   (define (foo level form)
     (cond ((vector? form)
            (let ((v (vector->list form)))
              (mvec form v (foo level v))))
           ((atom? form) (list 'quote form))
           ((eq? 'quasiquote (car form))
            (mcons form ''quasiquote (foo (1+ level) (cdr form))))
           (#t (if (zero? level)
                   (cond ((eq? (car form) 'unquote) (cadr form))
                         ((eq? (car form) 'unquote-splicing)
                          (error "Unquote-splicing wasn't in a list: "
                                 form))
                         ((and (pair? (car form))
                               (eq? (caar form) 'unquote-splicing))
                          (mappend form (cadar form)
                                   (foo level (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))
                   (cond ((eq? (car form) 'unquote)
                          (mcons form ''unquote (foo (-1+ level)
                                                     (cdr form))))
                         ((eq? (car form) 'unquote-splicing)
                          (mcons form ''unquote-splicing
                                      (foo (-1+ level) (cdr form))))
                         (#t (mcons form (foo level (car form))
                                         (foo level (cdr form)))))))))
   (foo 0 (cadr l))))

;
; --- original version of quasiquote macro ---
;
;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;
;
;(macro
; quasiquote
; (lambda (l)
;   (define (mcons f l r)
;     (if (and (pair? r)
;              (eq? (car r) 'quote)
;              (eq? (cadr r) (cdr f))
;              (pair? l)
;              (eq? (car l) 'quote)
;              (eq? (cadr l) (car f)))
;         (list 'quote f)
;         (list 'cons l r)))
;   (define (mappend f l r)
;     (if (or (null? (cdr f))
;             (and (pair? r)
;                  (eq? (car r) 'quote)
;                  (eq? (cadr r) '())))
;        l
;         (list 'append l r)))
;   (define (mvec form v v1)
;     (if (and (pair? v1) (eq? (car v1) 'quote) (eq? (cadr v1) v))
;         form
;         (list 'list->vector v1)))
;   (define (foo level form)
;     (cond ((vector? form)
;            (let ((v (vector->list form)))
;              (mvec form v (foo level v))))
;           ((atom? form) (list 'quote form))
;           ((eq? 'quasiquote (car form))
;            (mcons form ''quasiquote (foo (add1 level) (cdr form))))
;           (#t (if (zero? level)
;                   (cond ((eq? (car form) 'unquote) (cadr form))
;                         ((eq? (car form) 'unquote-splicing)
;                          (error "Unquote-splicing wasn't in a list:"
;                                 form))
;                         ((and (pair? (car form))
;                               (eq? (caar form) 'unquote-splicing))
;                          (mappend form (cadar form) (foo level (cdr
;                                                                 form))))
;                         (#t (mcons form (foo level (car form))
;                                         (foo level (cdr form)))))
;                   (cond ((eq? (car form) 'unquote)
;                          (mcons form ''unquote (foo (sub1 level)
;                                                     (cdr form))))
;                         ((eq? (car form) 'unquote-splicing)
;                          (mcons form ''unquote-splicing
;                                      (foo (sub1 level) (cdr form))))
;                         (#t (mcons form (foo level (car form))
;                                         (foo level (cdr form)))))))))
;   (foo 0 (cadr l))))
;

;;
;; -- delay --
;;
;; (delay <exp>)
;; = (make-promise (lambda () <exp>))
;;
(macro delay
  (lambda (l)
    `(let ((make-promise
            (lambda (proc)
              (let ((already-run? #f) (result #f))
                (set-promise
                  (lambda ()
                    (cond ((not already-run?)
                           (set! result (proc))
                           (set! already-run? #t)))
                    result))))))
       (make-promise (lambda () ,@(cdr l))))))

;;;    rec is used to define self-recursive procedure locally
;;;    Note rec returns procedure, not a form.
(macro rec (lambda (rec-macro)
  (apply (lambda (proc body)
    `(letrec ((,proc ,body)) ,proc))
  (cdr rec-macro))))

;;; while pred is true, evaluate the expressions in body and return the
;;; result of the last expression evaluated (or #f if none were evaluated)
(macro while (lambda (while-macro)
  (apply 
    (lambda (pred . body)
      (let ((while-loop (gensym))
	    (while-res (gensym)))
        `(letrec ((,while-loop
	           (lambda (,while-res)
		     (if ,pred (,while-loop (begin ,@body)) ,while-res))))
           (,while-loop #f))))
    (cdr while-macro))))

(macro when
       (lambda (args)
         `(if ,(cadr args)
              (begin ,@(cddr args))
              #f)))

(macro unless
       (lambda (args)
         `(if ,(cadr args)
              #t
              (begin ,@(cddr args)))))

