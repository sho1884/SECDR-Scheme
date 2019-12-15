;;
;; compiler.scm
;;

;;;; SECDR Compiler
;;
;;   Date written    11-Feb-1990 by Atsushi Moriwaki
;;   Date revised    20-Feb-1990 by Atsushi Moriwaki
;;   Date revised    06-Mar-1990 by Atsushi Moriwaki
;;   Date revised    31-Jul-1992 by Shoichi Hayashi
;;   Date revised    20-Nov-1992 by Shoichi Hayashi
;;   Date revised    13-Mar-1993 by Shoichi Hayashi
;;   Date revised    07-May-1993 by Shoichi Hayashi
;;   Date revised    11-May-1993 by Shoichi Hayashi
;;
;;========================================================================
;;
;;   Copyright(C) 1990,1991,1992,1993 Atsushi Moriwaki, Shoichi Hayashi.
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;   The author can be reached at s-haya@rst.fujixerox.co.jp or
;;   Shoichi Hayashi,
;;   SYSTEMS & COMMUNICATIONS LAB.  FUJI XEROX Co.,LTD.
;;   134 GODOCHO, HODOGAYA-KU, YOKOHAMA-SHI, KANAGAWA, 240, JAPAN
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/compiler.scm,v 0.2 1994/09/14 02:55:46 s-haya Exp s-haya $
;;

(define (-*-compile-*- exp . mode)
;;
;;; Definition of operator numbers
;;
  (define %LD% '#0)
  (define %TLD% '#1)
  (define %GLD% '#2)
  (define %LDC% '#3)
  (define %LDF% '#4)
  (define %AP% '#5)
  (define %TAP% '#6)
  (define %PUSH% '#7)
  (define %RTN% '#8)
  (define %SEL% '#9)
  (define %TSEL% '#10)
  (define %ASSIG% '#11)
  (define %TASSIG% '#12)
  (define %GASSIG% '#13)
  (define %DEF% '#14)
  (define %PUSHCONS% '#15)
  (define %SAVE% '#16)
  (define %EXEC% '#17)
  (define %STOP% '#18)
;;
;; --- List of machine transitions ---
;;
;; S E (LD (i . j) . C) D R ==> S E C D r
;;  where r = (list-ref (list-ref E i) j)
;;
;; S E (TLD (i . j) . C) D R ==> S E C D r
;;  where r = (list-tail (list-ref E i) j)
;;
;; S E (GLD sym . C) D R ==> S E C D r
;;  where r = gloabl value of sym
;;
;; S E (LDC const . C) D R ==> S E C D const
;;
;; S E (LDF code . C) D R ==> S E C D (closure of code and E)
;;
;; (args . S) E (AP . C) D op ==>
;;  case: op is closure
;;    () (args . env) code (S E C . D) NIL
;;    where code is closure code of op and
;;    env is closure environment of op.
;;  case: op is primivite procedure
;;    First, execute primitive procedure op with arguments args
;;    and set registers to S E C D r, where r is the return
;;    value of primitine procedure op.
;;  case: op is continuation
;;    s e c d r
;;    where r is first element of args and
;;    s e c d are saved registers in op.
;;
;; (args . S) E (TAP . C) D op ==>
;;  case: op is closure
;;    () (args . env) code D NIL
;;    where code is closure code of op and
;;    env is closure environment of op.
;;  case: op is primivite procedure
;;    First, execute primitive procedure op with arguments args
;;    and set registers to S E C D r, where r is the return
;;    value of primitine procedure op.
;;  case: op is continuation
;;    s e c d r
;;    where x is the first element of args and
;;    s e c d r are saved registers in op.
;;
;; S E (PUSH . C) D R ==> (R . S) E C D R
;;
;; S E (RTN . C) (s e c . d) R ==> s e c d R
;;
;; S E (SEL ct cf . C) D test ==> S E cx (S E C . D) test
;;  where cx = (if test ct cf)
;;
;; S E (TSEL ct cf . C) D test ==> S E cx D test
;;  where cx = (if test ct cf)
;;
;; S E (ASSIG (i . j) . C) D R ==> S E' C D R
;;  where E' is made by
;;    (set-car! (list-tail (list-ref E i) j) R)
;;
;; S E (TASSIG (i . j) . C) D R ==> S E' C D R
;;  where E' is made by
;;    (if (zero? j)
;;      (set-car! (list-tail E i) R)
;;      (set-cdr! (list-tail (list-ref E i) (- j 1)) R))
;;
;; S E (GASSIG sym . C) D R ==> S E C D R
;;  where global value of sym = R
;;
;; S E (DEF sym . C) D R ==> S E C D sym
;;  where global value of sym = R
;;
;; (s . S) E (PUSHCONS . C) D R ==> ((R . s) . S) E C D R
;;
;; S E (SAVE C1 . C2) D R ==> S E C1 (S E C2 . D) R
;;
;; S E (EXEC . C) D code ==> NIL NIL code (S E C . D) NIL
;;
;; S E (STOP . C) D R ==> Stop SECDR execution
;;

;;
;;; comp assumes 5-syntaxes quote, lambda, if, define(top-level), set!
;;
  (define (comp exp vars cont tail)
;;
;;; Local procedures for comp
;;

;;  compile the body of lambda
    (define (comp-body e v c)
     (if (pair? e)
       (if (null? (cdr e))
         (comp (car e) v c #t)
         (comp (car e) v (comp-body (cdr e) v c) #f))
       (comp e v c #t)))

;;  compile arguments
    (define (comp-args e v c)
      (if (pair? e)
        (comp-args (cdr e) v (comp (car e) v (cons %PUSHCONS% c) #f))
        (comp e v (cons %PUSH% c) #f)))

;;  search a position of symbol in frame
    (define (frame-loc f s)
      (define (iter-list-loc i j l s)
        (if (pair? l)
          (if (eq? (car l) s)
              (cons (cons i j) #f)
              (iter-list-loc i (1+ j) (cdr l) s))
          (if (eq? l s) (cons (cons i j) #t) #f)))
      (define (iter-frame-loc i f s)
        (if (pair? f)
          (let ((loc (iter-list-loc i 0 (car f) s)))
            (if loc loc (iter-frame-loc (1+ i) (cdr f) s)))
          #f))
      (iter-frame-loc 0 f s))

;;
;;; Begining of main part of procedure 'comp'
;;
    (if (pair? exp)
      (cond
        ((eq? (car exp) 'quote)
          (cons %LDC% (cons (cadr exp) cont)))
        ((eq? (car exp) 'lambda)
          (cons %LDF%
                (cons
                  (comp-body
                    (cddr exp)
                    (cons (cadr exp) vars)
                    (cons %RTN% '()))
                  cont)))
        ((eq? (car exp) 'begin)
          (cons %LDF%
                (cons
                  (comp-body
                    (cdr exp)
                    (cons '() vars)
                    (cons %RTN% '()))
                  cont)))
        ((eq? (car exp) 'if)
          (let
            ((thenpt
               (comp (caddr exp) vars (cons %RTN% '()) #t))
             (elsept
               (comp (car (cdddr exp)) vars (cons %RTN% '()) #t)))
            (comp (cadr exp)
                  vars
                  (cons (if tail %TSEL% %SEL%)
                        (cons thenpt (cons elsept cont)))
                  #f)))
        ((eq? (car exp) 'define)
          (let ((loc (frame-loc vars (cadr exp))))
            (comp
              (caddr exp)
              vars
             (if loc
               (if (cdr loc)
                 (cons %TASSIG% (cons (car loc) cont))
                 (cons %ASSIG%  (cons (car loc) cont)))
               (cons %DEF% (cons (cadr exp) cont)))
             #f)))
        ((eq? (car exp) 'global-define)
          (comp
            (caddr exp)
            vars
            (cons %DEF% (cons (cadr exp) cont))
            #f))
        ((eq? (car exp) 'set!)
          (let ((loc (frame-loc vars (cadr exp))))
            (comp
              (caddr exp)
              vars
             (if loc
               (if (cdr loc)
                 (cons %TASSIG% (cons (car loc) cont))
                 (cons %ASSIG%  (cons (car loc) cont)))
               (cons %GASSIG% (cons (cadr exp) cont)))
             #f)))
        (#t (comp-args (cdr exp)
                       vars
                       (comp (car exp)
                             vars
                             (cons (if tail %TAP% %AP%) cont)
                             #f))))
      ;; else (pair? exp)
      (if (symbol? exp)
        (let ((loc (frame-loc vars exp)))
          (if loc
            (if (cdr loc)
              (cons %TLD% (cons (car loc) cont))
              (cons %LD% (cons (car loc) cont)))
            (cons %GLD% (cons exp cont))))
        (cons %LDC% (cons exp cont)))))
;;
;;; End of procedure 'comp'
;;

;;
;;; Macro expansion
;;
  (define (macro-expand exp)
    (define (tail-map proc lis)
      (if (pair? lis)
        (cons (proc (car lis)) (tail-map proc (cdr lis)))
        (proc lis)))
    (define (check-variables l)
      (let ((ch-var (lambda (s)
              (if (symbol? s)
                (if (or (get s '%special-form%)
                        (get s '%macro-special-form%))
                  (error "Unable to use specail-form form as varibale: " s)
                  #t)
              (error "Expect symbol: " s)))))
        (cond
          ((pair? l) (ch-var (car l)) (check-variables (cdr l)))
          ((null? l) #t)
          (#t (ch-var l)))))

    (if (pair? exp)
      (cond
;; lambda
        ((eq? (car exp) 'lambda)
          (let ((vars (cadr exp))
                (body (tail-map macro-expand (cddr exp))))
            (define (get-def-vars b)
              (if (pair? b)
                (if (pair? (car b))
		    (cond ((eq? (caar b) 'define)
			   (cons (cadar b) (get-def-vars (cdr b))))
			  ((eq? (caar b) 'if)
			   (append (get-def-vars (cdar b))
				   (get-def-vars (cdr b))))
			  ((and (pair? (caar b))(eq? (caaar b) 'begin))
			   (append (get-def-vars (cdaar b))
				   (get-def-vars (cdr b))))
			  (else (get-def-vars (cdr b))))
                    (get-def-vars (cdr b)))
                '()))
            (check-variables vars)
            (let ((def-vars (get-def-vars body)))
              (if (null? def-vars)
                 ;;
                 ;;; case of no internal defintion
                 ;;
                `(lambda ,vars ,@body)
                 ;;
                 ;;; case of internal definition
                 ;;
                 ;; (lambda (<var1> ...)
                 ;;   (define <def-var1> <def-body1>) ...
                 ;;   <body>)
                 ;; ==>
                 ;; (lambda (<var1> ...)
                 ;;   ((lambda (<def-var1> ...)
                 ;;      (define <def-var1> <def-body1>) ...
                 ;;      <body>)
                 ;;    '() ...))
                 ;;
                `(lambda ,vars
                   ((lambda ,def-vars ,@body)
                    ,@(map (lambda (a) ''()) def-vars)))))))
;; begin
        ((eq? (car exp) 'begin)
         `((begin
             ,@(tail-map macro-expand (cdr exp)))))
;; quote
        ((eq? (car exp) 'quote) exp)
;; define
        ((eq? (car exp) 'define)
          (let ((var
                  (if (pair? (cadr exp))
                    (caadr exp)
                    (cadr exp)))
                (body
                  (if (pair? (cadr exp))
                    `(lambda ,(cdadr exp) ,@(cddr exp))
                    (caddr exp))))
            (check-variables var)
            `(define ,var ,(macro-expand body))))
;; if
        ((eq? (car exp) 'if)
          `(if ,@(tail-map macro-expand
                   (if (null? (cdddr exp))
                     `(,@(cdr exp) #f)
                     (cdr exp)))))
;; set!
        ((eq? (car exp) 'set!)
          (check-variables (cadr exp))
          `(set! ,(cadr exp) ,(macro-expand (caddr exp))))
;; expansion of macro-special-form
        (#t
          (if (symbol? (car exp))
            (let ((macro-proc (get (car exp) '%macro-special-form%)))
              (if macro-proc
                (macro-expand (macro-proc exp))
                (cons (car exp) (tail-map macro-expand (cdr exp)))))
            (tail-map macro-expand exp))))
      (begin (if (symbol? exp) (check-variables exp)) exp)))
;;
;;; End of macro-expand
;;

  (put 'lambda '%special-form% #t)
  (put 'begin '%special-form% #t)
  (put 'quote '%special-form% #t)
  (put 'define '%special-form% #t)
  (put 'global-define '%special-form% #t)
  (put 'if '%special-form% #t)
  (put 'set! '%special-form% #t)

  (if (not (null? mode))
    (macro-expand exp)
    (comp (macro-expand exp) '() (cons %RTN% '()) #t)))
;;
;;; End of -*-compile-*-
;;

(define (macro-expand exp)
  (compile exp #t))
