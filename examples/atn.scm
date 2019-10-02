
;;; atn.scm

;;;;    for SECDR Scheme
;;;     Date revised    11-August-1992 by Shoichi Hayashi

(macro exif
  (lambda (l)
    (let ((condition (cadr l))(body (cddr l)))
      `(if (and ,condition (not (null? ,condition))) ,@body))))

; From nttdpe!nttlab!icot32!kddlab!uunet!uunet!math.fu-berlin.de!mailgzrz!
; w203zrz!mbut0135 Fri Jul 12 17:20:58 JST 1991
; Article: 1915 of comp.lang.scheme
; Path: nttsgw!nttdpe!nttlab!icot32!kddlab!uunet!uunet!math.fu-berlin.de!
; mailgzrz!w203zrz!mbut0135
; From: mbut0135@w203zrz.zrz.tu-berlin.de (Matthias Butt)
; Newsgroups: comp.lang.scheme
; Subject: Fun with continuations
; Keywords: Continuations, ATN, Lexical vs. Dynamic Scope
; Message-ID: <807@mailgzrz.tu-berlin.de>
; Date: 2 Jul 91 12:09:03 GMT
; Sender: news@mailgzrz.tu-berlin.de
; Organization: TU-Berlin
; Lines: 349
; Nntp-Posting-Host: w203zrz.zrz.tu-berlin.de


; Some time ago I remember having read a brief notice about the possibility
; to implement an ATN parser with some help of continuations. When we had to
; implement an ATN for teaching purposes we were looking into the literature
; and were horrified by the amount of housekeeping that is required by some
; ATN implementations (e.g. the one in Gazdar/Mellish: Natural Language
; Processing in LISP). By housekeeping I mean keeping some kind of agenda
; with al large number of ATN states and register values on it.

; I then remembered the continuation approach and tried to implement something
; along these lines. The basic Idea is, that the usual exit (via the last
; statement in the lambda body) is only taken if the parse is a failure
; otherwise we return a continuation together with parse val;ue and (possibly)
; rest tape via a non-local exit. If the parse was successful but we want to
; give it a second try we just call the continuation returned.

; My first question is whether this is considered to be an acceptable style
; of programming. On the one hand I find the solution rather elegant and
; in particular very short compared to what I find in the literature. On the
; other hand some people might get a little dizzy if they try to follow the
; actual course of computation. It might seem like the good old (or hated)
; code jumping that used to be done with GOTO in BASIC.

;A second question concerns the practical use of the ATN-parser as implemented.
; It is not quite easy to understand, how it can be properly called from a
; larger program, in particular if the continuation returned will be invoked
; later. Apparently the continuation does not carry a lexical environment  
; (whose bindings might be changed before the continuation is carried out) but
; a dynamic(or fluid) environment whose bindings will stay the same as when
; the continuation was generated. This at least is the case in PC-Scheme,
; therefore the functions FIND-ALL-PATHS and FIND-ALL-COMPLETE-PATHS at the
; end of the following code-listing only work with ls being a fluid variable.
; In my old version of MacScheme, however, it worked with normal LET (no
; fluid variables are available). I find the latter much more convenient
; and somehow easier to understand. Is tehre any way to simulate this
; behaviour in PC-SCHEME (which I comnsider being correct about the type
; of environment carried by a continuation)?

; Any comments and suggestions are appreciated!

; --- rather lengthy code listing - sorry  

; --- cut here ---


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;ATN-parser;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;_1991 by Matthias Butt & Klaus Fenchel

;--------------------- accessors and predicates -------------------------------
;a) network components

(define registers
  (lambda (network)
    (cadr (assoc 'registers network))))

(define initial-nodes 
  (lambda (network)
 	 (list-ref (assoc 'Initial network) 1)))

(define initial-test
  (lambda (network)
    (list-ref (assoc 'Initial network) 2)))

(define initial-actions
  (lambda (network)
    (list-tail (assoc 'Initial network) 3)))

(define final-nodes 
  (lambda (network)
  	(list-ref (assoc 'Final network) 1)))

(define final-test 
  (lambda (network)
  	(list-ref (assoc 'Final network) 2)))

(define final-actions 
  (lambda (network)
  	(list-tail (assoc 'Final network) 3)))

(define transitions 
  (lambda (network)
  	(cdddr network)))


;b) transition components

(define from
  (lambda (transition)
    (list-ref transition 1)))

(define to
  (lambda (transition)
    (list-ref transition 3)))

(define by
  (lambda (transition)
    (list-ref transition 5)))

(define test
  (lambda (transition)
    (list-ref transition 6)))

(define actions
  (lambda (transition)
    (list-tail transition 7)))

(define push?
  (lambda (edge)
    (eq? 'push (car edge))))


;----------------- evaluators for test and action forms -----------------------

(define exec/return-value
  ;Takes a list of forms, a set of bindings and an optional value for the 
  ;star variable. The bindings are passed as a list of two lists, the first
  ;list contains some symbol names and the second (wich must be of the same
  ;length) containsa value for each symbol, example: ((a b c)(1 2 3)). The
  ;forms are evaluated in an environment where the symbol names are bound to 
  ;their respective values, i.e. a = 1; b = 2; c = 3 in the above example and  
  ;the symbol star is boundto the value which is passed as the third argument
  ;(if any). The value of the last form is returned as the result.
  (lambda (forms bindings . star)
    (exif forms
        (apply                             ;Apply 
         (eval                             ;a
          `(lambda                         ;function whose
            ,(exif star                    ;lambda list contains
                 (cons 'star               ;'star (possibly)
                       (car bindings))  
                 (car bindings))           ;and the supplied symbol names
            ,@forms))                      ;and whose body contains the forms
         (append star (cadr bindings)))))) ;to the supplied values.

(define exec/return-bindings
  ;This function works just like EXEC/RETURN-VALUE except that it returns
  ;the bindings (possibly changed by side effects of the supplied forms)
  ;instead of the value of the last form.
  (lambda (forms bindings . star)
    (exif forms
        (apply
         (eval
          `(lambda
             ,(exif star 
                  (cons 'star
                        (car bindings))  
                  (car bindings))
             ,@forms
             (list (quote ,(car bindings))    ;Return a list of list of symbols
                   (list ,@(car bindings))))) ;and list of new values.
         (append star (cadr bindings)))
        bindings)))                           ;no forms -> return bindings


;------------------------------- main routine ---------------------------------

(define atn-parse
  (lambda (network tape . hold)
    (let ((bindings                                   ;Initialize bindings
           `((hold ,@(registers network))             ;with value passed by
             (,hold ,@(map (lambda (x)())             ;hold and nil for all
                           (registers network))))))   ;the other variables.
      (exif (exec/return-value                        ;If initial test is
           (list (initial-test network))              ;successful
           bindings)
          (begin
           (set! bindings                             ;do initial actions
                 (exec/return-bindings                ;(possibly changing
                  (initial-actions network)           ;current bindings
                  bindings))
           (call-with-current-continuation            ;establish a catcher
            (lambda (catcher)                         ;for exit on success
              (for-each                               ;and call 
               (lambda (node)                         ;recognize-next
                 (recognize-next node
                                 network
                                 tape
                                 bindings
                                 catcher))
               (initial-nodes network))               ;on each initial node.
             ())))))))


  (define recognize-next
  (lambda (node network tape bindings catcher)
    (if (member node (final-nodes network))    ;If we have reached a final node
        (exif (exec/return-value               ;and if the final test
             (list (final-test network))       ;evaluates to true
             bindings)                         ;with current bindings
            (call-with-current-continuation    ;then make new continuation
             (lambda (cont)                    ;and
               (catcher                        ;return the last catcher (pop)
                (list (exec/return-value       ;with a list of
                       (final-actions network) ;result of final-actions 
                       bindings)
                      tape                     ;rest tape and the
                      cont))))))               ;newly established continuation.
    
     (for-each                                 ;For each transition,
      (lambda (transition)                     ;if this transition
        (if (equal? (from transition) node)    ;starts from current node:
            (let ((edge (by transition)))
              (cond 
               ((eq? '|#| edge)               ;If it is a jump
                (exif (exec/return-value      ;and if its test evaluates to
                     (list (test transition)) ;something else than nil
                      bindings)               ;with current bindings
                    (recognize-next           ;then recognize next
                     (to transition)          ;start at end of this transition
                     network                  ;(same network)
                     tape                     ;(same tape)
                     (exec/return-bindings    ;bindings may be changed by
                      (actions transition)    ;evaluating the final
                      bindings)               ;actions of this transition
                     catcher)))               ;(same catcher).
               
               ((and tape (atom? edge))       ;If Label is atom and tape left
                (let ((star (car tape)))      ;set star to next element.
                  (if (equal? edge star)      ;Is it equal to the label?
                      (exif (exec/return-value  ;Then if test is true
                           (list (test transition)) 
                           bindings 
                           star)
                          (recognize-next     ;recognize next
                           (to transition)
                           network
                           (cdr tape)         ;with rest of tape.
                           (exec/return-bindings 
                            (actions transition) 
                            bindings
                            star)
                           catcher)))))
               
               ((and tape (push? edge))         ;If it is a push to
                (let ((result                   ;another network:
                       (exif (cddr edge)        ;Any value to pass?
                           (atn-parse           ;Call atn-parse with
                            (eval (cadr edge))  ;the network and
                            tape                ;the current tape.
                            (exec/return-value  ;(The value returned by this
                             (cddr edge)        ;is bound to the hold variable
                             bindings))         ;within the new network.)
                           (atn-parse           ;No value to pass - same as
                            (eval (cadr edge))  ;above but with only two
                            tape))))            ;parameters supplied.
                  (exif result                  ;If the network has found a way
                      (let ((star               ;set star
                             (car result)))     ;to the actual result
                        (exif (exec/return-value      ;then if the test 
                             (list (test transition)) ;of this transition
                             bindings                 ;results in something
                             star)                    ;else but nil
                            (recognize-next           ;recognize next with
                             (to transition)
                             network
                             (cadr result)            ;rest-tape returned by
                             (exec/return-bindings    ;the subnetwork
                              (actions transition) 
                              bindings
                              star)
                             catcher))
                        ((caddr result)         ;or call the continuation
                         ())))))))))            ;returned by the subnetwork.
      (transitions network))))
  
  

;------------------------- some useful functions ------------------------------
  
; (define find-all-paths
;   (lambda (network tape)
;     (fluid-let ((ls ()))
;       (let ((result (atn-parse network tape)))
;         (if result
;             (begin
;              (set! (fluid ls) (cons result (fluid ls)))
;              ((caddr result)()))
;             (fluid ls))))))

(define find-all-paths
  (lambda (network tape)
    (let ((ls ()))
      (let ((result (atn-parse network tape)))
        (if (not (null? result))
            (begin
             (set! ls (cons result ls))
             ((caddr result)()))
            ls)))))

; (define find-all-complete-paths
;   (lambda (network tape)
;     (fluid-let ((ls ()))
;       (let ((result (atn-parse network tape)))
;         (if result
;             (begin
; 	      (if (null? (cadr result))
; 		  (set! (fluid ls) 
; 			(cons (car result) 
; 			      (fluid ls)))
; 		  ((caddr result)()))
; 	      (fluid ls)))))))
  
(define find-all-complete-paths
  (lambda (network tape)
    (let ((ls ()))
      (let ((result (atn-parse network tape)))
        (if (not (null? result))
            (begin
	      (if (null? (cadr result))
		  (set! ls (cons (car result) ls))
		  ((caddr result)()))
	      ls))))))
  
;------------------------ a few simple test networks --------------------------

(define anbncn
  ;Recognizes any sequence a^n b^n c^n e.g. (a b c) (a a b b c c).
  ;The return value is n.
  '((Registers (n m))
    (Initial (0)		#t      (set! n 0))
    (Final (2)				(= n m)  n)
    (From 0 to 0 by a   #t      (set! n (+ 1 n)))
    (From 0 to 1 by b   #t      (set! m 1))    
    (From 1 to 1 by b			(> n m) (set! m (1+ m)))
    (From 1 to 2 by c   (= n m) (set! m 1))
    (From 2 to 2 by c			(> n m) (set! m (1+ m)))))

;------------------------------------------------------------------------------

(define n1
  ;Recognizes the sequence (a b c) and nothing else.
  ;Return value is #t.
    '((registers ())
      (initial (0) #t)
      (final (3)   #t #t)
      (from 0 to 1 by a #t)
      (from 1 to 2 by b #t)
      (from 2 to 3 by c #t)))
  
(define n2
  ;Recognizes ([a b c]^n); e.g. (a b c) (a b c a b c a b c).
  ;Uses n1. Return-value is n.
  '((registers (n))
    (initial (0) #t (set! n 0))
    (final (1)   #t n)
    (from 0 to 1 by (push n1) #t (set! n (1+ n)))
    (from 1 to 0 by |#|       #t)))

;------------------------------------------------------------------------------

(define n3
  '((registers (mid))
    (initial (0) #t (exif hold (set! mid (car hold))))
    (final (3) #t mid)
    (from 0 to 1 by a #t)
    (from 1 to 2 by a (or (not hold)(eq? mid star))(set! mid star))
    (from 1 to 2 by b (or (not hold)(eq? mid star))(set! mid star))
    (from 1 to 2 by c (or (not hold)(eq? mid star))(set! mid star))
    (from 2 to 3 by c #t)))

(define n4
  '((registers (n mid))
    (initial (0) #t (set! n 0))
    (final (1)   #t n)
    (from 0 to 1 by (push n3)     #t (set! n (1+ n))(set! mid star))
    (from 1 to 1 by (push n3 mid) #t (set! n (1+ n)))))

