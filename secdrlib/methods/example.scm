; EXAMPLES (cut here to end of file to throw examples away)
;
; Our first example class, or object type, is ``parent'', represented
; by the following constructor procedure.

(define (new-parent arg)
  (let ((state-var (* arg arg))
        (supers ()))
    
    (define (report-state-var)
      (display state-var)
      (newline)
      state-var)
    
    (define (update-state-var arg)
      (set! state-var (* arg arg)))
    
    (define (echo arg)
      (display arg) (newline))
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)  report-state-var  )
       (  (eq? msg 'update)  update-state-var  )
       (  (eq? msg 'echo)    echo  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Parent" msg)  )))
    
    self))

; This class, or constructor procedure, completely illustrates, by
; example, the programming convention of the ``methods'' technique.  
; The constructor takes a single argument, whose square it stores in a
; local state variable.  Another state variable, the list of
; supertypes, is set to nil, since this class is at the root of an
; inheritance hierarchy.  Three methods are defined, one that reports
; and returns the current value of the state variable, one that sets
; the state variable equal to a new square, and one that merely echoes
; its argument.  A method dispatching procedure, conventionally named
; ``self'', tests a given message against three symbols and returns
; the corresponding method if a match is found.  If no match is found,
; the list of supertypes is searched for a match.  In the case of this
; class, ``parent'', the search is purely formal, to illustrate how it
; should be done, since ``parent'' has no ancestors.  However, if a
; match were found among the list of supertypes, the method would be
; returned.  Note how the search relys on the fact that any non-nil
; result is treated as a successful ``cond'' clause, terminating the
; ``cond'' statement. Search-supertypes returns nil only when a match
; is not found.  Finally, if no match is found locally or among the
; supertypes, an appropriate error-method is pseudo-created and
; returned.
; 
; We now test this class by making an instance and passing it
; messages.

(define p (new-parent 42))

((p 'report))

((p 'update) 69)

((p 'report))

((p 'echo) (list 1 2 3))

; We test error handling:

((p 'bogus))

(set-debug-method-mode)

((p 'bogus))

(reset-debug-method-mode)

((p 'bogus) 'here 'are 'some 'junk 'arguments)

; Continuing this example, let us define a child class inheriting
; all attributes and methods of the parent.  Note the attributes of
; the parent are only accessible through the parent's method
; discipline.  This is a strict form of inheritance, and the default
; in C++, for example.  (C++ allows the programmer to override 
; ancestors' access discipline, at his own peril.)

(define (new-child arg1 arg2)
  
  (let* (  (leg1 (* arg1 arg1))
           (leg2 (* arg2 arg2))
           (hypotenuse (+ leg1 leg2))
           (supers (list
                    (new-parent hypotenuse)))  )
    
    (define (report)
      (for-all-parents supers 'report)
      (display "Leg1 = ") (display (sqrt leg1)) (newline)
      (display "Leg2 = ") (display (sqrt leg2)) (newline)
      (display "Hypo = ") (display (sqrt hypotenuse)) (newline))
    
    (define (update-leg1 val)
      (set! leg1 (* val val))
      (set! hypotenuse (+ leg1 leg2)))
    
    (define (update-leg2 val)
      (set! leg2 (* val val))
      (set! hypotenuse (+ leg1 leg2)))
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)       report       )
       (  (eq? msg 'update-leg1)  update-leg1  )
       (  (eq? msg 'update-leg2)  update-leg2  )
       (  (search-supertypes supers msg)       )
       (  else  (make-error-method "Child" msg)       )))
    
    self))

; We now test the child type.

(define c (new-child 3 4))

((c 'report))  ;;; passes message to all parents

((c 'update-leg1) 5)

((c 'update-leg2) 12)

((c 'report))

((c 'echo) '(foo bar))   ;;; msg known only in the parent

((c 'bogus) 'baz 'rat)

(set-debug-method-mode)

((c 'bogus) 'baz 'rat)

(reset-debug-method-mode)

((c 'bogus) 'baz 'rat)

; The last example, presented without detailed narrative, shows a 
; slightly deeper inheritance hierarchy.  The leaf is a type named
; ``daughter''.  Its two parent classes are ``mother'' and ``father''.  
; In turn, every mother has an ``estate'' and a ``religion'' (please
; excuse the somewhat strained metaphor of inheritance; this is just
; a little example).  

(define (new-estate value)
  (let ((value value)
        (supers ()))
    
    (define (report)
      (display "Estate = $") (display value) (newline))
    
    (define (what-value) value)
    
    (define (increase amount) (set! value (+ value amount)))
    
    (define (decrease amount) (set! value (- value amount)))
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)       report  )
       (  (eq? msg 'what-estate)  what-value  )
       (  (eq? msg 'increase)     increase  )
       (  (eq? msg 'decrease)     decrease  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Estate" msg)  )))
    
    self))

(define (new-religion theReligion)
  (let ((religion theReligion)
        (supers ()))
    
    (define (report) (display "Religion = ") (display religion) (newline))
    
    (define (what-religion) religion)
    
    (define (convert theNewReligion) (set! religion theNewReligion))
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)         report  )
       (  (eq? msg 'convert)        convert  )
       (  (eq? msg 'what-religion)  what-religion  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Religion" msg)  )))
    
    self))

(define (new-father eye-color)
  (let ((eye-color eye-color)
        (supers ()))
    
    (define (report) (display "Father's eye color = ")
      (display eye-color) (newline))
    
    (define (what-eye-color) eye-color)
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)          report  )
       (  (eq? msg 'what-eye-color)  what-eye-color  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Father" msg)  )))
    
    self))

(define (new-mother eye-color estate religion)
  (let ((eye-color eye-color)
        (supers (list
                 (new-estate estate)
                 (new-religion religion))))
    
    (define (report)
      (for-all-parents supers 'report)
      (display "Mother's eye color = ")
      (display eye-color) (newline))
    
    (define (what-eye-color) eye-color)
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)          report  )
       (  (eq? msg 'what-eye-color)  what-eye-color  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Mother" msg)  )))
    
    self))

(define (new-daughter eye-color)
  (let* ((eye-color eye-color)
         (parents-eye-color
          (if (eq? eye-color 'blue)  'blue  'brown))
         (supers (list
                  (new-father parents-eye-color)
                  (new-mother parents-eye-color 500000 'Jewish))))
    
    (define (report)
      (for-all-parents supers 'report)
      (display "Daughter's eye color = ")
      (display eye-color)
      (newline))
    
    (define (what-eye-color) eye-color)
    
    (define (self msg)
      (cond
       (  (eq? msg 'report)          report  )
       (  (eq? msg 'what-eye-color)  what-eye-color  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Daughter" msg)  )))
    
    self))

(define dbl (new-daughter 'blue))

((dbl 'report))

((dbl 'convert) 'muslim)

((dbl 'report))

((dbl 'increase) 50000)

((dbl 'report))

(define dbr (new-daughter 'brown))

((dbr 'report))

((dbr 'decrease) 250000)

((dbr 'report))

((dbr 'bogus))

(set-debug-method-mode)

((dbr 'bogus))

(reset-debug-method-mode)


; From brian@topaz.jpl.nasa.gov Mon Jul 10 14:27:56 1989
; From: brian@topaz.jpl.nasa.gov (Brian of ASTD-CP)
; Newsgroups: comp.lang.scheme
; Subject: OOP in Scheme (serious example)
; Date: 5 Jul 89 21:47:32 GMT
; Reply-To: brian@topaz.Jpl.Nasa.Gov (Brian of ASTD-CP)
; Organization: Jet Propulsion Laboratory, Pasadena, CA

; This'll be my last submission on this topic, so I promise I won't
; be burning up the wires with any more.  I thought a serious 
; example would be of some interest, however, so here is a FIFO
; queue data type.  I'll be building classes for priority queues,
; heaps, splay trees, and assorted others, as well as a data flow
; executive.  Anyone interested further in this topic may feel
; free to e-mail me.  Again, sorry for the length of these sub-
; missions.  BCB.

;================================================================
;| Brian Beckman                  | brian@topaz.jpl.nasa.gov    |
;| Mail Stop 510-202              | (818) 397-9207              |
;| Jet Propulsion Laboratory      |                             |
;| Pasadena, CA 91109             | 3 July 1989                 |
;================================================================

;;; Adapted from Abelson & Sussman, Ch. 3, Pg 208 ff.
;;; Uses the ``methods'' OOP package.  This is an expanded,
;;; industrial-strength solution to Exercise 3.22 of A & S.

(define (new-queue . initial-list)
  
  (let (  (q (cons () ()))
          (dummy (if (not (null? initial-list))
                     (set! initial-list (car initial-list))))
          (supers ())  )
    
    (define (head) (car q))
    (define (tail) (cdr q))
    (define (set-head! item) (set-car! q item))
    (define (set-tail! item) (set-cdr! q item))
    
    (define (empty-queue?) (null? (head)))
    
    (define (front)
      (if (send self 'empty?)
          (error "FRONT called on empty queue")
          (car (head))))
    
    (define (insert-queue! item)
      (let ((elt (cons item ())))  ; could be (list item)
        (cond
         (  (send self 'empty?)
            (set-head! elt)
            (set-tail! elt)  
            self  )
         (  else
            (set-cdr! (tail) elt)
            (set-tail! elt)  
            self  ))))
    
    (define (insert-list! lyst)
      (cond
       (  (null? lyst)  self  )
       (  else
          (send self 'insert! (car lyst))
          (insert-list! (cdr lyst))  )))
    
    (define (remove-queue!)
      (cond
       (  (send self 'empty?)
          (error "REMOVE called on empty queue")  )
       (  else
          (set-head! (cdr (head)))  self)))
    
    (define (clear-queue!)
      (set! q (cons () ()))
      self)
    
    (define (print) (display (head)) (newline))
    
    (define (self msg)
      (cond
       (  (eq? msg 'insert!)       insert-queue!  )
       (  (eq? msg 'empty?)        empty-queue?   )
       (  (eq? msg 'remove!)       remove-queue!  )
       (  (eq? msg 'clear!)        clear-queue!   )
       (  (eq? msg 'front)         front  )
       (  (eq? msg 'print)         print  )
       (  (eq? msg 'list)          (lambda () (head))  )
       (  (eq? msg 'insert-list!)  insert-list!  )
       (  (search-supertypes supers msg)  )
       (  else  (make-error-method "Queue" msg)  )))
    
    (insert-list! initial-list)  ;;; returns ``self''
    
    ))
    ;;; end of new-queue

; Test suite for queues.

(define q (new-queue '(a b c d e)))

(send q 'print)

(send q 'list)

(send (send q 'remove!) 'print)

(send q 'empty?)

(send (send q 'clear!) 'empty?)

(send q 'print)

(define q (new-queue))

(send q 'empty?)



