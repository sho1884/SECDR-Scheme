; OOP In Scheme
; From brian@jade.jpl.nasa.gov Mon Jul 10 14:26:34 1989
; From: brian@jade.jpl.nasa.gov (Brian of ASTD-CP)
; Newsgroups: comp.lang.scheme
; Subject: OOP in Scheme (addendum)
; Date: 3 Jul 89 08:16:05 GMT
; Reply-To: brian@jade.Jpl.Nasa.Gov (Brian of ASTD-CP)
; Organization: Jet Propulsion Laboratory, Pasadena, CA

; methods.scm  (ADDENDUM)
;
; ===============================================================
; Brian Beckman                  |    brian@topaz.jpl.nasa.gov
; Mail Stop 510-202              |    (818) 397-9207
; Jet Propulsion Laboratory      | 
; Pasadena, CA 91109             |    30 June 1989
; ===============================================================

; There are two ways to invoke a method in an object.  The
; first is to send the object a message, getting back a procedure.
; This procedure can then be invoked at will on an appropriate
; set of arguments.  Such an idiom usually results in expressions
; like the following:
;
;   ((foo 'do-it) arg1 arg2)
;
; This is fairly readable and a fine idiom, but it has its 
; limitations.  Suppose that this expression were to result in
; another object, to which we should like to send the message
; 'baz with the arguments 'rat and 'ter.  Then we should write
; the following:
;
;   ((((foo 'do-it) arg1 arg2) 'baz) 'rat 'ter)
;
; The number of leading parentheses is a problem.  It is easy 
; to devise nested message passing expressions that are
; much more difficult to write than to devise, merely because of
; the number of leading parentheses that must be presaged.  Lisp 
; already has a problem with closing parentheses; we don't want
; to compound the felony in this package by introducing a 
; corresponding problem with opening parentheses.  
;
; We need a ``send'' routine that does little more than reduce
; the need for leading parenthese.  This is, admittedly, merely 
; a syntactic issue.  Consider the following, which is the second
; way to send a message to an object:

(define (send object msg . args)
  (apply (object msg) args))

; The earlier example message passing expressions can now be
; much more easly written much more nicely, as follows:
;
;    (send object 'msg arg1 arg2)
;
; and
;
;    (send (send object 'msg arg1 arg2) 'baz 'rat 'ter)


; From brian@jade.jpl.nasa.gov Mon Jul 10 14:27:29 1989
; From: brian@jade.jpl.nasa.gov (Brian of ASTD-CP)
; Newsgroups: comp.lang.scheme
; Subject: OOP in Scheme
; Summary: ``methods'', a tiny scheme package humbly submitted
; Date: 2 Jul 89 07:31:07 GMT
; Reply-To: brian@jade.Jpl.Nasa.Gov (Brian of ASTD-CP)
; Organization: Jet Propulsion Laboratory, Pasadena, CA

; methods2.scm
;
; ===============================================================
; Brian Beckman                  |    brian@topaz.jpl.nasa.gov
; Mail Stop 510-202              |    (818) 397-9207
; Jet Propulsion Laboratory      | 
; Pasadena, CA 91109             |    30 June 1989
; ===============================================================

; INTRODUCTION
;
; This is a tiny object-oriented programming system with multiple
; inheritance and error handling.  It is modeled after the message
; passing modules in Chapter 3 of Abelson & Sussman.  It is 
; implemented in ``pure'' Scheme, without macros or syntax
; extensions.
; 
; This programming system is implemented as a technique, or
; programming convention, with some helper routines.  The programming
; convention is not enforced, as we choose to avoid syntax-extensions
; for portability's sake.  The technique is illustrated in this file
; with a few examples.  In example one, a parent class, named
; ``parent,'' passes its attributes to a child named ``child.''  In
; example two, two parents, ``mother'' ``fater'', pass their attributes
; to a child class, ``daughter.''  The reader will perceive the technique
; by generalization from these examples and will be able to apply it
; to his or her own problems.
; 
; Every class is represented by its constructor procedure.  This
; procedure returns a message dispatching procedure.  The message
; dispatching procedure should be named ``self'' so that an object can
; conveniently send messages to itself.  However, ``self'' is an
; internal name not known outside the constructor.  
; 
; In summary, classes are represented by constructor procedures, and
; objects, or instances of classes, are represented by message
; dispatching procedures.  The present version of ``methods'' does not
; support code sharing, so every instance of a class has its own
; private copies of the method code.  We expect to implement code
; sharing in a later version of ``methods''.
; 
; The message dispatching procedure walks the multiple inheritance
; hierarchy upwards until it finds an object that can understand a
; message, starting with itself.  If no object that can understand the
; message is found, a global error procedure is called.
; 
; IMPLEMENTATION
;
; Error processing is challenging.  We should like to have two modes.
; In ``normal mode'', an error is reported only by the first receiver
; of a message.  In ``debug mode'', an inheritance traceback should be
; given whereby every object in an inheritance hierarchy will report
; when it fails to recognize a given message.  The following variable
; represents that mode.  (For simplicity, this object is hidden only
; by its name, which is unusual enough that it is unlikely to be
; trammeled by an application.  This is not the recommended technique
; for data hiding.  Data hiding ought to be implemented through the
; techniques shown in this file!  However, since this error handling
; part of the methods package is considered system programming,
; certain liberties in style are justifiable.  There are in fact, good
; technical reasons for the error handling code to be implemented with
; global variables, which the perceptive reader will be able to
; deduce.)

(define **method-mode** 'normal-method-mode)

; The user can set these modes as follows.

(define (set-debug-method-mode)
  (set! **method-mode** 'debug-method-mode))

(define (set-normal-method-mode)
  (set! **method-mode** 'normal-method-mode))

(define (reset-debug-method-mode)  ;;; synonym
  (set! **method-mode** 'normal-method-mode))

; and test them with the following routine:

(define (test-debug-method-mode)
  (eq? **method-mode** 'debug-method-mode))

; Before presenting the examples of classes and objects, some helper
; routines are needed.
;
; When an object cannot recognize a message, and none of its ancestor
; objects can recognize it, the object creates an error procedure and
; returns it as the result of the message dispatcher.  

(define **method-error-class-name** "No class name.")

(define **method-error-message** 'no-message)

(define (error-method . junk-args)
  (display **method-error-class-name**)
  (display ": uknown message: '")
  (display **method-error-message**)
  (newline)
  #f)

(define (make-error-method class-name msg)
  (set! **method-error-class-name** class-name)
  (set! **method-error-message** msg)
  error-method)

; The procedure that walks the inheritance hierarchy must cooperate
; in the error handling.  

(define (search-supertypes supers msg)
  (define method #f)
  (if (test-debug-method-mode)
      (begin
       (display "Searching...")
       (newline)))
  (cond
   (  (null? supers)  #f  )
   (  (begin
       (set! method ((car supers) msg))
       (eq? method error-method))
                      (if (test-debug-method-mode)
                          (error-method))
                      (search-supertypes (cdr supers) msg)  )
   (  else  method  )))

; This procedure implements the inheritance of methods.  It is greatly
; complicated by proper error handling.  Without error handling, the
; routine would resemble the following, which is much easier to
; understand (without error handling, the programming convention is
; that an object that does not understand a message returns the
; unexecutable method ``#f'').
;
; (define (search-supertypes supers msg)
;   (cond
;     (  (null? supers)  #f  )
;     (  ((car supers) msg)  )
;     (  else  (search-supertypes (cdr supers) msg)  )))
;
; The actual routine, with proper error handling, works as follows.  A
; local variable, ``method'', is defined.  Its value is not important
; to begin with.  If debugging is on, we print a message telling the
; user that the inheritance hierarchy is being searched.  Then, the
; list of supertypes is investigated.  If the list is empty, we return
; nil, which signals the caller to create and return the error-method,
; as we shall see in the examples later.  If the list is not empty, we
; pass the message to the first supertype in the list.  The return
; value is assigned to the local variable ``method''.  If the returned
; method is the one and only global error-method, then the supertype,
; and, recursively, all its supertypes, did not know the message.  
; If debugging is on, we execute the returned error-method, contributing
; to the aforementioned inheritance traceback.  Finally, we return 
; the value of a recursive call of search-supertypes on the remainder 
; of the list of supertypes.  If the returned method is not the 
; error-method, then the supertype did understand the message after 
; all somewhere in the hierarchy, and the returned method is the
; return value of this procedure.
;
; Note that the list of supertypes is searched in order from front
; to back.  The first match of a message results in the successful
; finding of a method.  The order of supertypes in the list is 
; significant only when more than one supertype can understand
; a given message.  The earlier members of the list will shadow
; the later ones.  In some object-oriented programming systems, one
; refers to the ``overriding'' of methods.  The shadowing in 
; ``methods'' is our form of method overriding, and it is under
; explicit control of the programmer who sets the order of supertypes
; in the list of supertypes.  
; 
; In summary, search-supertypes passes the message to the ancestors,
; in pre-order, returning the first method found.  
;
; The next helper routine passes a message, and a variable number of
; arguments, to all the parents of an object.  For side effects, it
; executes any methods found.  Parents are defined as 
; first level ancestors.

(define (for-all-parents supers msg . args)
  (let (  (method-list
           (map (lambda (supertype) (supertype msg)) supers))
          (for-proc
           (lambda (method) (apply method args)))  )
    (for-each for-proc method-list)))

; With the current programming convention, it is not possible to pass
; a message to all ancestors and execute the methods for side-effect
; without explicit cooperation on the part of the objects involved. In
; other words, the procedure ``for-all-ancestors'', analogous to
; ``for-all-parents'', cannot be implemented in the current version of
; the methods package.  The reason is that the convention calls for
; every class to call ``search-supertypes'', which stops when it finds
; a method.  The convention would have to be augmented so that objects
; would call ``find-all-methods'' (defined below) on an appropriate
; message.  Since we expect the need for ``for-all-ancestors'' to be
; fairly rare, the necessary changes to the methods package will be
; reserved for a future version.

(define (find-all-methods supers msg)
  (cond
   (  (null? supers)  #f  )
   (  else  (cons ((car supers) msg)
                  (find-all-methods (cdr supers) msg))  )))

