;;; $Id: meroon.scm,v 2.22 1992/11/12 18:37:32 queinnec Exp $ change pour elk
;;; Copyright (c) 1990, 91, 92 by Christian Queinnec. All rights reserved.

;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to    
;;;   <queinnec@polytechnique.fr>   or to:   <Christian.Queinnec@inria.fr>
;;;                                 or to:
;;;   Christian Queinnec                     Christian Queinnec
;;;   Laboratoire d'Informatique de l'X      INRIA -- Rocquencourt
;;;   Ecole Polytechnique                    Domaine de Voluceau, BP 105
;;;   91128 Palaiseau                        78153 Le Chesnay Cedex
;;;   France                                 France

;;;                ***********************************************
;;;                    Small, Efficient and Innovative Class System
;;;                          Meroon $Revision: 2.22 $
;;;                              Christian Queinnec  
;;;                   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;                ************************************************

;;; In order to load or compile this file, you must load or compile the
;;; appropriate prologue. Some important facts are:
;;; -a- all errors are mapped onto the function oo-error.
;;; -b- macros are defined by the define-oo-macro macro.
;;; -c- use of macros such as when and until.
;;; -d- some utility functions like mapcan, oo-concat or flat are used.
;;; -e- oo-apply corresponds to a n-ary apply operator.
;;; -f- macros use the gensym function.
;;; The actual prologues are meroon.pcs for PC-Scheme 3.03
;;;                          meroon.s2c for Scheme->C 01nov91 (interpreter)
;;;                          meroon.sc for Scheme->C 01nov91 (compiler)
;;;                          meroon.ss  for Chez Scheme ?
;;;                          meroon.gambit for Gambit Mac 1.9
;;;                          meroon.elk for Elk 1.5

;;; VERSION 2 Notes:
;;; Version 2.x uses much less space than the previous V1.13. Accessors,
;;; allocators are now closures sharing the same code rather than being
;;; optimized one by one at the expense of code duplication. 
;;; Version 2.x is nevertheless slower than the previous 1.13 version.  
;;; I compare V2 / V1.13 on a compiled application with 64 classes, 
;;; 15 generic functions, 52 methods and 487169 calls to generic functions.
;;;         code segment size: 1M / 1.9M     speed: 88s / 59s
;;; The source of the loss is that accessors are now careful.
;;; New features:
;;; All the code around define-class is completely new.
;;; The :careless, :eponymous and :metaclass options are new.
;;; Errors are now mapped on Meroon objects named anomalies. They are
;;; handled by the generic meroon-error function.
;;; The code is checked against case-sensitivity with Elk.
;;; Coercers and initialization are new.

;;;================================================================== The code
;;; This vector records existing classes. Any class can be retrieved by
;;; its number (which acts as an index in this vector). The vector
;;; is automatically extended whenever useful.
(define *classes* (vector #f #f #f #f #f #f #f #f #f #f))

;;; the number of classes in *classes*
(define *class-number* 0)

;;; This vector records all existing generic functions.
;;; This is not good for the GC, but is portable since there is no
;;; need to open the guts of the representation of a generic function
;;; to find the dispatch table. A generic function is represented by
;;; a regular Scheme function AND an instance of class Generic kept
;;; within this vector. This generic object is accessed by its name
;;; which must also be the name of the generic function (sigh).
(define *generics* (vector #f #f #f #f #f #f #f #f))

;;; The number of generic functions in *generics*
(define *generic-number* 0)

;;; The list of traced generic functions. This list is an AList 
;;; mapping names to the alterated fields of traced generic functions.
(define *traced-generics* (list))

;;; All errors seen by meroon lead to an anomaly object stored in this
;;; variable.
(define *last-meroon-anomaly* #f)

;;;========================================================== General utilities
;;; Other utilities appear in the appropriate prologue.

;;; (lit fn end '(e1 e2 .. eN)) -> (fn 'e1 (fn 'e2 .. (fn eN end)..))
(define (lit fn end list)
  (if (pair? list)
      (fn (car list) 
          (lit fn end (cdr list)) )
      end ) )

;;; (vector-map f #(v1 v2 .. vN)) -> (begin (f v1) (f v2) .. (f vN))
(define (vector-map fn v)
  (let ((n (vector-length v)))
    (define (mapvector i)
      (when (< i n)
            (fn (vector-ref v i))
            (mapvector (+ 1 i)) ) )
    (mapvector 0) ) )

;;; Return a new bigger vector. If the second argument is present then
;;; the returned vector is at least that long.
(define (vector-extend s . at-least)
  (let* ((n (vector-length s))
         (r (make-vector (if (pair? at-least)
                             (max (car at-least) n)
                             (+ n 1 (quotient n 2)) )
                         #f )) )
    (vector-copy! s r 0 n)
    r ) )

;;; Copy vector old[start..end[ into vector new[start..end[
(define (vector-copy! old new start end)
  (let copy ((i start))
    (when (< i end)
          (vector-set! new i (vector-ref old i))
          (copy (+ i 1)) ) ) )

;;; Look for an object located in a sequence which, when applied key on it,
;;; returns name. If such object does not exist apply default on name.
;;; This function mimics Common Lisp find function on sequence which 
;;; can be list of sequences.
(define (sequence-find name s key default)
  (cond ((null? s) (default name))
        ((vector? s)
         (let look ((i 0))
           (if (and (< i (vector-length s))
                    (vector-ref s i) )
               (if (eq? name (key (vector-ref s i)))
                   (vector-ref s i)
                   (look (+ i 1)) )
               (default name) ) ) )
        ((pair? s)
         (let look ((s s))
           (if (pair? s)
               (if (eq? name (key (car s)))
                   (car s)
                   (look (cdr s)) )
               (default name) ) ) ) ) )

;;; Look for a value in a plist prefixed by a keyword, 
;;; if not there invoke default.
(define (find-value-in-plist keyword plist default)
  (define (find plist)
    (if (pair? plist)
        (if (eq? keyword (car plist))
            (if (pair? (cdr plist))
                (cadr plist)
                (meroon-error (create-Syntax-Anomaly 'define-class
                               "Incorrect list of options" plist )) )
            (case (car plist)
              ((:metaclass) 
               (if (pair? (cddr plist))
                   (find (cddr plist))
                   (meroon-error (create-Syntax-Anomaly 'define-class
                                  "Incorrect list of options" plist )) ) )
              ((:prototype :mutable :careless)
               (find (cdr plist)) )
              (else (find (cdr plist))) ) ) 
        (default keyword) ) )
  (find plist) )

;;;=============================================================== Define-class
;;; The macro to define a class with a name, a super class and 
;;; the extra fields (some may be indexed). Some class options such
;;; as :prototype, :immutable or :careless can be mentioned.
;;; Unlike Common Lisp, options do not have the structure of a plist,
;;; they are followed by a value only if necessary. The three current
;;; options do not need such values and act by themselves.
(define-oo-macro (define-class name super-name own-field-descs . class-options)
  (unless (symbol? name) 
          (meroon-error (create-Syntax-Anomaly 'define-class 
                     "Not a name for a class" name )) )
  (unless (symbol? super-name) 
          (meroon-error (create-Syntax-Anomaly 'define-class 
                     "Not a name for a super class" super-name )) )
  (let* ((super-class 
          (symbol->class 
           super-name 
           (lambda (name) 
             (meroon-error (create-Syntax-Anomaly 'define-class 
                        "No such super class" name )) ) ) )
         (g (gensym))
        ;; register the class at macroexpand-time
         (class (add-subclass name super-class 
                              own-field-descs class-options )) )
    (if (memq ':prototype class-options)
        ;; The :prototype option is used to register the class not to
        ;; generate code. It is mainly used for separate compilation.
        ;; At load-time the Field-names will be checked for conformity.
        `(let ((,g (let ((,g (symbol->class ',name)))
                     (cons ,g (Class-fields ,g)) )))
           (unless 
            (and ,@(map 
                    (lambda (field)
                      `(begin 
                         (set! ,g (cdr ,g))
                         (and 
                          ;; same name
                          (eq? ',(Field-name field) 
                               (Field-name (car ,g)) )
                          ;; same class
                          (eq? ',(Class-name (object->class field))
                               (Class-name (object->class (car ,g)) ) ) ) ) )
                        (Class-fields class) )
                 (null? (cdr ,g)) )
            (meroon-error 
             (create-Syntax-Anomaly 'define-class
                                    "Inappropriate :prototype !?" 
                                    ',name ) ) )
           ',name )
        ;; Register the class at load-time
        `(let ((,g (add-subclass ',name (symbol->class ',super-name) 
                                 ',own-field-descs ',class-options )))
           ,@(if (memq ':eponymous class-options)
                 `((oo-set! ,name ,g))
                 `() ) 
           ;; Generates the names of associated functions
           ,(generate-related-names class name g)
           ',name ) ) ) )

;;; Generates the name related to a given object (class or field).
;;; Variable will hold the class object at load-time,
;;; and name the name of the class.
;;; TEMP: This function will be redefined to become generic.
(define (generate-related-names o name variable)
  ((cond ((Class? o) generate-class-related-names)
         ((Field? o) generate-field-related-names)
         (else (meroon-error (create-Domain-Anomaly
                              'generate-related-names
                              "No method" o name variable ))) )
   o name variable ) )

;;; Generates all the names that accompany a class. 
(define (generate-class-related-names class name variable)
  `(begin
     (oo-set! ,(oo-concat name "?") (Class-predicate ,variable))
     (oo-set! ,(oo-concat 'make- name) (Class-maker ,variable))
     (oo-set! ,(oo-concat 'allocate- name) (Class-allocator ,variable))
     ;;(define-generic (,(oo-concat '-> name) (o)) ...)
     ;;(define-method (,(oo-concat '-> name) (o ,name)) o)
     (let ((fields (Class-fields ,variable)))
       1951 ; so the body is never empty.
       ,@(map (lambda (field) 
                `(begin 
                   ,(generate-related-names field name '(car fields))
                   (set! fields (cdr fields)) ) )
              (Class-fields class) ) ) ) )

;;; Generates the names for the accessors to a field. 
(define (generate-field-related-names field name variable)
  `(begin
     (oo-set! ,(oo-concat name '- (Field-name field))
              (Field-reader ,variable) )
     ,@(cond 
        ((Mutable-Mono-Field? field)
         `((oo-set! ,(oo-concat 'set- name '- (Field-name field) "!")
                    (Mutable-Mono-Field-writer ,variable) )) )
        ((Mutable-Poly-Field? field)
         `((oo-set! ,(oo-concat 'set- name '- (Field-name field) "!")
                    (Mutable-Poly-Field-writer ,variable) )) )
        (else '()) )
     ,@(if (Poly-Field? field)
           `((oo-set! ,(oo-concat name '- (Field-name field) '-length)
                      (Poly-Field-lengther ,variable) ))
           '() ) ) )

;;; The run-time function that defines a new class. Try to reuse
;;; the old object if it already exists, this is highly questionable
;;; and might disappear in future releases.
(define (class-add-subclass name super-class own-field-descs class-options)
  (let* ((class (symbol->class name (lambda (name) #f)))
         ;; reuse the old number if it already exists.
         (cn (if class (Class-number class) *class-number*))
         (already-there? class)
         (metaclass (find-value-in-plist ':metaclass class-options 
                                         (lambda (kw) 'Class) ))
         (class-allocator (Class-allocator (symbol->class metaclass))) )
    (if (and already-there? (memq ':prototype class-options))
        ;; probably interpreted load time, it is not a redefinition
        ;; since there is the :prototype keyword, the expansion of
        ;; the macro will check the conformity. Return the class as it is!
        class
        (let ((class
               (if (and class 
                        (eq? metaclass (Class-name (object->class class))) )
                   ;; reuse the old object 
                   (let ((old-super-class (Class-super class)))
                     ;; unlink the class from its previous super-class
                     ;; but keep the same subclasses even if meaningless
                     (set-Class-subclasses! 
                      old-super-class 
                      (remove (Class-number class)
                              (Class-subclasses old-super-class) ) )
                     (set-Class-super! class super-class)
                     class )
                   ;; create a fresh class (no arguments means no polyfield !?)
                   (class-allocator) ) ))
          (set-Class-name! class name)
          (set-Class-number! class cn)
          (set-Class-super! class super-class)
          (set-Class-subclasses! class (list))
          (set-Class-predicate!
           class (lambda (o) (and (Object? o) (is-a? o class))) )
          (vector-set! *classes* (Class-number class) class)
          ;; set the number of the next class to be defined
          (unless already-there?
                  (set! *class-number* (+ 1 *class-number*))
                  ;; and extend global resources if necessary
                  (when (>= *class-number* (vector-length *classes*))
                        (extend-classes-number!) ) )
          ;; to create fields needed the class object
          (parse-fields! class own-field-descs class-options)
          ;; these two were waiting for the list of fields.
          (set-Class-allocator! class (create-allocator class))
          (set-Class-maker! class (create-maker class))
          ;; link the class within the subclasses of its super
          (set-Class-subclasses!
           super-class 
           (cons (Class-number class) (Class-subclasses super-class)) )
          ;; Keep the current methods if the class was already existing.
          (unless (or already-there? (memq ':prototype class-options))
                  (let ((num (Class-number class))
                        (super-num (Class-number (Class-super class))) )
                    ;; copy all initial methods from the super
                    (vector-map 
                     (lambda (g)
                       (when g (vector-set! 
                                (Generic-dispatch-table g)
                                num
                                (vector-ref (Generic-dispatch-table g)
                                            super-num ) )) )
                     *generics* ) ) )
          (initialize! class) ) ) ) ) 

;;; TEMP: This function will be redefined to become generic.
(define add-subclass class-add-subclass)

;;; Create a predicate, except Object? they all share the same code.
;;; Caution: the Object? predicate is weak and aproximate.
;;; It is not used in the previous function since class is not yet created.
(define (make-predicate class)
  (lambda (o) (and (Object? o) (is-a? o class))) )

;;;==================================================== Class related functions
;;; The theory is to optimize the case where classes do not use indexed slots.
;;; Nevertheless some optimizations are also performed when there is
;;; only a single indexed slot. 

;;;================================================================== Allocator
;;; Create the allocator of a class. Only two cases are recognized.
(define (create-allocator class)
  (let ((fields (Class-fields class)))
    (if (every? Mono-Field? fields)
        (make-simple-allocator class (length fields))
        (make-complex-allocator class) ) ) )

;;; Create an allocator for classes without indexed slots.
(define (make-simple-allocator class size)
  (let ((num (Class-number class)))
    (lambda ()
      (let ((v (make-vector (+ 1 size) num)))
        ;; If it is a class then add-subclass will trigger the initialization.
        (if (Class? v) v (initialize! v)) ) ) ) )

;;; Creates a generic (unoptimized) allocator.
(define (make-complex-allocator class)
  (let ((fields (Class-fields class)))
    (lambda sizes
      ;; Check sizes
      (unless (every? (lambda (size) (and (number? size) (>= size 0)))
                      sizes )
              (meroon-error (create-Allocation-Anomaly
                             'Meroon-allocator
                             "Sizes must be positive integers" 
                             class sizes )) )
      ;; determine the total size of the instance to allocate
      (let ((v (let count 
                   ((fields fields)
                    (sizes sizes)
                    (total 1) )
                 (if (pair? fields)
                     (cond 
                      ((Poly-Field? (car fields))
                       (if (pair? sizes)
                           (count (cdr fields) (cdr sizes) 
                                  (+ 1 (car sizes) total) )
                           (meroon-error 
                            (create-Allocation-Anomaly
                             'Meroon-allocator 
                             "Missing size" class '() ) ) ) )
                      ((Mono-Field? (car fields))
                       (count (cdr fields) sizes (+ 1 total)) ) )
                     (make-vector total) ) )))
        ;; Structure the bare instance with the correct sizes
        (vector-set! v 0 (Class-number class))
        (let skeletize ((fields fields)
                        (sizes sizes)
                        (index 1) )
          (if (pair? fields)
              (cond ((Poly-Field? (car fields))
                     (vector-set! v index (car sizes))
                     (skeletize (cdr fields) (cdr sizes) 
                                (+ 1 index (car sizes)) ) )
                    ((Mono-Field? (car fields))
                     (skeletize (cdr fields) sizes (+ 1 index)) ) )
              ;; call the generic initialize! function
              (if (Class? v) v (initialize! v)) ) ) ) ) ) );qnc

;;;====================================================================== Maker
;;; Create the maker of a class, only two cases are recognized.
(define (create-maker class)
  (let* ((fields (Class-fields class)))
    (if (every? Mono-Field? fields)
        (make-simple-maker class (length fields))
        (make-complex-maker class) ) ) )

;;; Creates a maker for instances without indexed field.
(define (make-simple-maker class size)
  (let* ((n (Class-number class))
         (fields (Class-fields class))
         (arity (length fields)) )
    (lambda args
      (if (= arity (length args))
          (oo-apply vector n args)
          (meroon-error (create-Allocation-Anomaly 'Meroon-maker 
                     "Incorrect number of arguments" class args )) ) ) ) )

;;; Creates a generic (unoptimized) maker
(define (make-complex-maker class)
  (let* ((n (Class-number class))
         (fields (Class-fields class))
         (arity (length fields)) )
    (lambda original-args
      ;; Check the number of arguments
      (let check
          ((fields fields)
           (args original-args) )
        (if (pair? fields)
            (cond 
             ((Poly-Field? (car fields))
              (if (pair? args)
                  (if (and (integer? (car args))
                           (>= (car args) 0) )
                      (let skip ((args (cdr args))
                                 (size (car args)) )
                        (if (> size 0)
                            (if (pair? args)
                                (skip (cdr args) (- size 1))
                                (meroon-error 
                                 (create-Allocation-Anomaly 'Meroon-maker
                                  "Not enough arguments" class '() )) )
                            (check (cdr fields) args) ) )
                      (meroon-error 
                       (create-Allocation-Anomaly 'Meroon-maker
                        "Incorrect size" class args ) ) )
                  (meroon-error 
                   (create-Allocation-Anomaly 'meroon-maker 
                    "Missing size" class args ) ) ) )
             ((Mono-Field? (car fields))
              (if (pair? args)
                  (check (cdr fields) (cdr args))
                  (meroon-error 
                   (create-Allocation-Anomaly 'Meroon-maker 
                    "Not enough field" class args ) ) ) ) )
            ;; do not call initialize!
            (if (null? args)
                (oo-apply vector n original-args)
                (meroon-error 
                 (create-Allocation-Anomaly 'Meroon-maker 
                  "Too much arguments" class args ) ) ) ) ) ) ) )

;;;;=================================================== Field related functions
;;; parse own-field-descs, create the field objects with the appropriate
;;; accessors. Fields have a slot to indicate the class which 
;;; introduced them. To make them printable, it is the Class-number rather
;;; than the class itself (as in the subclasses field of classes).
;;; Return the list of field-descriptors.
(define (parse-fields! class own-field-descs class-options)
  (let ((super-fields (Class-fields (Class-super class)))
        (num (Class-number class)) )
    (let scan ((own-field-descs own-field-descs)
               (fields super-fields) )
      ;; create a field object
      (define (parse type name field-options)
        (unless (symbol? name)
                (meroon-error (create-Syntax-Anomaly 'define-class
                           "Not a field name" class name )) )
        ;; check if the field is not redefined
        (when (sequence-find name fields
                             Field-name (lambda (name) #f) )
              (meroon-error 
               (create-Syntax-Anomaly 'define-class 
                "Field redefinition" class name ) ) )
        (create-accessors! ; must return the initialized field
         (case type
           ((mono)
            (if (memq ':immutable field-options)
                (make-Mono-Field name num 'wait)
                (make-Mutable-Mono-Field name num 'wait 'wait) ) )
           ((poly)
            (if (memq ':immutable field-options)
                (make-Poly-Field name num 'wait 'wait)
                (make-Mutable-Poly-Field name num 'wait 'wait 'wait) ) ) )
         fields field-options ) )
      ;; parse each field-descriptors
      (if (pair? own-field-descs)
          (let* ((field-desc (car own-field-descs))
                 (field 
                  (cond ((symbol? field-desc)
                         (parse 'mono field-desc class-options) )
                        ((and (pair? field-desc) ; at least a field name
                              (pair? (cdr field-desc)) )
                         (case (car field-desc)
                           ((= mono) 
                            (parse 'mono (cadr field-desc) 
                                   (append (cddr field-desc)
                                           class-options ) ) )
                           ((* poly)
                            (parse 'poly (cadr field-desc) 
                                   (append (cddr field-desc)
                                           class-options ) ) )
                           (else (meroon-error 
                                  (create-Syntax-Anomaly 'define-class
                                   "Invalid field descriptor" 
                                   class field-desc ) )) ) )
                        (else (meroon-error 
                               (create-Syntax-Anomaly 'define-class
                                "Invalid field description"
                                class field-desc ) )) ) ))
            (scan (cdr own-field-descs) (append fields (list field))) )
          (begin (set-Class-fields! class fields)
                 fields ) ) ) ) )

;;; Create the accessors to a field. Field is a Field instance,
;;; fields is the list of preceding fields in the instance.
;;; This function will be redefined as a generic function later.
(define (create-accessors! field fields field-options)
  (cond                                 ; create lengther
   ((Poly-Field? field)
    (set-Poly-Field-lengther!
     field (create-poly-lengther field fields field-options) ) ) )
  (cond                                 ; create writer
   ((Mutable-Mono-Field? field)
    (set-Mutable-Mono-Field-writer!
     field (create-mono-writer field fields field-options) ) )
   ((Mutable-Poly-Field? field)
    (set-Mutable-Poly-Field-writer!
     field (create-poly-writer field fields field-options) ) ) )
  (cond                                 ; create reader in any case
   ((Mono-Field? field)
    (set-Field-reader!
     field (create-mono-reader field fields field-options) ) )
   ((Poly-Field? field)
    (set-Field-reader! ;;qnc
     field (create-poly-reader field fields field-options) ) ) )
  field )

;;;================================================================ Mono-Field 
;;; Create a reader for a Mono-Field. Three cases are recognized:
;;; access to a (possibly indexed) slot without preceding indexed slots,
;;; access to a slot with one preceding indexed slot,
;;; general access to a slot.

(define (create-mono-reader field fields field-options)
  (let ((class (number->class (Field-class field))))
    (let count ((offset 1)
                (fields fields)
                (indexed-offsets '()) )
      (if (null? fields)
          (cond ((null? indexed-offsets) 
                 (make-direct-reader 
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset ) )
                ((null? (cdr indexed-offsets))
                 (make-post-indexed-reader
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset (car indexed-offsets) ) )
                (else (make-complex-reader 
                       (if (memq ':careless field-options) 'careless 'careful)
                       field class offset (reverse indexed-offsets) )) )
          (cond ((Poly-Field? (car fields))
                 (count 1 (cdr fields) (cons offset indexed-offsets)) )
                ((Mono-Field? (car fields))
                 (count (+ 1 offset) (cdr fields) indexed-offsets) ) ) ) ) ) )

;;; Create a writer for a Mono-Field
(define (create-mono-writer field fields field-options)
  (let ((class (number->class (Field-class field))))
    (let count ((offset 1)
                (fields fields)
                (indexed-offsets '()) )
      (if (null? fields)
          (cond ((null? indexed-offsets) 
                 (make-direct-writer 
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset ) )
                ((null? (cdr indexed-offsets))
                 (make-post-indexed-writer
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset (car indexed-offsets) ) )
                (else (make-complex-writer 
                       (if (memq ':careless field-options) 'careless 'careful)
                       field class offset (reverse indexed-offsets) )) )
          (cond ((Poly-Field? (car fields))
                 (count 1 (cdr fields) (cons offset indexed-offsets)) )
                ((Mono-Field? (car fields))
                 (count (+ 1 offset) (cdr fields) indexed-offsets) ) ) ) ) ) )

;;; Create a lengther for a Poly-Field.
(define (create-poly-lengther field fields field-options)
  (create-mono-reader field fields field-options) )

(define (create-poly-reader field fields field-options)
  (let ((class (number->class (Field-class field))))
    (let count ((offset 1)
                (fields fields)
                (indexed-offsets '()) )
      (if (null? fields)
          (cond ((null? indexed-offsets) 
                 (make-indexed-direct-reader 
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset ) )
                (else (make-indexed-complex-reader 
                       (if (memq ':careless field-options) 'careless 'careful)
                       field class offset (reverse indexed-offsets) )) )
          (cond ((Poly-Field? (car fields))
                 (count 1 (cdr fields) (cons offset indexed-offsets)) )
                ((Mono-Field? (car fields))
                 (count (+ 1 offset) (cdr fields) indexed-offsets) ) ) ) ) ) )

(define (create-poly-writer field fields field-options)
  (let ((class (number->class (Field-class field))))
    (let count ((offset 1)
                (fields fields)
                (indexed-offsets '()) )
      (if (null? fields)
          (cond ((null? indexed-offsets) 
                 (make-indexed-direct-writer 
                  (if (memq ':careless field-options) 'careless 'careful)
                  field class offset ) )
                (else (make-indexed-complex-writer 
                       (if (memq ':careless field-options) 'careless 'careful)
                       field class offset (reverse indexed-offsets) )) )
          (cond ((Poly-Field? (car fields))
                 (count 1 (cdr fields) (cons offset indexed-offsets)) )
                ((Mono-Field? (car fields))
                 (count (+ 1 offset) (cdr fields) indexed-offsets) ) ) ) ) ) )

;;;=========================================== Shared accessor for regular slot
;;; careful accessors check that the accessed intance has a correct class.
;;; careless do not (you can use careless accessors by adding :careless
;;; to the field or class option). 
(define (make-direct-reader care field class offset)
  (case care
    ((careful) (lambda (o) (if (is-a? o class)
                               (vector-ref o offset)
                               (meroon-error 
                                (create-Domain-Anomaly
                                 field "Wrong class" class o ) ) )))
    ((careless) (lambda (o) (vector-ref o offset))) ) )

(define (make-direct-writer care field class offset)
  (case care
    ((careful) (lambda (o value) (if (is-a? o class)
                                     (vector-set! o offset value)
                                     (meroon-error 
                                      (create-Domain-Anomaly
                                       field "Wrong class" 
                                       class o value ) ) ) ))
    ((careless) (lambda (o value) (vector-set! o offset value))) ) )

(define (make-post-indexed-reader care field class offset indexed-offset)
  (case care
    ((careful) (lambda (o) 
                 (if (is-a? o class)
                     (vector-ref o (+ indexed-offset
                                      (vector-ref o indexed-offset)
                                      offset ))
                     (meroon-error (create-Domain-Anomaly
                                field "Wrong class" class o ) ) ) ))
    ((careless) (lambda (o) (vector-ref o (+ indexed-offset
                                            (vector-ref o indexed-offset)
                                            offset )))) ) )

(define (make-post-indexed-writer care field class offset indexed-offset)
  (case care
    ((careful) (lambda (o v) 
                 (if (is-a? o class)
                     (vector-set! o (+ indexed-offset
                                       (vector-ref o indexed-offset)
                                       offset ) v )
                     (meroon-error (create-Domain-Anomaly
                                field "Wrong class" class o v ) ) ) ))
    ((careless) (lambda (o v) 
                  (vector-set! o (+ indexed-offset
                                    (vector-ref o indexed-offset)
                                    offset ) v ) )) ) )

;;; These define the most complex case. It allows to read a slot
;;; located offset slot after the indexed slots which are at
;;; relative indexed-offsets. 
(define (make-complex-reader care field class offset indexed-offsets)
  (lambda (o)
    (when (eq? care 'careful) 
          (unless (is-a? o class) (meroon-error 
                                   (create-Domain-Anomaly
                                    field "Wrong class" class o ) )) )
    (vector-ref o (compute-offset o offset indexed-offsets)) ) )
                         
(define (make-complex-writer care field class offset indexed-offsets)
  (lambda (o v)
    (when (eq? care 'careful) 
          (unless (is-a? o class) (meroon-error 
                                   (create-Domain-Anomaly
                                    field "Wrong class" class o v ) )) )
    (vector-set! o (compute-offset o offset indexed-offsets) v) ) )

(define (compute-offset o offset indexed-offsets)
  (let compute ((indexed-offsets indexed-offsets)
                (index 0) )
    ;;(format #t "offsets=~A, index=~A.~%" indexed-offsets index)
    (if (pair? indexed-offsets)
        (compute (cdr indexed-offsets) 
                 (+ index (car indexed-offsets)
                    (vector-ref o (+ index (car indexed-offsets))) ) )
        (+ index offset) ) ) )

;;;=========================================== Shared accessor for indexed slot
;;; These functions are similar to the previous ones except that they
;;; deal with indexed fields. They take as extra argument the index
;;; within the indexed field and dynamically check if the index is
;;; within bounds (except in careless mode).
(define (make-indexed-direct-reader care field class offset)
  (case care
    ((careful) (lambda (o i) 
                 (if (is-a? o class)
                     (if (and (>= i 0) (< i (vector-ref o offset)))
                         (vector-ref o (+ i 1 offset))
                         (meroon-error 
                          (create-Domain-Anomaly
                           field "Index out of bounds" class o i ) ) )
                     (meroon-error 
                      (create-Domain-Anomaly
                       field "Wrong class" 
                       class o ) ) ) ))
    ((careless) (lambda (o i) (vector-ref o (+ i 1 offset)))) ) )

(define (make-indexed-direct-writer care field class offset)
  (case care
    ((careful) (lambda (o i value) 
                 (if (is-a? o class)
                     (if (and (>= i 0) (< i (vector-ref o offset)))
                         (vector-set! o (+ i 1 offset) value)
                         (meroon-error 
                          (create-Domain-Anomaly
                           field "Index out of bounds" class o i value ) ) )
                     (meroon-error 
                      (create-Domain-Anomaly
                       field "Wrong class" class o i value ) ) ) ))
    ((careless) (lambda (o i value) (vector-set! o (+ i 1 offset) value))) ) )

(define (make-indexed-complex-reader care field class offset indexed-offsets)
  (lambda (o i)
    (when (eq? care 'careful) 
          (unless (is-a? o class) (meroon-error
                                   (create-Domain-Anomaly
                                    field "Wrong class" class o ) )) )
    (let ((offset (compute-offset o offset indexed-offsets)))
      (if (and (>= i 0) (< i (vector-ref o offset)))
          (vector-ref o (+ i 1 offset))
          (meroon-error 
           (create-Domain-Anomaly
            field "Index out of bounds" class o i ) ) ) ) ) )

(define (make-indexed-complex-writer care field class offset indexed-offsets)
  (lambda (o i v)
    (when (eq? care 'careful) 
          (unless (is-a? o class) (meroon-error 
                                   (create-Domain-Anomaly
                                    field "Wrong class" class o i v ) )) )
    (let ((offset (compute-offset o offset indexed-offsets)))
      (if (and (>= i 0) (< i (vector-ref o offset)))
          (vector-set! o (+ i 1 offset) v)
          (meroon-error 
           (create-Domain-Anomaly
            field "Index out of bounds" class o i v ) ) ) ) ) )

;;;========================================================= Initial accessors
;;; Build by hand the initial set of accessors needed for the bootstrap
;;; They will be redefined as careful accessors at the end of the bootstrap.
(define Class-name (make-direct-reader 'careless '-f- '-c- 1))
(define meroon+Class-name Class-name)
(define set-Class-name! (make-direct-writer 'careless '-f- '-c- 1))
(define Class-number (make-direct-reader 'careless '-f- '-c- 2))
(define meroon+Class-number Class-number)
(define set-Class-number! (make-direct-writer 'careless '-f- '-c- 2))
(define Class-fields (make-direct-reader 'careless '-f- '-c- 3))
(define set-Class-fields! (make-direct-writer 'careless '-f- '-c- 3))
(define Class-super (make-direct-reader 'careless '-f- '-c- 4))
(define meroon+Class-super Class-super)
(define set-Class-super! (make-direct-writer 'careless '-f- '-c- 4))
(define Class-subclasses (make-direct-reader 'careless '-f- '-c- 5))
(define set-Class-subclasses! (make-direct-writer 'careless '-f- '-c- 5))
(define Class-predicate (make-direct-reader 'careless '-f- '-c- 6))
(define set-Class-predicate! (make-direct-writer 'careless '-f- '-c- 6))
(define Class-allocator (make-direct-reader 'careless '-f- '-c- 7))
(define set-Class-allocator! (make-direct-writer 'careless '-f- '-c- 7))
(define Class-maker (make-direct-reader 'careless '-f- '-c- 8))
(define set-Class-maker! (make-direct-writer 'careless '-f- '-c- 8))

(define Field-name (make-direct-reader 'careless '-f- '-c- 1))
(define Field-class (make-direct-reader 'careless '-f- '-c- 2))
(define set-Field-class! (make-direct-writer 'careless '-f- '-c- 2))
(define Field-reader (make-direct-reader 'careless '-f- '-c- 3))
(define set-Field-reader! (make-direct-writer 'careless '-f- '-c- 3))
(define Poly-Field-lengther (make-direct-reader 'careless '-f- '-c- 4))
(define set-Poly-Field-lengther! (make-direct-writer 'careless '-f- '-c- 4))
(define Mutable-Mono-Field-writer (make-direct-reader 'careless '-f- '-c- 4))
(define set-Mutable-Mono-Field-writer! 
  (make-direct-writer 'careless '-f- '-c- 4) )
(define Mutable-Poly-Field-writer (make-direct-reader 'careless '-f- '-c- 5))
(define set-Mutable-Poly-Field-writer!
  (make-direct-writer 'careless '-f- '-c- 5) )

(define Generic-name (make-direct-reader 'careless '-f- '-c- 1))
(define meroon+Generic-name Generic-name)
(define set-Generic-name! (make-direct-writer 'careless '-f- '-c- 1))
(define Generic-discriminator (make-direct-reader 'careless '-f- '-c- 2))
(define set-Generic-discriminator! (make-direct-writer 'careless '-f- '-c- 2))
(define Generic-dispatch-table (make-direct-reader 'careless '-f- '-c- 3))
(define set-Generic-dispatch-table! (make-direct-writer 'careless '-f- '-c- 3))
(define Generic-default (make-direct-reader 'careless '-f- '-c- 4))
(define set-Generic-default! (make-direct-writer 'careless '-f- '-c- 4))
(define Generic-variables (make-direct-reader 'careless '-f- '-c- 5))
(define set-Generic-variables! (make-direct-writer 'careless '-f- '-c- 5))

(define allocate-Generic 'wait)
(define make-Mono-Field 'wait)
(define make-Poly-Field 'wait)
(define make-Mutable-Mono-Field 'wait)
(define make-Mutable-Poly-Field 'wait)

(define Class? 'wait)
(define Field? 'wait)
(define Mutable-Mono-Field? 'wait)
(define Mono-Field? 'wait)
(define Mutable-Poly-Field? 'wait)
(define Poly-Field? 'wait)

;;;======================================================== Predefined classes
;;; Build by hand the initial net of classes with all the relationship
;;; needed to bootstrap the rest of the file.
(let* ((mkmf (lambda (name) ; create a Mutable-Mono-Field (by hand)
               (vector 6 name 'wait 'wait 'wait) ))
       (o (vector 1                     ; internal index (Object is a Class)
                  'Object               ; name
                  0                     ; number
                  (list)                ; fields
                  #f                    ; super (here there is no parent)
                  (list 1 2 3)          ; subclasses
                  'wait                 ; predicate
                  'wait                 ; allocator
                  'wait                 ; maker
                  ))
       (c (vector 1
                  'Class
                  1
                  (map mkmf 
                       '(name number fields super subclasses
                              predicate allocator maker ) )
                  o
                  (list)
                  'wait
                  'wait
                  'wait ))
       (g (vector 1
                  'Generic
                  2
                  (map mkmf
                       '(name discriminator dispatch-table default
                              variables ) )
                  o
                  (list)
                  'wait
                  'wait
                  'wait ))
       (ff (vector 1
                   'Field
                   3
                   (map mkmf '(name class reader))
                   o
                   (list 4 5)
                   'wait
                   'wait
                   'wait ))
       (fm (vector 1
                   'Mono-Field
                   4
                   (map mkmf '(name class reader))
                   ff
                   (list 6)
                   'wait
                   'wait
                   'wait ))
       (fp (vector 1
                   'Poly-Field
                   5
                   (map mkmf '(name class reader lengther))
                   ff
                   (list 7)
                   'wait
                   'wait
                   'wait ))
       (mfm (vector 1
                   'Mutable-Mono-Field
                   6
                   (map mkmf '(name class reader writer))
                   fm
                   (list)
                   'wait
                   'wait
                   'wait ))
       (mfp (vector 1
                   'Mutable-Poly-Field
                   7
                   (map mkmf '(name class reader lengther writer))
                   fp
                   (list)
                   'wait
                   'wait
                   'wait )) )
  (vector-set! *classes* 0 o)
  (vector-set! *classes* 1 c)
  (vector-set! *classes* 2 g)
  (vector-set! *classes* 3 ff)
  (vector-set! *classes* 4 fm)
  (vector-set! *classes* 5 fp)
  (vector-set! *classes* 6 mfm)
  (vector-set! *classes* 7 mfp)
  (set! *class-number* 8)
  (set! allocate-Generic 
        (lambda () 
          (let ((v (make-vector (+ 1 (length (Class-fields g))))))
            (vector-set! v 0 (Class-number g))
            v ) ) )
  (oo-set! make-Mono-Field 
          (lambda (name class reader)
            (vector (Class-number fm) name class reader)) )
  (oo-set! make-Poly-Field 
          (lambda (name class reader lengther)
            (vector (Class-number fp) name class reader lengther)) )
  (oo-set! make-Mutable-Mono-Field 
          (lambda (name class reader writer)
            (vector (Class-number mfm) name class reader writer)) )
  (oo-set! make-Mutable-Poly-Field 
          (lambda (name class reader lengther writer)
            (vector (Class-number mfp) name class reader lengther writer )) )
  (set-Class-predicate! c (make-predicate c))
  (set-Class-predicate! g (make-predicate g))
  (set-Class-predicate! ff (make-predicate ff))
  (set-Class-predicate! fm (make-predicate fm))
  (set-Class-predicate! fp (make-predicate fp))
  (set-Class-predicate! mfm (make-predicate mfm))
  (set-Class-predicate! mfp (make-predicate mfp))
  (oo-set! Class? (Class-predicate c))
  (oo-set! Field? (Class-predicate ff))
  (oo-set! Mono-Field? (Class-predicate fm))
  (oo-set! Mutable-Mono-Field? (Class-predicate mfm))
  (oo-set! Poly-Field? (Class-predicate fp))
  (oo-set! Mutable-Poly-Field? (Class-predicate mfp))
  'genesis )

;;;========================================================== Primitive library
;;; tests if the instance O belongs to class. O is assumed to be an object.
;;; It is important to use direct (careless) accessors to avoid endless loops.
;;; This predicate must bevery efficient since it is called before any
;;; use of a careful accessor.
;(define is-a? 
;  (lambda (o class)
;    (let ((n (meroon+Class-number class)))
;      (let up ((c (object->class o)))
;        (or (= n (meroon+Class-number c))
;            (and (meroon+Class-super c)
;                 (up (meroon+Class-super c)) ) ) ) ) ) )
;;; 25% faster on the bench:
(define is-a? 
  (lambda (o class)
    (let up ((c (object->class o)))
      (or (eq? class c)
          (let ((sc (meroon+Class-super c)))
            (and sc (up sc)) ) ) ) ) )
;;; 1% faster on the bench:
;(define is-a? 
;  (let ((old-o #f)
;        ;; Optimize the 100 first subclass-levels.
;        ;; classes contains the cached classes of the last instance, starting
;        ;; with the class of old-o up to the looked for class and followed
;        ;; by a #f to mark where the research halted.
;        (classes (make-vector 100 #f)) )
;    (lambda (o class)
;      (if (eq? o old-o)
;          (let look ((i 1)              ; next index
;                     (c (vector-ref classes 0)) ) ; (object->class old-o)
;            (or (eq? class c)
;                (if c
;                    (look (+ 1 i) (vector-ref classes i))
;                    ;; c was #f
;                    (let up ((i (- i 1))
;                             (c (vector-ref classes (- i 2))) )
;                      (let ((super (meroon+Class-super c)))
;                        (vector-set! classes i super)
;                        (and super (or (eq? super class)
;                                       (up (+ 1 i) super) )) ) ) ) ) )
;          (let ((c (object->class o)))
;            (vector-set! classes 0 c)
;            (let up ((i 1) (c c))
;              (or (and (eq? class c)
;                       (begin (vector-set! classes i #f) #t) )
;                  (let ((super (meroon+Class-super c)))
;                    (vector-set! classes i super)
;                    ;; Object's super is #f.
;                    (and super (up (+ 1 i) super)) ) ) ) ) ) ) ) )
;;; The original one with eq? instead of = [Thanks to Manuel Serrano]
;;; still 25% slower.
;(define is-a? 
;  (lambda (o class)
;    (let ((n (meroon+Class-number class)))
;      (let up ((c (object->class o)))
;        (or (eq? n (meroon+Class-number c))
;            (and (meroon+Class-super c)
;                 (up (meroon+Class-super c)) ) ) ) ) ) )

;;; extracts the internal number of a class from within an instance.
(define (object->class-number o)
  (vector-ref o 0) )

;;; return the class of an object (class-of in other OOL).
(define (object->class o)
  (vector-ref *classes* (object->class-number o)) )

;;; return the ith known class.
(define (number->class i)
  (vector-ref *classes* i) )

;;; returns the class named name or apply default on this name.
;;; This is an expensive function since it uses sequence-find.
;;; It is important to use direct (careless) accessors to avoid endless loops.
(define (symbol->class name . default)
  (sequence-find
   name *classes* 
   meroon+Class-name
   (if (pair? default)
       (car default)
       (lambda (name) (meroon-error 
                       (create-Anomaly 'symbol->class 
                                       "No such class" name ) )) ) ) )

;;; returns the generic instance named name or apply default on this name.
;;; This is an expensive function since it uses sequence-find.
(define (symbol->generic name . default)
  (sequence-find 
   name *generics* 
   meroon+Generic-name
   (if (pair? default)
       (car default)
       (lambda (name) (meroon-error 
                       (create-Anomaly 'symbol->generic 
                                     "No such generic" name ) )) ) ) )

;;; The following function is temporarily defined and 
;;; is used only during the bootstrap. It will be turned 
;;; into a generic function that the user can customize.
;;; TEMP: This will become a generic function.
(define (initialize! o) o)

;;; extend the vector of classes as well as the dispatch tables of all
;;; generic functions (whether traced or not).
(define (extend-classes-number!)
  (vector-map (lambda (g)
                (when g
                      (set-Generic-dispatch-table!
                       g (vector-extend (Generic-dispatch-table g)) ) ) )
              *generics* )
  (set! *classes* (vector-extend *classes*))
  #f )

;;;============================================================= Define-generic
;;; find a pair in specs while flattening specs into a variable-list:
;;; call k on these two results. Useful for parsing define-generic
;;; and define-method.
(define (parse-disc specs k)
  (let ((disc #f)
        (variables 'wait) )
    (set! variables
          (let search ((vars specs))
            (if (pair? vars)
                (if (pair? (car vars))
                    (begin (set! disc (car vars))
                           (cons (caar vars) (search (cdr vars))) )
                    (cons (car vars) (search (cdr vars))) )
                vars ) ) )
    (if disc (k disc variables)
        (meroon-error 
         (create-Syntax-Anomaly
          'define-method "No discrimination variable" specs ) ) ) ) )

;;; (define-generic (foo x (y) z) [ . default-body])
;;; Methods are represented by fixed arity functions.
(define-oo-macro (define-generic call . body)
  (unless (and (pair? call)
               (symbol? (car call))
               (pair? (cdr call)) )
          (meroon-error 
           (create-Syntax-Anomaly
            'define-generic "Incorrect invokation form" call ) ) )
  (parse-disc 
   (cdr call)
   (lambda (disc-spec variables)
     (let ((generic (gensym))
           (discriminator (gensym))
           (default (gensym)) )
       `(oo-set! ,(car call) 
              (letrec 
                  ((,generic (create-generic ',(car call)))
                   (,discriminator
                    (lambda ,variables
                      ((if (Object? ,(car disc-spec)) 
                           (vector-ref
                            (Generic-dispatch-table ,generic)
                            (object->class-number ,(car disc-spec)) )
                           ,default )
                       . ,(flat variables) ) ) )
                   (,default
                     (lambda ,(flat variables) 
                       ,(if (pair? body)
                            `(begin . ,body)
                            `(meroon-error
                              (create-Domain-Anomaly ,generic
                               "No method for" . ,(flat variables) ) ) ) ) ) )
                (set-Generic-variables! ,generic ',(cdr call))
                (set-Generic-discriminator! ,generic ,discriminator)
                (set-Generic-default! ,generic ,default)
                (set-Generic-dispatch-table!
                 ,generic (make-vector (vector-length *classes*) ,default) )
                ;; This is the original discriminator bound to the name
                ;; of the generic function.
                (lambda ,variables
                  (if (eq? ,discriminator (Generic-discriminator ,generic))
                      ;; Normal (and fast) case similar to
                      ;; (,discriminator . ,(flat variables)) but
                      ;; removes the cost of one invokation.
                      ((if (Object? ,(car disc-spec)) ;;qnc
                           (vector-ref
                            (Generic-dispatch-table ,generic)
                            (object->class-number ,(car disc-spec)) ) ;qnc
                           ,default )
                       . ,(flat variables) )
                      ;; only useful when a function is traced:
                      ,(if (null? (cdr (last-pair variables)))
                           `((Generic-discriminator ,generic)
                             . ,variables )
                           `(oo-apply (Generic-discriminator ,generic)
                                      . ,(flat variables) ) ) ) ) ) ) ) ) ) )

;;; Creates a new generic instance (trying to reuse an old one).
(define (create-generic name)
  (or (symbol->generic name (lambda (name) #f) )
      (let ((g (allocate-Generic)))
        (set-Generic-name! g name)
        (when (>= *generic-number* (vector-length *generics*))
              (set! *generics* (vector-extend *generics*)) )
        (vector-set! *generics* *generic-number* g)
        (set! *generic-number* (+ 1 *generic-number*))
        g ) ) )

;;;============================================================== Define-method
;;; (define-method (foo x (y class) z) . body)
;;; Even if the list of variables is dotted, the corresponding
;;; method has a fix arity.
(define-oo-macro (define-method call . body)
  (unless (and (pair? call)
               (symbol? (car call)) )
          (meroon-error 
           (create-Syntax-Anomaly 'define-method 
                                "Incorrect invokation form" call ) ) )
  (parse-disc 
   (cdr call)
   (lambda (disc-spec variables)
     (unless (pair? (cdr disc-spec))
             (meroon-error 
              (create-Syntax-Anomaly 'define-method 
                                   "No discriminating class" disc-spec ) ) )
     (let ((g (gensym)) (c (gensym)))
       `(register-method 
         ',(car call)
         ',(cadr disc-spec)
         ',(cdr call)
         (lambda (,g ,c)
           (lambda ,(flat variables)
             ;; Call the super-method with the same arguments
             (define (call-next-method)
               ((vector-ref (Generic-dispatch-table ,g)
                            (Class-number (Class-super ,c)) )
                . ,(flat variables) ) )
             . ,body ) ) ) ) ) ) )

;;; This function is called at run-time to officially register the method.
(define (register-method generic-name
                         discriminating-class-name
                         variable-list
                         method-maker )
  (let ((g (symbol->generic generic-name
                            (lambda (name)
                              (meroon-error 
                               (create-Domain-Anomaly
                                'define-method
                                "No such generic function"
                                generic-name discriminating-class-name
                                variable-list method-maker ) ) ) ))
        (c (symbol->class discriminating-class-name
                          (lambda (name)
                              (meroon-error 
                               (create-Domain-Anomaly
                                'define-method
                                "No such class"
                                generic-name discriminating-class-name
                                variable-list method-maker ) ) ) )) )
    (unless (coherent-variables? (Generic-variables g) variable-list)
            (meroon-error
             (create-Syntax-Anomaly 'define-method 
                                  "Non congruent lambda-lists" 
                                  (Generic-variables g) variable-list ) ) )
    (add-method! g c (method-maker g c)) ) )

;;; Add a method (a function) to a generic associated with a given class.
;;; The method is propagated to all subclasses which do not redefine it.
(define (add-method! generic class method)
  (define (propagate class old-method)
    (when (eq? old-method (vector-ref (Generic-dispatch-table generic)
                                      (Class-number class) ))
          (vector-set! (Generic-dispatch-table generic)
                       (Class-number class)
                       method )
          (for-each (lambda (c) (propagate (number->class c) old-method))
                    (Class-subclasses class) ) ) )
  (propagate class (vector-ref (Generic-dispatch-table generic)
                               (Class-number class) ))
  method )

;;; Test the congruence of two variable lists. 
;;; They may both contain a final n-ary (dotted) variable (which 
;;; cannot be the discriminating variable (of course).
(define (coherent-variables? la lb)
  (if (pair? la)
      (if (pair? lb)
          (and (or ;; similar discriminating variable
                   (and (pair? (car la))
                        (pair? (car lb)) )
                   ;; similar regular variable
                   (and (symbol? (car la))
                        (symbol? (car lb)) ) )
               (coherent-variables? (cdr la) (cdr lb)) )
          #f )
      (or (and (null? la) (null? lb))
          ;; similar dotted variable
          (and (symbol? la) (symbol? lb)) ) ) )

;;;======================================================== fundamental class
;;; Object is too intricate to be regenerated, so it is hand-built.
;;;(define-class Object no-super ())

;;; rather weak ! Could be improved when Scheme can create new types.
(define Object? 
  (lambda (o)
    (and (vector? o)
         (number? (object->class-number o)) ) ) )

(define make-Object
  (let ((n (Class-number (symbol->class 'Object))))
    (lambda () (vector n)) ) )

(define allocate-Object make-Object)

(let ((o (symbol->class 'Object)))
  ;; do not share instances of Object
  (set-Class-predicate! o Object?)
  (set-Class-allocator! o make-Object)
  (set-Class-maker!     o make-Object)
  "Object built" )

;;;================================================== Other fundamental classes
;;; The other fundamental classes can now be automatically regenerated
;;; as well as all related functions are now carefully (re)defined.

(define-class Class Object 
  (name number fields super subclasses predicate allocator maker) )

(define-class Generic Object 
  (name discriminator dispatch-table default variables))

(define-class Field Object (name class reader))

(define-class Mono-Field Field ())

(define-class Poly-Field Field (lengther))

(define-class Mutable-Mono-Field Mono-Field (writer))

(define-class Mutable-Poly-Field Poly-Field (writer))

(define-class Anomaly Object (intention message (* culprits)))

;;;=================================================================== Library

;;; Automatically called on new instances and predefined to do nothing.
;;; It is there to be customized on your classes. 
;;; There is not yet a uniform protocal for initialization.       FUTURE
(define-generic (initialize! (o)) o)

;;; Field-writer masks mono- or poly- fields
(define-generic (Field-writer (o)))

(define-method (Field-writer (o Mutable-Mono-Field))
  (Mutable-Mono-Field-writer o) )

(define-method (Field-writer (o Mutable-Poly-Field))
  (Mutable-Poly-Field-writer o) )

;;; and so does Mutable-Field? 
(define-generic (Mutable-Field? (o)))

(define-method (Mutable-Field? (o Field)) #f)
(define-method (Mutable-Field? (o Mutable-Mono-Field)) #t)
(define-method (Mutable-Field? (o Mutable-Poly-Field)) #t)

;;;====================================================== Meta-Object utilities
;;; Make generic some of the previous functions so that metaclasses can
;;; be used ie Class can be subclassed.

;;; takes an object and generates the global names that are associated
(define-generic (generate-related-names (o) name variable))

(define-method (generate-related-names (o Class) name variable)
  (generate-class-related-names o name variable) )

(define-method (generate-related-names (o Field) name variable)
  (generate-field-related-names o name variable) )

;;; Creates a new class as subclass of another
(define-generic (add-subclass name (super-class) 
                              own-field-descs class-options ))

(define-method (add-subclass name (super-class Class) 
                             own-field-descs class-options )
  (class-add-subclass name super-class own-field-descs class-options) )

;;;=================================================================== Printing
;;; Show printed images of instances.
(define-generic (show (o) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (cond ((symbol? o) (display o stream))
          ((boolean? o) (display (if o "#T" "#F") stream))
          ((null? o) (display "()" stream))
          ((string? o) (display o stream))
          ((number? o) (display o stream))
          ((pair? o) (show-list o stream))
          ((vector? o) (show-vector o stream))
          ((procedure? o) (display o stream))
          ((char? o) (display o stream))
          (else (meroon-error 
                 (create-Domain-Anomaly 
                  (symbol->generic 'show)
                  "Does not know how to show" o ) )) ) ) )

(define (show-list o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (define (show-content o)
      (show (car o) stream)
      (cond ((null? (cdr o)) #t)
            ((pair? (cdr o)) (display " " stream)
                             (show-content (cdr o)) )
            (else (display " . " stream)
                  (show (cdr o) stream) ) ) )
    (display "(" stream)
    (show-content o)
    (display ")" stream) ) )

(define (show-vector o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (n (vector-length o)) )
    (define (show-content i)
      (when (< i n) 
            (show (vector-ref o i) stream)
            (display " " stream)
            (show-content (+ 1 i)) ) )
    (display "#(" stream)
    (show-content 0)
    (display ")" stream) ) )

(define-method (show (o Object) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port)))
        (name (Class-name (object->class o))) )
    (display "#<a" stream)
    ;; a french misinterpretation of english pronunciation
    ;;(case (string-ref (symbol->string name) 0)
    ;;  ((#\a #\e #\i #\o #\u #\y) (write-char #\n stream))
    ;;  (else #f) )
    (write-char #\space stream)
    (display name stream)
    (display ">" stream)
    o ) )

(define-method (show (o Class) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Class: " stream)
    (display (Class-name o) stream)
    (display ">" stream)
    o ) )

(define-method (show (o Generic) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<Generic: " stream)
    (display (Generic-name o) stream)
    (display ">" stream)
    o ) )

;;; A * or = follows the word Field to indicate if it is a mono or
;;; a Poly-Field. A ! is also inserted if the field is mutable.
(let ((displayer (lambda (status o stream)
                   (display "#<Field" stream)
                   (display status stream)
                   (when (Mutable-Field? o) (display "!"))
                   (display (Field-name o) stream)
                   (display ">" stream)
                   o )))
  (define-method (show (o Mono-Field) . stream)
    (let ((stream (if (pair? stream) (car stream) (current-output-port))))
      (displayer "=" o stream) ) )
  (define-method (show (o Poly-Field) . stream)
    (let ((stream (if (pair? stream) (car stream) (current-output-port))))
      (displayer "*" o stream) ) )
  'show )

(define-method (show (o Anomaly) . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (display "#<" stream)
    (if (memq (Anomaly-intention o) 
              '(Domain-Anomaly Syntax-Anomaly Allocation-Anomaly) )
        (display (Anomaly-intention o) stream)
        (display "Anomaly" stream) )
    (display "(in:" stream)
     (if (memq (Anomaly-intention o) 
              '(Domain-Anomaly Syntax-Anomaly Allocation-Anomaly) )
         (display (Anomaly-culprits o 0) stream)
         (display (Anomaly-intention o) stream) )
    (display "):" stream)
    (display (Anomaly-message o) stream)
    (display ">" stream) ) )
    
;;;======================================================================= copy
;;; This generic function returns a copy of any Meroon object. 
;;; This is only a shallow (not recursive) copy.

(define-generic (clone (o)))

(define-method (clone (o Object))
  (let* ((n (vector-length o))
         (r (make-vector n)) )
    (vector-copy! o r 0 n)
    r ) )

;;;============================================ New library of useful functions
;;; This function displays the inheritance tree of classes.
;;; Display all classes without arguments.
(define (show-hierarchy . args)
  (let* ((arg (if (pair? args) (car args) 'Object))
         (class (cond ((symbol? arg) (symbol->class arg))
                      ((Class? arg) arg)
                      (else (meroon-error 
                             (create-Domain-Anomaly
                              'show-hierarchy
                              "Not coercible to Class" arg ) )) ))
         (stream (if (pair? args) (cdr args) '()))
         (stream (if (pair? stream) (car stream) (current-output-port))) )
    (define (show-class c indent)
      (do ((i 0 (+ 1 i)))
          ((>= i indent))
        (display " " stream) )
      (show c stream)
      (newline stream)
      (for-each (lambda (c) (show-class (number->class c) (+ indent 1)))
                (Class-subclasses c) ) )
    (display "Subclass tree of " stream)
    (display (Class-name class) stream)
    (newline stream)
    (show-class class 0)
    #t ) )

;;; Show all different methods attached to a generic function.
;;; Display all generic functions without arguments.
(define (show-generic . args)
  (if (pair? args)
      (oo-apply show-generic-function args)
      (vector-map (lambda (generic)
                    (when generic (show-generic-function generic)) )
                  *generics* ) ) )

(define (show-generic-function name . stream)
  (let* ((stream (if (pair? stream) (car stream) (current-output-port)))
         (generic
          (cond ((symbol? name) (symbol->generic name))
                ((Generic? name) name)
                (else (meroon-error 
                       (create-Domain-Anomaly
                        'show-generic
                        "Not coercible to Generic"
                        name ) )) ) )
         (name (Generic-name generic))
         (dispatch-table (Generic-dispatch-table generic)) )
    (define (show-method c super-method indent)
      (let ((current-method (vector-ref dispatch-table (Class-number c))))
        (unless (eq? super-method current-method)
          (do ((i 0 (+ 1 i)))
              ((>= i indent))
            (display " " stream) )
          (show c stream)
          (newline stream) )
        (for-each (lambda (c) (show-method (number->class c)
                                           current-method
                                           (+ indent 1) ))
                  (Class-subclasses c) ) ) )
    (display "Methods on " stream)
    (display name stream)
    (newline stream)
    (show-method (symbol->class 'Object) 
                 (Generic-default generic)
                 0 )
    #t ) )

;;; Displays some figures on Meroon
(define meroon-version
  "Meroon $Revision: 2.22 $ of $Date: 1992/11/12 18:37:32 $." )

(define (show-meroon . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (newline stream)
    (display meroon-version stream)
    (newline stream)
    (display "Total number of classes: " stream)
    (display *class-number* stream)
    (newline stream)
    (display "Total number of generic functions: " stream)
    (display *generic-number* stream)
    (newline stream)
    #t ) )

;;;==================================================================== Tracing
;;; This function installs two functions named before and after 
;;; to advise the use of a generic instance. Before is called on 
;;; the input arguments while after is invoked on the result.

(define (generic-trace generic before after)
  (cond ((Generic? generic) #t)
        ((symbol? generic) (set! generic (symbol->generic generic)))
        (else (meroon-error 
               (create-Domain-Anomaly
                'generic-trace "Not coercible to Generic" generic ) )) )
  (unless (procedure? before)
          (meroon-error 
           (create-Domain-Anomaly
            'generic-trace "Not a procedure" before ) ) )
  (unless (procedure? after)
          (meroon-error 
           (create-Domain-Anomaly
            'generic-trace "Not a procedure" after ) ) )
  (let* ((couple (assq (Generic-name generic) *traced-generics*))
         (traced (or couple
                     (list (Generic-name generic)
                           (Generic-discriminator generic) ) ))
         (old-discriminator (cadr traced)) )
    ;; A function is hooked with the last hooks, previous are lost
    (set-Generic-discriminator!
     generic
     (lambda args
       (oo-apply before args)
       (after (oo-apply old-discriminator args)) ) )
    (unless couple
            (set! *traced-generics* (cons traced *traced-generics*)) )
    generic ) )

;;; remove the tracing functions associated to a generic instance.
(define (generic-untrace generic)
  (unless (Generic? generic)
          (meroon-error 
           (create-Domain-Anomaly
            'generic-untrace "Not a Generic object" generic ) ) )
  (let ((name (Generic-name generic))
        (head (cons 1936 *traced-generics*)) )
    (define (look l)
      (cond ((null? (cdr l)) #t)
            ((eq? name (car (cadr l)))
             (set-Generic-discriminator!
              generic (cadr (cadr l)) )
             (set-cdr! l (cddr l)) )
            (else (look (cdr l))) ) )
    (look head)
    (set! *traced-generics* (cdr head)) )
  generic )

;;;========================================================== Enhanced tracing
;;; A better tracing facility with meaningful defaults. Arguments are
;;; displayed with the generic function show. It is actually coded with
;;; generic-trace which is the lower level instrumenting facility.
(define (show-generic-trace . names)
  (for-each (lambda (name)
              (generic-trace 
               (cond ((symbol? name) (symbol->generic name))
                     ((Generic? name) name)
                     (else (meroon-error 
                            (create-Domain-Anomaly
                             'show-generic-trace
                             "Not coercible to Generic" name ) )) )
               (lambda args
                 (show name)
                 (display "<<")
                 (for-each (lambda (arg) 
                             (display " ")
                             (show arg) )
                           args )
                 (newline) )
               (lambda (result)
                 (show name)
                 (display ">> ")
                 (show result)
                 (newline)
                 result ) ) )
            names ) )

;;; (show-generic-untrace) untraces all traced generic.
(define (show-generic-untrace . names)
  (for-each (lambda (o)
              (generic-untrace 
               (cond ((symbol? o) (symbol->generic o))
                     ((Generic? o) o)
                     (else (meroon-error 
                            (create-Domain-Anomaly
                             'show-generic-untrace
                             "Not coercible to Generic" o ) )) ) ) )
            (if (pair? names) names
                *traced-generics* ) ) )

;;;==================================================================== Errors
;;; Anomalies are created in a special classes. 

(define-class Syntax-Anomaly Anomaly ())
(define-class Domain-Anomaly Anomaly ())
(define-class Allocation-Anomaly Anomaly ())

;;;  function is a generic object, a field or a symbol that 
;;; names a regular function.
(define (create-Domain-Anomaly function msg . all-arguments)
  (oo-apply make-Domain-Anomaly function msg 
         (length all-arguments) all-arguments ) )

(define (create-Syntax-Anomaly definer-name msg . arguments)
  (oo-apply make-Syntax-Anomaly definer-name msg 
         (length arguments) arguments ) )

(define (create-Allocation-Anomaly act msg . arguments)
  (oo-apply make-Allocation-Anomaly act msg
         (length arguments) arguments ) )

(define (create-Anomaly function msg . arguments)
  (oo-apply make-Anomaly function msg 
         (length arguments) arguments ) )

;;; Anomalies are reported with a generic function so users can add
;;; their proper errors. Oo-error is a special function defined in the
;;; various prologues that calls the underlying error system.
(define-generic (meroon-error (Anomaly))
  (oo-error 'meroon "Error" Anomaly) )

(define-method (meroon-error (o Anomaly))
  (set! *last-meroon-anomaly* o) ;qnc
  (oo-error (Anomaly-intention o) (Anomaly-message o) o) )

(let ((displayer (lambda (o type occur reason)
                   (newline)
                   (display "******************* Meroon ")(display type)
                   (display " Anomaly *********************")(newline)
                   (display "Occurred in: ")(show occur)(newline)
                   (display "Reason: ")(show reason)(newline)
                   (do ((i 0 (+ 1 i)))
                       ((= i (Anomaly-culprits-length o)) #f)
                     (display "Culprit#")(display i)(display ": ")
                     (show (Anomaly-culprits o i))
                     (newline) ) )))
  (define-method (meroon-error (o Syntax-Anomaly))
    (displayer o 'syntax (Anomaly-intention o) (Anomaly-message o))
    (call-next-method) )
  (define-method (meroon-error (o Domain-Anomaly))
    (if (Field? (Anomaly-intention o))
        (displayer o 'access (Anomaly-intention o) (Anomaly-message o))
        (displayer o 'domain (Anomaly-intention o) (Anomaly-message o)) )
    (call-next-method) )
  (define-method (meroon-error (o Allocation-Anomaly))
    (displayer o 'allocation (Anomaly-intention o) (Anomaly-message o))
    (call-next-method) ) )

;;;============================================================ Unveiling
;;; Display details of objects

;;; The list of already seen objects to detect cycles
(define *meroon-already-seen-objects* '())

;;; The entry point:
(define (unveil o . stream)
  (let ((stream (if (pair? stream) (car stream) (current-output-port))))
    (set! *meroon-already-seen-objects* '())
    (show-unveiled o 0 stream)
    (set! *meroon-already-seen-objects* '()) ; GC: dont keep this !
    #t ) )

;;; Pretty print O, INDENT is the current indentation.
(define-generic (show-unveiled (o) indent stream)
  (show o stream) )

;;; Newline, indent. Also mark vertical bars to explicit alignment.
(define (goto-margin indent stream)
  (define step 2)
  (define bigstep (* 3 step))
  (newline stream)
  (do ((i 0 (+ 1 i)))
      ((= i indent) #f)
    (write-char (cond ((= (- bigstep 1) (modulo i bigstep)) #\*)
                      ((= (- step 1) (modulo i step)) #\|)
                      (else #\space) )
                stream ) ) )

;;; The generic way to display Scode expression (or more generally 
;;; any acyclic Meroon objects).
;;; Display O with INDENT on STREAM. FUNCTIONS specify the functions that
;;; must be used to display the fields of O, by default show-unveiled is used.
(define (generic-show-unveiled o indent stream functions)
  (let* ((class (object->class o))
         (already-seen (memq o *meroon-already-seen-objects*))
         (index (if (pair? already-seen)
                    (length already-seen)
                    (begin (set! *meroon-already-seen-objects*
                                 (cons o *meroon-already-seen-objects*) )
                           (length *meroon-already-seen-objects*) ) )) )
    (if already-seen
        (begin 
          ;;(format stream "<the ~A referred above as ~A>"
          ;;        (Class-name class) index )
          (display "<the " stream)
          (display (Class-name class) stream)
          (display " referred above as " stream)
          (display index stream)
          (display ">" stream) )
        (begin
          (goto-margin indent stream)
          ;;(format stream "(~A <------------- [Id: ~A]"
          ;;        (Class-name class) index )
          (display "(a " stream)
          (display (Class-name class) stream)
          (display " <------------- [Id: " stream)
          (display index stream)
          (display "]" stream)
          (let ((indent (+ 1 indent)))
            (generic-show-unveiled-field-content
             o indent stream functions (Class-fields class) ) ) 
          ;;(format stream " end ~A)" (Class-name class))
          (display " end " stream)
          (display (Class-name class) stream)
          (display ")" stream) ) ) ) )

;;; Display the content of a field
(define (generic-show-unveiled-field-content o indent stream functions fields)
  (define (enum functions fields)
    (when (pair? fields)
          ;;(format stream "~A: " (Field-name (car fields)))
          (goto-margin indent stream)
          (cond                ;; should be generic FUTURE
           ((pair? functions)
            (display (Field-name (car fields)) stream)
            (display ": " stream)
            ((car functions) ((Field-reader (car fields)) o) indent stream) )
           ((Mono-Field? (car fields))
            (display (Field-name (car fields)) stream)
            (display ": " stream)
            (show-unveiled ((Field-reader (car fields)) o) indent stream) )
           ;; should be generic  FUTURE
           ((Poly-Field? (car fields)) 
            (let loop ((n ((Poly-Field-lengther (car fields)) o))
                       (i 0) )
              (when (< i n)
                    (display (Field-name (car fields)) stream)
                    (display "[" stream)
                    (display i stream)
                    (display "]: " stream)
                    (show-unveiled ((Field-reader (car fields)) o i)
                                   indent stream )
                    (goto-margin indent stream)
                    (loop n (+ 1 i)) ) ) ) )
           (enum (if (pair? functions) (cdr functions) '())
                (cdr fields) ) ) )
  (enum functions fields) )

;;; The fundamental method is to call generic-show-unveiled.
(define-method (show-unveiled (o Object) indent stream)
  (generic-show-unveiled o indent stream '()) )

;;;============================================================ Personal notes
;;; pourquoi pas faire une fonction coerce ou as (regarder Dylan et OakLisp) ?
;;; ameliorer par des schemas particuliers les generiques a peu de methodes.
;;; faire table de messages d'erreur avec objet Anomaly.
;;; donner des flottants pour les numeros de classes ?

;;; End of Meroon Code.
