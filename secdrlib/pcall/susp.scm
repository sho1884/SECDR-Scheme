;;; $Id: susp.scm,v 1.6 1992/01/15 11:38:52 queinnec Exp $

;;;:::::::::::::::::::::::::::::::::::::::::::::::::::
;;;             (Composable) futures
;;;    (after an idea from Vincent Poirriez)
;;;
;;; Christian Queinnec queinnec@poly.polytechnique.fr
;;; \'Ecole Polytechnique \& INRIA--Rocquencourt
;;; 91128 Palaiseau Cedex -- France
;;;:::::::::::::::::::::::::::::::::::::::::::::::::::

;;; This file defines futures. Futures can be defined by means of 
;;; call/cc and pcall (see the paper referenced in threads.scm).
;;; Moreover these futures can be `pursued' without being touched
;;; The difference between a touch and a pursue is that 
;;; touch makes the caller waits where pursue makes the caller gets a
;;; suspension which will be determined when the future will be determined.
;;; A companion paper on pursuing suspensions is in preparation. 

;;; Since the scheduler is non preemptive and only one thread is running,
;;; there is no race condition between them and shared variables do not
;;; need to be protected.

;;; PORTABILITY NOTE
;;; -- Future is a macro defined with extend-syntax.
;;; -- Errneous situations call the `wrong' function which is not defined.

;;; To ease debug, all `objects' (boxes and suspensions) are numbered.
;;; This serial number can bbe retrieved with the message `id'.
(define *serial-counter* 0)

;;; DATA STRUCTURES
;;; A place is a place-holder for a value, it is thus a 1-place buffer
;;; or a M-structure. A place contains a list of threads 
;;; waiting for this value as well as a list of waiting suspensions. Both
;;; will be awaked when the place gets a value ie will be 
;;; scheduled to run. The place can be `pursued' into a suspension.

(define (make-place)
  (letrec 
      ((value 'wait)                    ; The value
       (set? #f)                        ; Is the value present ?
       (dependent* '())                 ; The list of waiting threads
       (suspension* '())                ; the list of dependent suspensions
       (serial-number (begin (set! *serial-counter* (+ 1 *serial-counter*))
                             *serial-counter* ))
       (place (lambda (msg . args)        ; the place itself
              (case msg  
                ((show)                 ; DEBUG
                 `(place ,serial-number
                       set? = ,set?
                       suspension* = ,(map (lambda (susp) (susp 'id))
                                           suspension* )
                       dependent* = ,dependent*
                       value = ,value ) )
                ((id) serial-number)    ; DEBUG
                ((valued?) set?)
                ((set!)
                 (when set? 
                       (wrong "A place cannot be assigned more than once!" 
                              place ) )
                 (set! value (car args))
                 (set! set? #t)
                 ;; awake depending threads and suspensions
                 (let ((k* dependent*)
                       (s* suspension*) )
                   (set! dependent* '())
                   (spawn-threads 
                    (map (lambda (k) (lambda (go) (k value)))
                         k* ) )
                   (set! suspension* '())
                   (spawn-threads
                    (map (lambda (susp) (lambda (go) (susp 'compute!)))
                         s* ) ) ) )
                ((ref)
                 (if set?
                     value
                     (call/cc (lambda (k) ; suspends the caller
                                (set! dependent* (cons k dependent*))
                                (blackhole) )) ) )
                ((pursue)
                 (if set? 
                     ((car args) value)
                     (let ((susp (make-suspension (car args) place)))
                       (set! suspension* (cons susp suspension*))
                       susp ) ) )
                (else (wrong "Does not understand" msg place)) ) )) )
    place ) )

;;; A suspension contains a function that will be applied on the value 
;;; of the related place (or suspension) on which it depends. The function 
;;; is only applied once after that its value is cached in the suspension.
;;; The function will only be applied if the suspension is touched or
;;; pursued. Note that the related place does not refer to the suspension so
;;; suspensions that are no more referenced can disappear.

(define (make-suspension f place-or-susp)
  (letrec 
      ((value 'wait)                    ; the value
       (set? #f)                        ; is the value present ?
       (dependent* '())                 ; the other waiting threads
       (suspension* '())                ; the list of dependent suspensions
       (serial-number (begin (set! *serial-counter* (+ 1 *serial-counter*))
                             *serial-counter* ))
       (susp (lambda (msg . args)       ; the suspension itself
               (case msg
                 ((show)                ; DEBUG
                  `(suspension ,serial-number
                               depends-on = ,(place-or-susp 'id)
                               set? = ,set?
                               suspension* = ,(map (lambda (susp) (susp 'id))
                                                   suspension* )
                               dependent* = ,dependent*
                               value = ,value ) )
                 ((id) serial-number)   ; DEBUG
                 ((compute!)
                  (set! value (f (place-or-susp 'ref)))
                  (set! set? #t)
                  ;; awake depending threads and suspensions
                  (let ((k* dependent*)
                        (s* suspension*) )
                    (set! dependent* '())
                    (spawn-threads 
                     (map (lambda (k) (lambda (go) (k value)))
                          k* ) )
                    (set! suspension* '())
                    (spawn-threads
                     (map (lambda (susp) (lambda (go) (susp 'compute!)))
                          s* ) ) ) )
                 ((ref)
                  (if set?
                      value
                      (call/cc (lambda (k) ; suspends the caller
                                 (set! dependent* (cons k dependent*))
                                 (blackhole) )) ) )
                 ((pursue)
                  (if set? 
                      ((car args) value)
                      (let ((new-susp (make-suspension (car args) susp)))
                        (set! suspension* (cons new-susp suspension*))
                        new-susp ) ) )
                 (else (wrong "Does not understand" msg susp)) ) )) )
    susp ) )

;;; Besides creation, three operations are provided:
;;; touch makes the current thread obtain the value of a place (or suspension)
;;;       otherwise it is paused until the place (or suspension) gets a value.
;;; pursue composes a place (or a suspension) with a function
;;; assign! sets the value of a place and unfreezes all the threads
;;;          waiting for this value.
;;; All these operations force the queue of eligible threads to be emptied.

(define (touch place-or-susp)
  (place-or-susp 'ref) )

(define (pursue place-or-susp f)
  (place-or-susp 'pursue f) )

(define (assign! place value)
  (place 'set! value) )
;;; The thread system ensures that the new spawned threads will be run.


;;; REMARK
; How can be compared delays, futures and places [find another name] ?
; -- Delays delay computations until force'd. When forced they are only
; evaluated once. Delays are computed by need.
; -- Futures span new threads to compute. When touch'ed
; futures block the caller. When a future is determined all waiting 
; threads are made eligible. Futures are computed sepculatively.
; -- Places are not associated to computations, there are just place-holders
; for values. They suspend callers when they are still empty, they resume
; them when the value is present. Computation is done when possible (like
; data-flow machines).

(extend-syntax (future)
   ((future form) (create-future (lambda () form))) )

(define (create-future thunk)
  (let ((the-future (make-place)) )
    ;; spawn a new process to fill in the place
    (spawn-threads (list (lambda (restart)
                           (assign! the-future (thunk))
                           (blackhole) )))
    ;; and return the place
    the-future ) )

;;; Delay is called freeze not to clash with the usual delay of Scheme.
(extend-syntax (freeze)
  ((freeze form) (create-frozen (lambda () form))) )

(define (create-frozen thunk)
  (let ((set? #f)
        (place (make-place)) )
    (lambda (msg . args)
      (case msg 
        ;; modify the REF message to force the computation (once)
        ((ref)
         (unless set? (assign! place (thunk))
                 (set! set? #t) )
         (touch place) )
        ((show) `(freeze set? = ,set? . ,(place 'show))) ; DEBUG
        ;; Cannot do other thing on a delay
        (else (wrong "Does not understand" msg place)) ) ) ) )

(define (unfreeze promise)
  (touch promise) )


