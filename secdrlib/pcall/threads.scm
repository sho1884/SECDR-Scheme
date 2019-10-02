;;; $Id: threads.scm,v 1.6 1992/01/15 11:39:31 queinnec Exp $

;;;:::::::::::::::::::::::::::::::::::::::::::::::::::
;;;             Multi-Threads System
;;;
;;; Christian Queinnec queinnec@poly.polytechnique.fr
;;; \'Ecole Polytechnique \& INRIA--Rocquencourt
;;; 91128 Palaiseau Cedex -- France
;;;:::::::::::::::::::::::::::::::::::::::::::::::::::

;;; This file defines some functions that allow to simulate pcall
;;; and futures on a regular Scheme inplementation. 
;;; It has been tested on MacScheme 1.5 and Scheme->C 01nov91jfb.
;;; It heavily uses continuations with indefinite extent.
;;; Future use pcall and call/cc. Pcall must be understood
;;; as a new special form which semantics is described in the paper:
;;;@InProceedings{Queinnec91b,
;;;  author =       "Christian Queinnec",
;;;  title =        "{C}rystal {S}cheme, {A} {L}anguage for {M}assivel
;;;                  {P}arallel {M}achines",
;;;  editor =       "M Durand and F {El Dabaghi}",
;;;  booktitle =    "Second Symposium on High Performance Computing",
;;;  year =         1991,
;;;  pages =        "91-102",
;;;  publisher =    north-holland,
;;;  address =      "Montpellier (France)",
;;;  month =        oct,
;;;  abstract = "
;;;Massively parallel computers are built out of thousand conventional
;;;but powerful processors with independent memories. Very simple
;;;topologies mainly based on physical neighbourhood link these
;;;processors. The paper discusses extensions to the Scheme language in
;;;order to master such machines. Allowing arguments of functions to be
;;;concurrently evaluated introduces parallelism. Migration across the
;;;different processors is achieved through a remote evaluation
;;;mechanism. We propose a clean semantics for first class continuations
;;;in presence of concurrency and show how they can be used to build,
;;;in the language itself, advanced concurrent constructs such as futures.
;;;Eventually we comment some simulations, with various topologies and
;;;migration policies, which enables to appreciate our previous
;;;linguistical choices and confirms the viability of the model."
;;;}

;;; THEORY OF OPERATION
;;; In order to use pcall and other functions controlling threads,
;;; you must wrap your computation in a thunk and invoke start-multithreads
;;; on it as in:
;;;     (start-multithreads (lambda ()
;;;                            ... computation with pcall or futures ... ))
;;; Pcall and future spawn threads. Threads gain control when calls to
;;; pcall or future occur: the underlying scheduler is non-preemptive. 

;;; PORTABILITY NOTE: 
;;; -- Pcall is a macro defined with extend-syntax.
;;; -- reverse! is a physical reverse.

;;; LOW-LEVEL FUNCTIONS
;;; The system is defined with only four low level functions:
;;; blackhole, spawn-threads, resume-scheduler and start-multithreads.

;;; (blackhole . arguments) kills the current thread
;;; and thus has no value at all.

(define blackhole 'defined-below)

;;; (spawn-threads list-of-thunks) spawns new threads. Threads are
;;; represented by continuations or unary functions that will finally
;;; call the scheduler ie that calls blackhole or spawn-threads.
;;; The value returned by spawn-threads is undefined. 

(define spawn-threads 'defined-below)

;;; (resume-scheduler value) is an internal function normally not to be used.
;;; It returns the value as a new value for start-multithreads.
;;; The returned value is undefined.

(define resume-scheduler 'defined-below)

;;; Start a new thread system. This function must be invoked to initialize
;;; the two previous functions since it diddles with continuations.
;;; It must be called only once since multiple invokations create
;;; multiple threads systems that clash on the two previous functions.
;;; start-multithreads return the list of all the results that were
;;; returned to the continuation of start-multithreads. There can
;;; be more than once in Scheme if you consider for instance:
;;; (start-multithreads (lambda () (call/cc (lambda (k) (pcall (k 1) (k 2))))))
;;; this returns the list (1 2) or (2 1) depending on the scheduling order.

(define (start-multithreads thunk)
  (call/cc 
   (lambda (exit)         
     (letrec ((queue '())        ; The queue of eligible threads
              (results '())      ; the list of results to give to exit
              (resume-toplevel   ; the scheduler itself
               (lambda (val)
                 (set! results (cons val results))
                 (resume-scheduler val) ) ) )
       (set! resume-scheduler
             (lambda (val)
               (if (pair? queue)
                   (oneof queue
                          (lambda (thread others)
                            (set! queue others)
                            (resume-scheduler (thread 'go)) ) )
                   (exit results) ) ) )
       (set! spawn-threads
             (lambda (threads)
               (set! queue (append threads queue)) ) )
       (set! blackhole 
             (lambda values
               (resume-scheduler 'blackhole) ) )
       (resume-toplevel (thunk)) ) )) )

;;; The alea is contained in the oneof function
;;; oneof selects one of the z* and calls q on the selected item and the
;;; rest of all the left items. The `remove' local function can be removed
;;; if your system offers it.
;;; You can improve oneof with a real alea but the following works well 
;;; enough: just take the 3rd item of the list.

(define (oneof z* q)
  (define (remove item list)
    (if (eq? item (car list))
        (cdr list)
        (cons (car list) (remove item (cdr list))) ) )
  (define (loop n threads)
    (if (pair? threads)
        (if (> n 0)
            (loop (- n 1) (cdr threads))
            (q (car threads) (remove (car threads) z*)) )
        (loop n z*) ) )
  (loop 3 z*) )


;;; HIGH LEVEL FEATURES: 
;;; blackhole (already seen) and pcall.

;;; (pcall fun . arguments) compute concurrently the terms of the
;;; application and apply it. The semantics of the continuation
;;; of the arguments is somewhat special, see the paper referenced above.

(extend-syntax (pcall)
     ((pcall form ...)
      (spawn-pcall (lambda () form) ...) ) )

;;; This predicate checks if an argument is already computed or not.
;;; The function allocates the frame necessary for pcall.
;;; This frame contains at index:
;;;    0 the list of patches to further apply on the frame
;;;    1 the continuation of the pcall form
;;;    2 the count of remaining arguments that are waited for (this
;;;      improves the speed of already-computed-frame?
;;; 3... the values of the terms of the application (initialized to undef).

(define already-computed-argument? 'wait)
(define make-pcall-frame 'wait)

(let ((undef (list "undef")))
  (set! already-computed-argument? 
        (lambda (frame index) 
          (not (eq? (vector-ref frame (+ 3 index)) undef))) )
  (set! make-pcall-frame
        (lambda (nargs)
          (let ((v (make-vector (+ nargs 3) undef)))
            (vector-set! v 0 '())
            (vector-set! v 2 nargs)
            v ) ) )
  #f )

;;; This function tests if a frame is ready for application ie if all
;;; terms have at least returned one value.

(define (pcall-frame-ready? frame)
  (= (vector-ref frame 2) 0) )

;;; This function stores a value in a pcall-frame. It serves as
;;; the continuation of all the arguments of a pcall form.

(define (store-value-in-pcall-frame frame index value)
  (if (already-computed-argument? frame index)
      (if (pcall-frame-ready? frame)
          ;; store in place, previous information is already used.
          (vector-set! frame (+ 3 index) value)
          ;; just record the patch to be done later
          (vector-set! frame 0 (cons (cons index value) 
                                     (vector-ref frame 0) )) )
      ;; store the argument and update the counter
      (begin (vector-set! frame (+ 3 index) value)
             (vector-set! frame 2 (- (vector-ref frame 2) 1)) ) )
  (when (pcall-frame-ready? frame)
        (perform-applications frame) ) )

;;; Performs the needed applications present in pcall-frame
;;; Assume the pcall-frame is ready.

(define (perform-applications frame)
  (define (spawn-application)
    (do ((i (- (vector-length frame) 1) (- i 1))
         (r '()) )
        ((= i 2) (spawn-threads
                  (list
                   (lambda (go) ((vector-ref frame 1)
                                 (apply (car r) (cdr r)) )) ) ))
        (set! r (cons (vector-ref frame i) r)) ) )
  ;; spawn the application already present
  (spawn-application)
  ;; and for each recorded updates, spawn the corresponding application
  (when (pair? (vector-ref frame 0))
        (let ((updates (vector-ref frame 0)))
          (vector-set! frame 0 '())
          (for-each 
           (lambda (index+value)
             (vector-set! frame (car index+value) (cdr index+value))
             (spawn-application) )
           (reverse! updates) ) ) ) )
             
;;; spawn-pcall represents the run-time of the pcall macro.    

(define (spawn-pcall . thunks)
  (let ((frame (make-pcall-frame (length thunks))))
    (call/cc (lambda (k)
               (vector-set! frame 1 k)
               (spawn-threads
                (map (lambda (thunk index)
                       (lambda (go)
                         (store-value-in-pcall-frame 
                          frame index (thunk) ) ) )
                     thunks 
                     (iota 0 (length thunks)) ) ) 
               (blackhole) )) ) )

;;; generate the list of integers [base .. limit[

(define (iota base limit)
  (if (< base limit)
      (cons base (iota (+ 1 base) limit))
      '() ) )
