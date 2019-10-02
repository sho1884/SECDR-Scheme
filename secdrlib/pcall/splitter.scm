;;; $Id: splitter.scm,v 1.3 1992/01/15 12:00:30 queinnec Exp $

;;;                 The definite version of splitter
;;;            by Bernard Serpette and Christian Queinnec
;;;            Ecole Polytechnique and INRIA-Rocquencourt.

;;; This version comes from our POPL91 paper and mimics an implementation.
;;; The current variant offers the splitter operator with the following
;;; signatures:
;;;      (splitter (lambda (mark) ...))
;;;      (abort mark (lambda () ...))
;;;      (call/pc mark (lambda (partial-cont) ...))
;;;      (partial-cont value)
;;;      (in-dynamic-extent? mark)

;;; The splitter operator is explained in the following reference:
;;; @InProceedings{Queinnec91a,
;;;   author =       "Christian Queinnec and Bernard Serpette",
;;;   title =        "{A} {D}ynamic {E}xtent {C}ontrol {O}perator for
;;;                   {P}artial {C}ontinuations",
;;;   booktitle =    popl91,
;;;   year =         1991,
;;;   pages =        "174--184",
;;;   address =      "Orlando (Florida USA)",
;;;   month =        jan,
;;;   ecritsdIcsla = 5,
;;;   abstract = "
;;; A partial continuation is a prefix of the computation that remains to
;;; be done. We propose in this paper a new operator
;;; which precisely controls which prefix is to be abstracted into a partial
;;; continuation. This operator is strongly related to the notion of
;;; dynamic extent which we denotationally characterize.
;;; Some programming examples are commented and
;;; we also show how to express previously proposed control operators.
;;; A suggested implementation is eventually discussed."
;;; }

;;; Exported functions are: splitter, abort, call/pc and in-dynamic-extent?

(define stack-of-marker '())

(define (splitter f)
  (let ((marker '()) 
        (v '()) 
        (topmarker '()) )
     (set! v
       (call/cc
        (lambda (kk)
          (set! marker (cons kk '()))
          (set! stack-of-marker 
              (cons marker stack-of-marker))
          (let ((v (f marker)))
            (set-car! (car stack-of-marker)
                      '() )
            v ) ) ) )
     (if (not (null? (caar stack-of-marker)))
        ;; Someone did (kk thunk)
         (begin
          ;; markers downto marker are obsolete.
          (obsolete-stack! marker)
          ;; Compute thunk with continuation kk.
          (set! v (v))
          (set-car! (car stack-of-marker)
                    '() ) ))
     (set! topmarker (car stack-of-marker))
     (set! stack-of-marker 
           (cdr stack-of-marker) )
     (cond
      ;; The continuation is 'return'
      ((null? (cdr topmarker)) v)
      ;; end of a partial continuation.
      (else ((cdr topmarker) v)) ) ) )

;;; Answers if the mark is valid ie if we are 
;;; in the dynamic extent of the mark.

(define (in-dynamic-extent? mark)
  (not (null? (car mark))) )

(define (abort marker thunk)
  (if (null? (car marker))
      (wrong "obsolete splitter")
      ;; Return to the marker.
      ((car marker) thunk) ) )

(define (call/pc marker g)
  (if (null? (car marker))
      (wrong "out of extent")
      (call/cc
       (lambda (kj)
         (g (make-pc kj marker)) ) ) ) )

(define (make-pc kj marker)
  (let ((slice (marker-prefix 
                stack-of-marker 
                marker )))
    (lambda (v)
      (call/cc
       (lambda (kc)
         (set! stack-of-marker
           (append slice
                   (cons (cons #t kc)
                         stack-of-marker ) ) )
         (kj v) ) ) ) ) )

(define (marker-prefix l m)
  (if (eq? (car l) m)
      '()
      (cons (cons #t (cdar l))
            (marker-prefix (cdr l) m) )))

(define (obsolete-stack! marker)
  (if (eq? (car stack-of-marker) marker)
    marker
   (begin (set-car! (car stack-of-marker)
                    '() )
          (set! stack-of-marker 
                (cdr stack-of-marker) )
          (obsolete-stack! marker) ) ) )  
