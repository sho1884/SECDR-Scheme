;;; Article: 2218 of comp.lang.scheme
;;; From: bevan@cs.man.ac.uk (Stephen J Bevan)
;;; Newsgroups: comp.lang.scheme
;;; Subject: Results of my question regarding continuation passing
;;; Message-ID: <BEVAN.91Oct3093716@hippo.cs.man.ac.uk>
;;; Date: 3 Oct 91 08:37:16 GMT
;;; Sender: news@cs.man.ac.uk
;;; Distribution: comp
;;; Organization: Department of Computer Science, University of Manchester
;;; Lines: 162

;;; First of all, thanks to all the people who replied to my previous
;;; request for help with writing continuation passing code.  After taking
;;; some of the advice on board, I think I finally managed to write in the
;;; style I was asking about.

;;; To give you an idea of what I was after, I've included some code at
;;; the end of this post that implements Haskell like enumFrom and
;;; enumFromTo with some examples of how to use them.

;;; The code starts of in the correct vein i.e. explicit continuations and
;;; no explicit recursion%.  However, it quickly degrades into using
;;; explicit recursion as I couldn't handle removing from all functions
;;; (my brain couldn't cope :-(

;;; Anyway, after struggling with this for days, I've now had my fill and
;;; I'm going back to Haskell (well Gofer actually).


;;; Stephen J. Bevan				bevan@cs.man.ac.uk


;;; % By `no explicit recursion' I mean that any named objects cannot
;;;   refer to themselves in their own definition i.e. you can only use
;;;   `let' to define function rather than `letrec' and `define' must be
;;;   used as a `let' rather than a `letrec'. 

;;;; -*- Scheme -*-
;;;; $Id$

;;; An attempt at providing an equivalent to Haskell's lazy lists.
;;; The idea is not to implement lazy lists, rather to provide the
;;; same functionality using generators or producer/consumers if you
;;; prefer.
;;;
;;; The idea is that you write producer functions that kick out values
;;; together with a producer that will produce the next value.
;;; Consumer functions then operate on the value and apply the
;;; producer to pump the next value.
;;;
;;; This is all fairly straightforward.  The slightly different
;;; approach I've taken here is to use more continuations that you
;;; would otherwise use in an attempt to get a handle on all parts of
;;; the computation.  By throwing in more continuations, it is
;;; possible to do unusual things like change the producer in mid
;;; stream.

;;;+fs
;;; A `producer' that generates all the values from `value' upwards to
;;; inifinity.  This achieves the same effect as Haskell's enumFrom.
;;;
;;; `cons-succ'
;;;    A `consumer' that should be applied to each value.
;;;    (lambda (value producer) ...)
;;;
;;; `cons-fail'
;;;    A `consumer' that is applied when the end of the sequence is
;;;    reached.  This is a dummy value as this producer never runs out
;;;    of values.  It is only here for compatibility with other
;;;    producers.
;;;-fs
;;; Note this does not use named self recursion.  The necessary
;;; recursion is a achieved by passing continuations.

(define enum-from
  (lambda (value cons-succ cons-fail)
    (let
	((producer
	  (lambda (value cons-succ cons-fail next-producer)
	    (cons-succ
	     value
	     (lambda (new-cons-succ new-cons-fail)
	       (next-producer 
		  (+ 1 value)
		  new-cons-succ
		  new-cons-fail
		  next-producer))))))
      (producer value cons-succ cons-fail producer))))


;;;+fs
;;; A `producer' that generates all the values from `lower-bound'
;;; upwards to `upper-bound'.  This achieves the same effect as
;;; Haskell's enumFromTo.
;;;
;;; `cons-succ'
;;;    A `consumer' that should be applied to each value.
;;;    (lambda (value producer) ...)
;;;
;;; `cons-fail'
;;;    A `consumer' that is applied after `upper-bound' has been
;;;    reached.  Its format should be :-
;;;
;;;      (lambda (producer) ...)
;;;
;;;    It is passed the `producer' that generated the values in the
;;;    hope that is might be useful.  Most of the time it is expected
;;;    that the producer will simply be ignored.
;;;-fs
;;; Note this does use named self recursion in the definition of
;;; `consumer' i.e. consumer is defined via letrec rather than let.
;;; There is no reason to other than I haven't got round to taking it out
;;; yet. 
;;;
(define enum-from-to
  (lambda (lower-bound upper-bound consumer-success consumer-failure)
    (letrec
	((consumer
	  (lambda (consumer-success consumer-failure value producer)
	    (if (<= value upper-bound)
		(consumer-success
 		  value
		  (lambda (new-consumer-success new-consumer-failure)
		    (producer
		      (lambda (new-value new-producer)
			(consumer
			  new-consumer-success
			  (lambda (v)
			    (new-consumer-failure (consumer-failure v)))
			  new-value 
			  new-producer))
		      new-consumer-failure)))
		(consumer-failure producer)))))
      (enum-from
        lower-bound
	(lambda (value new-producer)
	  (consumer consumer-success consumer-failure value new-producer))
	consumer-failure))))
	 


(define foldl
  (lambda (f z value producer)
    (producer
       (lambda (new-value new-producer)
	 (foldl f (f z value) new-value new-producer))
       (lambda (ignore) (f z value)))))


(define sum
  (lambda (value producer)
    (foldl + 0 value producer)))


(define h-reverse
  (lambda (value producer)
    (foldl (lambda (xs x) (cons x xs)) '() value producer)))


(define id (lambda (id) id))


;;; Sum the numbers 1 to 10
;;;
(define foo
  (lambda ()
    (enum-from-to 1 10 sum id)))

;;; Produce a list of the numbers 1 to 10 in reverse order
;;;
(define bar
  (lambda ()
    (enum-from-to 1 10 h-reverse id)))
