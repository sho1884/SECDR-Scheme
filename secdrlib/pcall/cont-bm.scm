;;; $Id: cont-bm.scm,v 1.8 1992/01/15 12:11:29 queinnec Exp $

;;;:::::::::::::::::::::::::::::::::::::::::::::::::::
;;;             benchmark using continuations
;;;
;;; Christian Queinnec queinnec@poly.polytechnique.fr
;;; \'Ecole Polytechnique \& INRIA--Rocquencourt
;;; 91128 Palaiseau Cedex -- France
;;;:::::::::::::::::::::::::::::::::::::::::::::::::::

;;; This file uses pcall, futures and splitter.
;;; It might serve as a benchmark for continuations with indefinite
;;; extent.

;;; FIBONACCI
;;; What would be a benchmark without fibonacci ?

(define fib
  (lambda (n)
    (if (<= n 2) 1
        (pcall + (fib (- n 1)) (fib (- n 2))) ) ) )

;;; CONCURRENT QUICKSORT
;;; Note that the separate function to extract higher or lower items
;;; with respect to a pivot can be run concurrently. On the other hand
;;; QuickSort is not really concurrent

(define qsort
  (lambda (l)
    (if (null? l) l
        (pcall append (qsort (separate < (car l) (cdr l)))
                      (separate = (car l) l)
                      (qsort (separate > (car l) (cdr l))) ) ) ) )

(define separate
  (lambda (comparator pivot l)
    (if (pair? l)
        (if (comparator (car l) pivot)
            (cons (car l) (separate comparator pivot (cdr l)))
            (separate comparator pivot (cdr l)) ) 
        '() ) ) )

;;; QUEENS
;;; Another great classical ! Particularly concurrent.

; Already defined in threads.scm
;(define iota
;  (lambda (start end)
;    (if (< start end)
;        (cons start (iota (+ start 1) end))
;        '() ) ) )

(define queens 
  (lambda (n)
    (search n                           ; number of queens to place
            0                           ; precise queen to place
            '()                         ; already placed queens
            (iota 0 n)                  ; possible positions for queen 0
            ) ) )

(define gather
  (lambda (f l)
    (if (pair? l)
        (pcall append (f (car l))
                      (gather f (cdr l)) )
        '() ) ) )

(define search
  (lambda (n p state l)
    (if (= n p)
        (list state)
        (gather (lambda (i) 
                  (if (check p i state)
                      (search n (+ 1 p) (cons i state) l)
                      '() ) )
                l ) ) ))

(define check
  (lambda (p i s)
    (or (null? s)
        (and (not (memq i s))           ; check line
             (check-others (- i 1) (+ i 1) s) ) ) ))

(define check-others
  (lambda (i j s)
    (or (null? s)                       ; check diagonals
        (and (not (= i (car s)))
             (not (= j (car s))) 
             (check-others (- i 1) (+ j 1) (cdr s)) ) ) ))

;;; PARTIAL CONTINUATIONS
;;; See Queinnec and Serpette's POPL 91 paper.

(define (visit tree fn)
  (if (pair? tree)
      (begin (visit (car tree) fn)
             (visit (cdr tree) fn) )
      (fn tree) ) )

(define (every? fn l)
  (if (pair? l) (and (fn (car l))
                     (every? fn (cdr l)) )
      #t ) )

(define (some? fn l)
  (if (pair? l)
      (or (fn (car l)) (some? fn (cdr l)) )
      #f ) )

(define (//map f l)
  (if (pair? l)
      (pcall cons (f (car l)) (//map f (cdr l)))
      '() ) )

(define (same-fringe trees)
  (define end (list 'end))              ; sentinel
  (define (loop leafs)
    (or (every? (lambda (leaf) (eq? leaf end)) leafs)
        (if (some? (lambda (leaf) (eq? leaf end)) leafs)
            #f
            (and (every? (lambda (leaf)(eq? (car leaf) (car (car leafs))))
                         (cdr leafs) )
                 (loop (//map (lambda (leaf) ((cdr leaf) end))
                              leafs ) ) ) ) ) )
  (loop (//map (lambda (tree)
                 (splitter 
                  (lambda (m)
                    (let ((mark m))
                      (visit tree
                             (lambda (leaf)
                               (call/pc 
                                mark 
                                (lambda (c)
                                  (abort mark
                                         (lambda ()
                                           (cons leaf 
                                                 (lambda (v)
                                                   (splitter
                                                    (lambda (m)
                                                      (set! mark m)
                                                      (c v) ) ) ) ) ) ) ) 
                                ) ) ) ) ) ) )
               trees )) )

;;; SUSPENSIONS

(define (times-list factor max stream)
 (cond ((null? stream) '())
       ((pair? stream)
        (let ((p (* factor (car stream))))
          (if (> p max)
              '()
              (cons p (times-list factor max (cdr stream))) ) ) )
       (else (pursue stream (lambda (v)
                              (times-list factor max v) ))) ) )

(define (merge stream1 stream2)
  (define (fuse v1 v2)
    (let ((a1 (car v1))
          (a2 (car v2)) )
      (cond ((< a1 a2) 
             (cons a1 (merge (cdr v1) stream2)) )
            ((> a1 a2) 
             (cons a2 (merge stream1 (cdr v2))) )
            (else
             (cons a1 (merge (cdr v1) (cdr v2)))) ) ) )
  (cond ((null? stream1) stream2)
        ((null? stream2) stream1)
        ((pair? stream1)
         (if (pair? stream2)
             (fuse stream1 stream2)
             (pursue stream2 (lambda (v2) (merge stream1 v2))) ) )
        (else (pursue stream1 (lambda (v1) (merge v1 stream2)))) ) )

(define (stream->value stream)
  (cond ((null? stream) '())
        ((pair? stream) 
         (cons (car stream) (stream->value (cdr stream))) )
        (else (stream->value (touch stream))) ) )        

(define (hamming max)
  (let* ((result (make-place))
         (r2 (times-list 2 max result))
         (r3 (times-list 3 max result))
         (r5 (times-list 5 max result))
         (r23 (merge r2 r3)) )
    (assign! result (cons 1 (merge r23 r5)))
    result ) )


;;; RUNNING THIS FILE
;;; These constants set the size of the benchmark.

(define fib-to-compute 15)
(define queens-size 6)
(define list-to-sort (reverse (iota 1 80)))
(define tree1 '(a . ( b . (c . d))))
(define tree2 '((a . (b . c)) . d))
(define tree3 '(((a . b) . c) . d))
(define bad-tree '(a b c . not-d))
(define trees (list (cons (cons tree1 tree1) (cons tree2 tree3))
                    (cons (cons tree1 (cons tree3 tree2)) tree3) 
                    (cons (cons (cons tree2 tree1) tree2) tree1) ))
(define bad-trees 
  (list (cons (cons tree1 tree1) (cons tree2 tree3))
        (cons (cons tree1 (cons tree3 tree2)) tree3) 
        (cons (cons (cons tree2 tree1) tree2) tree1)
        (cons (cons (cons tree2 tree1) bad-tree) tree1) ))
(define hamming-size 100)

;;; Just to see the expected results
(define (test)
  (start-multithreads
   (lambda ()
     (display "       Starting benchmark")
     (newline)
     (display (stream->value (hamming hamming-size)))
     ; (1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 ... 80 81 90 96 100)
     (newline)
     (display (qsort list-to-sort))
     ; (1 2 3 4 5 6 7 8 9 10 ...  75 76 77 78 79)
     (newline)
     (display (same-fringe trees))
     ; #T
     (newline)
     (display (same-fringe bad-trees))
     ; #F
     (newline)
     (display (fib fib-to-compute))
     ; 610
     (newline)
     (display (queens queens-size))
     ; ((4 2 0 5 3 1) (3 0 4 1 5 2) (2 5 1 4 0 3) (1 3 5 0 2 4))
     (newline)
     (display "       End of benchmark")
     (newline)
     #t ) ))

;;; To really benchmark the whole thing. Time measurement is to be added
;;; but could not since no portable way exists.
(define (benchmark)
  (start-multithreads
   (lambda ()
     (stream->value (hamming hamming-size))
     (qsort list-to-sort)
     (same-fringe trees)
     (same-fringe bad-trees)
     (fib fib-to-compute)
     (queens queens-size)
     #t ) ) )
