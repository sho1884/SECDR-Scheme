;;; Article: 2211 of comp.lang.scheme
;;; From: brian@granite.jpl.nasa.gov
;;; Newsgroups: comp.lang.scheme
;;; Subject: hacks for remove and remove!
;;; Summary: list-copy was fun, so here's more
;;; Message-ID: <1991Oct3.145636.25037@jpl-devvax.jpl.nasa.gov>
;;; Date: 3 Oct 91 14:56:36 GMT
;;; Sender: usenet@jpl-devvax.jpl.nasa.gov
;;;  (For NNTP so rrn will be able to post)
;;; Reply-To: brian@granite.jpl.nasa.gov
;;; Followup-To: comp.lang.scheme
;;; Organization: Jet Propulsion Laboratory
;;; Lines: 91
;;; Nntp-Posting-Host: 137.79.110.100

;;; I needed implementations of remove, remv, remq, and remove!,
;;; remv!, and remq! to port some Chez code to r4rs.  At the risk
;;; of being flamed ("you dare to call yourself a programmer!?!"),
;;; permit me to expose the following attempts to public criticism
;;; (I learned so much from the exchange about 'list-copy' that it
;;; might be worth the risk :-)

;;; *code ON***********************************************

(define (make-remover test-proc)
  (lambda (item list) 
    (cond
     ( (null? list)                 ; done
       list )
     
     ( (test-proc item (car list))  ; item at beginning
       ((make-remover test-proc) item (cdr list)) )
     
     ( else                         ; not at beginning
       (cons (car list) 
             ((make-remover test-proc) item (cdr list))) ))))

(define remove (make-remover equal?))
(define remv   (make-remover eqv?))
(define remq   (make-remover eq?))

(begin  ; test make-remover; unquote to use
  (let ((x '(a b c d a b c d a a a b b b c c c d d d a a a)))
    (display (remove 'a x)) (newline)
    (display x)             (newline)
    (display (remv 'a x))   (newline)
    (display x)             (newline)
    (display (remq 'a x))   (newline)
    (display x)             (newline)))

(define (make-!-remover test-proc)
  
  (define (remove-from-middle before-cell cell)
    (set-car! cell '())
    (set-cdr! before-cell (cdr cell))
    (set-cdr! cell '()))
  
  (define (make-rest-remover test-proc)
    (lambda (item orig-list before-cell cell)
      (cond
       ( (null? cell) orig-list )
       
       ( (test-proc item (car cell))
         (let ((after-cell (cdr cell)))
           (remove-from-middle before-cell cell)
           ((make-rest-remover test-proc)
            item orig-list before-cell after-cell)) )
         
       ( else
         ((make-rest-remover test-proc)
          item orig-list cell (cdr cell)) ))))
    
  (lambda (item list) 
    (cond
     ( (null? list)                 ; done
       list)
     
     ( (test-proc item (car list))
       (let ((a (cdr list)))
         (set-car! list (car a))
         (set-cdr! list (cdr a))
         (set-car! a '())
         (set-cdr! a '())
         ((make-!-remover test-proc) item list)) )
     
     ( else
       ((make-rest-remover test-proc)
        item list list (cdr list)) ))))


 (define remove! (make-!-remover equal?))
 (define remv!   (make-!-remover eqv?))
 (define remq!   (make-!-remover eq?))

(begin  ; test make-!-remover; unquote to use
  (let ((x '(a b c d a b c d a a a b b b c c c d d d a a a)))
    (display (remove! 'a x)) (newline)
    (display x)              (newline)
    (display (remv! 'b x))   (newline)
    (display x)              (newline)
    (display (remq! 'c x))   (newline)
    (display x)              (newline)))
