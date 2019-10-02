;
; Uniq Print/Read Feature
;      Programed by Masahiro Hirata
;      Last Update 88/12/07
;

;;     Date revised    28-May-1992 by Shoichi Hayashi
;;     Date revised    26-Jan-1993 by Shoichi Hayashi
;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/uniqprnt.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

(define (uniq-print x)
  (let ((ul (u-conv x)))
  (pretty-print ul)
  (uu-conv ul)
  #v))

(define (uniq-write x)
  (let ((ul (u-conv x)))
  (write ul)
  (uu-conv ul)
  #v))

(define (uniq-read x)
  (uu-conv (read)))

(define (u-conv x)
  (let ((oc nil)
        (asl nil)
        (n 0))
    
    (define (check-uniqness x)
      (letrec ((check
                (lambda (x)
                  (cond ((memq x oc)
                         (or (assq x asl)
                             (set! asl (cons (list x) asl))))
                        ((pair? x)
                         (set! oc (cons x oc))
                         (check (car x))
                         (check (cdr x)))
                        ((vector? x)
                         (set! oc (cons x oc))
                         (do ((max (vector-length x))
                              (i 0 (1+ i)))
                             ((>= i max))
			   (check (vector-ref x i))))))))
        (check x)))
    
    (define (u-conv-1 x)
      (let ((p (assq x asl)))
        (cond (p (or (and (null? (cdr p))
                          (set! n (1+ n))
                          (set-cdr! p (string->symbol
                                       (string-append "#" 
					      (number->string n) "#")))
                          (let ((label (string->symbol
					(string-append "#" 
					       (number->string n) "="))))
                            (cons label
                                  (cond ((pair? x)
                                         (let* ((l (u-conv-1 (car x)))
                                                (r (u-conv-1 (cdr x))))
                                           (set-car! x l)
                                           (set-cdr! x r)
                                           x))
                                        ((vector? x)
                                         (do ((max (vector-length x))
                                              (i 0 (1+ i)))
                                             ((>= i max) x)
					   (vector-set! x i
					     (u-conv-1 (vector-ref x i)))))))))
                     (cdr p)))
              ((pair? x)
               (let* ((l (u-conv-1 (car x)))
                      (r (u-conv-1 (cdr x))))
                 (set-car! x l)
                 (set-cdr! x r)
                 x))
              ((vector? x)
               (do ((max (vector-length x))
                    (i 0 (1+ i)))
                   ((>= i max) x)
		 (vector-set! x i (u-conv-1 (vector-ref x i)))))
              (else
               x))))
    
    (check-uniqness x)
    (u-conv-1 x)))

(define (uu-conv x)
  (let ((alist '()))
    
    (define (def-marker? a)
      (and (symbol? a)
           (let* ((s (symbol->string a))
                  (l (string-length s)))
             (and (>= l 3)
                  (eq? (string-ref s 0) #\#)
                  (eq? (string-ref s (-1+ l)) #\=)
                  (call-with-current-continuation
                   (lambda (return)
                     (do ((i 1 (1+ i)))
                         ((>= i (-1+ l)) #t)
		       (if (not (and (<= 48 (char->integer (string-ref s i)))
				     (<= (char->integer (string-ref s i)) 57)))
			   (return #f)))))))))
    
    (define (ref-marker? a)
      (and (symbol? a)
           (let* ((s (symbol->string a))
                  (l (string-length s)))
             (and (>= l 3)
                  (eq? (string-ref s 0) #\#)
                  (eq? (string-ref s (-1+ l)) #\#)
                  (call-with-current-continuation
                   (lambda (return)
                     (do ((i 1 (1+ i)))
                         ((>= i (-1+ l)) #t)
		       (if (not (and (<= 48 (char->integer (string-ref s i)))
				     (<= (char->integer (string-ref s i)) 57)))
			   (return #f)))))))))
    
    (define (marker->number a)
      (let ((s (symbol->string a)))
        (string->number (substring s 1 (-1+ (string-length s))))))
    
    (define (uu-conv-1 x)
      (cond ((symbol? x)
             (if (ref-marker? x)
                 (cdr (assoc (marker->number x) alist))
                 x))
            ((pair? x)
             (if (def-marker? (car x))
                 (begin (set! alist (cons (cons 
					   (marker->number (car x)) (cdr x))
                                          alist))
                        (uu-conv-1 (cdr x)))
                 (let* ((a (uu-conv-1 (car x)))
                        (d (uu-conv-1 (cdr x))))
                   (set-car! x a)
                   (set-cdr! x d)
                   x)))
            ((vector? x)
             (do ((m (vector-length x))
                  (i 0 (1+ i)))
                 ((>= i m) x)
	       (vector-set! x i (uu-conv-1 (vector-ref x i)))))
            (else x)))
    
    (uu-conv-1 x)))

; ******************** Sample ********************

;(uniq-print (let ((x '(a s d)))
;              (set-cdr! (last-pair x) (cdr x))
;              x))

;(uniq-print (let ((x '#((1) (2) (3) (4) (5))))
;              (vector-set! x 3 x)
;              (vector-set! x 0 (vector-ref x 2))
;              x))

;(define (b n)
;  (if (zero? n) '()
;      (let ((x (b (-1+ n)))) (cons x x))))
;(b 5)
;(uniq-print (b 5))

;(uu-conv (u-conv (b 5)))
