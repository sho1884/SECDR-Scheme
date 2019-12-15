;;
;; basic.scm
;;

;;;;    A Baisc Pakage for SECDR Scheme
;;;
;;;     Date written    11-Feb-1990 by Atsushi Moriwaki
;;;     Date revised    20-Feb-1990 by Atsushi Moriwaki
;;;     Date revised    06-Mar-1990 by Atsushi Moriwaki
;;;     Date revised    28-May-1992 by Shoichi Hayashi
;;;     Date revised    12-Apr-1993 by Shoichi Hayashi
;;;     Date revised    12-May-1993 by Shoichi Hayashi
;;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/Mulasame/RCS/basic.scm,v 0.2 1994/09/14 02:55:46 s-haya Exp s-haya $
;;

(define compile -*-compile-*-)

(define else #t)

(define nil '())

(define t #t)

(define (apply proc args)
  (proc . args))  ;; hacked!!

(let ((oldapply apply))
  (define (make-apply-list list1 list2)
    (if (null? list2)
	list1
	(make-apply-list (cons (car list2) list1) (cdr list2))))
  (set! apply (lambda (arga . argd)
		(if (= 1 (length argd))
		    (oldapply arga (car argd))
		    (letrec ((argdr (reverse argd))
			     (argdl (car argdr))
			     (args (cdr argdr)))
		      (oldapply arga (make-apply-list argdl args)))))))

(let ((append2 append))
  (define (multi-append l)
    (if (null? (cdr l)) (car l)
	(append2 (car l) (multi-append (cdr l)))))
  (set! append
	(lambda args
	  (if (null? args) '()
	      (multi-append args)))))

(let ((string-append2 string-append))
  (define (multi-append l)
    (if (null? (cdr l)) (car l)
	(string-append2 (car l) (multi-append (cdr l)))))
  (set! string-append
	(lambda (la . ld)
	  (cond ((null? la) "")
		((null? ld)
		 (if (string? la) la
		     (error "string-append -- arguments must be string")))
		(else
		 (multi-append (cons la ld)))))))

(let ((old-error error))
  (define (add-str args)
    (if (null? args)
	""
	(string-append " ~S" (add-str (cdr args)))))
  (define (replace arg)
    (if (null? arg) #f
	(if (string? (car arg))
	    (set-car! arg
		      (string-append (car arg) (add-str (cdr arg))))
	    (replace (cdr arg)))))
  (set! error
	(lambda (string . args)
	  (if (string? string)
	      (apply old-error
	       (cons (string-append string (add-str args)) args))
	      (begin
		(replace args)
		(apply old-error (cons string args)))))))

(define (string . x)
  (let* ((len (length x))
         (str (make-string len)))
        (let str-loop ((l x) (i 0))
          (if (>= i len)
              str
              (begin (string-set! str i (car l))
                     (str-loop (cdr l) (+ i 1)))))))

(define (char->string c)
  (string-set! "@" 0 c))

(define (assoc obj list)
  (if (null? list) #f
    (if (equal? obj (car (car list)))
      (car list)
      (assoc obj (cdr list)))))

(define (rational? x)
  (or (integer? x) (proper-rational? x)))

(define (real? x)
  (or (integer? x)
      (proper-rational? x)
      (proper-real? x)))

(define (number? x)
  (or (integer? x)
      (proper-rational? x)
      (proper-real? x)))

(define complex? number?)

(define (exact? n)
  (rational? n))

(define (inexact? n)
  (proper-real? n))

(define (exact->inexact n)
  (+ n 0.0))

(define (gcd . args)
  (define (gcd-2 a b)
    (if (zero? b) a
      (gcd-2 b (remainder a b))))
  (if (null? args) 0
    (let foo ((a (abs (car args)))
              (d (cdr args)))
      (if (pair? d)
        (foo (gcd-2 a (abs (car d))) (cdr d))
        a))))

(define (lcm . args)
  (let ((product (abs (apply * args)))
        (gcd-of-args (apply gcd args)))
      (if (zero? gcd-of-args) 1
        (quotient product gcd-of-args))))

(define (procedure? x)
  (or (primitive-procedure? x)
      (closure? x)
      (continuation? x)))

(define (some? proc plist)
  (cond
    ((null? plist) #f)
    ((proc (car plist)) #t)
    (else (some? proc (cdr plist)))))

(define (every? proc plist)
  (cond
    ((null? plist) #t)
    ((proc (car plist)) (every? proc (cdr plist)))
    (else #f)))

(define (map proc arg . rest)
  (define (every? proc plist)
    (cond
     ((null? plist) #t)
     ((proc (car plist)) (every? proc (cdr plist)))
     (else #f)))
  (define (map1 proc list)
    (if (pair? list)
      (cons (proc (car list)) (map1 proc (cdr list)))
      '()))
  (define (maps proc dlist)
    (if (every? pair? dlist)
      (cons (apply proc (map1 car dlist))
            (maps proc (map1 cdr dlist)))
      '()))
  (if (pair? rest)
    (maps proc (cons arg rest))
    (map1 proc arg)))

(define (for-each proc . args)
  (define (every? proc plist)
    (cond
     ((null? plist) #t)
     ((proc (car plist)) (every? proc (cdr plist)))
     (else #f)))
  (let for-each-loop ((dlist args))
    (if (every? pair? dlist)
      (begin (apply proc (map car dlist))
             (for-each-loop (map cdr dlist)))
      #v)))

(define (equal? x y)
  (cond
    ((pair? x)
      (if (and (pair? y)
               (equal? (car x) (car y)))
        (equal? (cdr x) (cdr y))
        #f))
    ((vector? x)
      (if (vector? y)
        (let ((len (vector-length x)))
          (if (= len (vector-length y))
            (let loop ((i 0))
              (if (< i len)
                (if (equal? (vector-ref x i) (vector-ref y i))
                  (loop (1+ i))
                  #f)
                #t))
            #f))
        #f))
    (#t (eqv? x y))))

(define (member e set)
  (if (pair? set)
    (if (equal? e (car set))
      set
      (member e (cdr set)))
    #f))

(define (last-pair x)
    (if (pair? (cdr x))
        (last-pair (cdr x))
        x))

(define (printf fmt . args) (%format% #t fmt . args))

(define (newline . port) (write-char #\newline . port))

(define (prompt-read prompt) (display prompt) (read))

(define (call-with-input-file string proc . bin-mode)
  (let ((input-port (open-input-file string . bin-mode))
        (old-error-hook -*-error-hook-*-))
    (set! -*-error-hook-*-
      (lambda ()
        (close-input-port input-port)
        (set! -*-error-hook-*- old-error-hook)
        (old-error-hook)))
    (proc input-port)
    (close-input-port input-port)
    (set! -*-error-hook-*- old-error-hook)
    #t))

(define (call-with-output-file string proc . bin-mode)
  (let ((output-port (open-output-file string . bin-mode))
        (old-error-hook -*-error-hook-*-))
    (set! -*-error-hook-*-
      (lambda ()
        (close-output-port output-port)
        (set! -*-error-hook-*- old-error-hook)
        (old-error-hook)))
    (proc output-port)
    (close-output-port output-port)
    (set! -*-error-hook-*- old-error-hook)
    #t))

(define (call-with-current-continuation proc)
  (proc (%current-dump%)))  ;; hacked!!

(define call/cc call-with-current-continuation)

(define make-temp-symbol gensym)

(define (vector . vec) (list->vector vec))

(define (force proc)
    (if (promise? proc) (proc) proc))

(macro cons-stream (lambda (l)
    `(cons ,(cadr l) (delay ,(caddr l)))))

(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))

(define (compile-file . args)
  (let* ((bin-file '()) 
         (scm-file-list 
           (let get-scm-file-list ((list args))
             (if (null? (cdr list))
               (begin (set! bin-file (car list)) '())
               (cons (car list) (get-scm-file-list (cdr list)))))))
    (call-with-output-file bin-file
      (lambda (bin-port)
        (for-each
          (lambda (scm-file)
            (call-with-input-file scm-file
              (lambda (scm-port)
                (%format% #t "compiling ~A " scm-file)
                (let comp-file-loop ((code (read scm-port)))
                  (if (eof-object? code)
                    (%format% #t " done~%")
                    (begin
                      (write (compile code) bin-port)
                      (write-char #\.)
                      (comp-file-loop (read scm-port))))))))
          scm-file-list)))))

(define (string->list s)
  (do ((i (- (string-length s) 1) (- i 1))
       (l '() (cons (string-ref s i) l)))
      ((< i 0) l)))

(define (list->string l) (apply string l))

(define (string-copy s)
  (do ((v (make-string (string-length s)))
       (i (- (string-length s) 1) (- i 1)))
      ((< i 0) v)
      (string-set! v i (string-ref s i))))

(define (string-fill! s obj)
  (do ((i (- (string-length s) 1) (- i 1)))
      ((< i 0))
      (string-set! s i obj)))

(define (vector-fill! s obj)
  (do ((i (- (vector-length s) 1) (- i 1)))
      ((< i 0))
      (vector-set! s i obj)))

;;
;;; The following rationalize procedure is due to Alan Bawden.
;;; -- ALAN@AI.AI.MIT.EDU --
;;

; This code assumes -perfect- arithmetic, so it wont get the
; exactness correct in any particular implementation.
; To really understand how this works, you should go learn about continued
; fractions.

(define (rationalize x e)
  (simplest-rational (- x e) (+ x e)))

; Produces the simplest rational between X and Y inclusive.
; (In the comments that follow, [x] means floor(x).)
(define (simplest-rational x y)
  (define (simplest-rational-internal x y)  ; assumes 0 < X < Y
    (let ((fx (floor x))    ; [X] <= X < [X]+1
      (fy (floor y)))   ; [Y] <= Y < [Y]+1, also [X] <= [Y]
      (cond ((not (< fx x))
         ;; X is an integer so X is the answer:
         fx)
        ((= fx fy)
         ;; [Y] = [X] < X < Y so expand the next term in the continued
         ;; fraction:
         (+ fx (/ (simplest-rational-internal (/ (- y fy)) (/ (- x fx))))))
        (else
         ;; [X] < X < [X]+1 <= [Y] <= Y so [X]+1 is the answer:
         (+ 1 fx)))))
  (cond ((< y x)
     ;; Y < X so swap and try again:
     (simplest-rational y x))
    ((not (< x y))
     ;; X = Y so if that is a rational that is the answer, otherwise
     ;; there is nothing we can return at all.
     ;(if (rational? x) x (error)))
     ; rewrite for SECDR Scheme
     (if (rational? x) x (error "Estimation is too small to rationalize")))
    ((positive? x)
     ;; 0 < X < Y which is what SIMPLEST-RATIONAL-INTERNAL expects:
     (simplest-rational-internal x y))
    ((negative? y)
     ;; X < Y < 0 so 0 < -Y < -X and we negate the answer:
     (- (simplest-rational-internal (- y) (- x))))
    (else
     ;; X <= 0 <= Y so zero is the answer:
     0)))

;;;
;;; Quicksort (straight from Wirth, Algorithmen & Datenstrukturen, p. 117)

(define (qsort obj pred)
  (define (vector-copy vec)
    (list->vector (vector->list vec)))
  (if (vector? obj)
      (qsort! (vector-copy obj) pred)
      (vector->list (qsort! (list->vector obj) pred))))

(define (qsort! v pred)
  (define (internal-sort l r)
    (let ((i l) (j r) (x (vector-ref v (quotient (- (+ l r) 1) 2))))
      (let loop ()
	(do () ((or (eq? x (vector-ref v i))(pred x (vector-ref v i))))
	  (set! i (+ i 1)))
	(do () ((or (eq? x (vector-ref v j))(pred (vector-ref v j) x)))
	  (set! j (- j 1)))
	(if (<= i j)
	    (let ((w (vector-ref v i)))
	      (vector-set! v i (vector-ref v j))
	      (vector-set! v j w)
	      (set! i (+ i 1))
	      (set! j (- j 1))))
	(if (<= i j)
	    (loop)))
      (if (< l j)
	  (internal-sort l j))
      (if (< i r)
	  (internal-sort i r))))
  (let ((len (vector-length v)))
    (if (> len 1)
	(internal-sort 0 (- len 1)))
    v))

;;
;;; Enf of Basic Package
;;
