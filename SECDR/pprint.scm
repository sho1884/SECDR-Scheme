;;
;; pprint.scm
;;

;;;;    A Tiny Pretty Printer for SECD-Scheme
;;;
;;;     Date written    28-Nov-1989 by Akira Kida (kida)
;;;     Date revised    30-Nov-1989 by Akira Kida (kida)
;;;     Date revised    14-Dec-1989 by Akira Kida (kida)
;;;     --- rewrite for SECD-Scheme ---
;;;     Date revised    24-Jan-1990 by Atsushi Moriwaki
;;;
;;========================================================================
;;
;; RCS info: $Header: /usr/PDS/lang/Scheme/SECDR/RCS/pprint.scm,v 1.0 1993/07/06 08:26:09 s-haya Rel s-haya $
;;

(define (pretty-print a-list . port)
  ; procedure list which needs exceptional handling.
  ; structure is
  ;     (name . special-indentation)
  ;
  ;     where name is a symbol and
  ;     special-indentaion is a boolean
  ;
  ; #1 Standard format, non special case.
  ;  (proc
  ;    arg1
  ;    arg2
  ;    arg3)
  ;
  ; #2 Format for special-indentation == 0
  ;  (proc arg1
  ;    arg2
  ;    arg3)
  ;
  ; #3 Format for special-indentaion == 1
  ;  (proc arg1
  ;        arg2
  ;        arg3)
  ;
  ; #4 Format for let style = 2
  ;  (let ((x .....)
  ;        (y .....))
  ;    <....>
  ;    <....>)
  ;
  (define exception
    '((lambda . 0) (if . 0) (and . 1)
      (or . 1) (let . 2) (case . 0)
      (define . 0) (macro . 0)
      (map . 0) (apply . 0)
      (eq? . 1) (eqv? . 1) (set! . 0)
      (let* . 2) (letrec . 2)
      (* . 1) (/ . 1) (+ . 1) (- . 1)
      (= . 1) (< . 1) (> . 1) (<= . 1) (>= . 1)
      (do . 0)))
  ; special quasi-procedure
  (define special
    '((quote 1 . "'") (quasiquote 1 . "`")
      (unquote 2 . ",") (unquote-splicing 2 . ",@")))
  ; check wheter the number of elements exceeds n or not.
  (define (less-than-n-elements? a-list n)
    ; count elements in a-list at most (n+1)
    (define (up-to-nth a-list n c)
      (cond
        ((null? a-list) c)
        ((pair? a-list)
          (set! c (up-to-nth (car a-list) n c))
          (if (< n c)
            c
            (up-to-nth (cdr a-list) n c)))
        (else (+ c 1))))
    (< (up-to-nth a-list n 0) n))
  ; check if the length is fit within n colums or not.
  (define (fit-in-n-width? a-list n)
    ; measure width of a-list at most (n+1)
    (define (up-to-n-width a-list n c)
      (cond
        ((null? a-list) c)
        ((pair? a-list)
          (set! c (up-to-n-width (car a-list) n c))
          (if (< n c)
            c
            (up-to-n-width (cdr a-list) n (+ c 1))))
        (else (+ c (list-width a-list)))))
    (< (up-to-n-width a-list n 0) n))
  ; indent and pretty-print
  (define (do-pp a-list col)
    (spaces col . port)
    (pp-list a-list col 2))
  ;; main logic.
  (define (pp-list a-list col step)
    (cond
      ((atom? a-list) (write a-list . port))     ; atom
      ((and (assq (car a-list) special)
         (pair? (cdr a-list))
         (null? (cddr a-list)))   ; check for proper quote etc.
        (let ((s (assq (car a-list) special)))
          (display (cddr s) . port)             ; display using abbrev.
          (pp-list
            (cadr a-list)
            (+ col (- (list-width (cddr s)) 2))
            (cadr s))))
      ((and (less-than-n-elements? a-list 8)
            (fit-in-n-width? a-list 54))
        (write a-list . port))
      (else                               ; long list.
        (let* ((sym (car a-list))
               (ex-col (assq sym exception)))
          (if (pair? ex-col)              ; check for exception.,
            (case (cdr ex-col)
              ((0 1)
                (display "(" . port)
                (write sym . port)
                (display " " . port)
                (pp-list (cadr a-list) (+ col 2 (list-width sym)) 2)
                (pp-args
                  (cdr (cdr a-list))
                  (+ col 2 (if (zero? (cdr ex-col)) 0 (list-width sym)))))
              ((2)
                (display "(" . port)
                (write sym  . port)
                (display " " . port)
                (if (symbol? (cadr a-list))
                  (begin ; named let
                    (write (cadr a-list) . port)
                    (display " " . port)
                    (pp-list
                      (caddr a-list)
                      (+ col 3 (list-width sym) (list-width (cadr a-list)))
                      1)
                    (pp-args (cdddr a-list) (+ col 2)))
                  (begin ; usual let
                    (pp-list (cadr a-list) (+ col 2 (list-width sym)) 1)
                    (pp-args (cddr a-list) (+ col 2)))))
              (else
                (error "Illegal exception")))
            (begin                        ; normal case.
              (display "(" . port)
              (pp-list (car a-list) (+ col 1) 2)
              (pp-args (cdr a-list) (+ col step))))))))
  ;; display arguments
  (define (pp-args a-list col)
    (cond
      ((null? a-list) (display ")" . port))
      ((pair? a-list)
        (newline . port)
        (do-pp (car a-list) col)
        (pp-args (cdr a-list) col))
      (else
        (display " . " . port)
        (write a-list . port)
        (display ")" . port))))
  (do-pp a-list 0)
  (newline . port))

