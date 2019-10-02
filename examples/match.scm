;;;; Article: 2157 of comp.lang.scheme
;;;; From: mj@cs.brown.edu (Mark Johnson)
;;;; Newsgroups: comp.lang.scheme
;;;; Subject: A pattern matching macro in scheme
;;;; Message-ID: <86004@brunix.UUCP>
;;;; Date: 13 Sep 91 13:10:15 GMT
;;;; Sender: news@brunix.UUCP
;;;; Reply-To: mj@cs.brown.edu (Mark Johnson)
;;;; Organization: Brown University Department of Computer Science
;;;; Lines: 51

;;; The following is the code for a simple pattern matching macro.
;;; I would appreciate comments or suggestions.
;;;
;;; A match macro call has the form
;;;
;;;     (match expr pattern match-expr doesnt-match-expr)
;;;
;;; and its value is match-expr if expr matches pattern, and 
;;; doesnt-match-expr if it doesn't.
;;;
;;; Patterns are lists like ('define fn-name ('lambda args . body)).
;;; Quoted expressions specify an equal? match, whereas unquoted symbols
;;; function as pattern variables that match anything.  These symbols
;;; are locally bound during the matching process to the parts of
;;; expr that they match, and match-expr is evaluated in this lexical
;;; environment.  ? is an anonymous variable.
;;;
;;; Example: The following function translates standard scheme function
;;; definitions into mit style functions definitions, and doesn't touch
;;; other forms.
;;;
;;; (define (standard-to-mit form)
;;;   (match form ('define fn-name ('lambda args . body))
;;;     `(define (,fn-name . ,args) . ,body)
;;;     form))


;;; This defines match as a macro in TI-scheme and MacScheme.  
;;; You may need another special form here.

(macro match
  (lambda (e)
    (match-maker (list-ref e 2) (list-ref e 1) (list-ref e 3) (list-ref e 4))))

(define (match-maker pattern parameter true-exp false-exp)
  (cond ((null? pattern) `(if (null? ,parameter) ,true-exp ,false-exp))
        ((number? pattern) 
         `(if (and (number? ,parameter) (= ,parameter ,pattern))
              ,true-exp
              ,false-exp))
        ((eq? pattern '?) true-exp)
        ((symbol? pattern) `(let ((,pattern ,parameter)) ,true-exp))
        ((and (pair? pattern) (eq? (car pattern) 'quote)) 
         `(if (equal? ',(cadr pattern) ,parameter) ,true-exp ,false-exp))
        ((pair? pattern) 
         `(if (pair? ,parameter)
              ,(match-maker (car pattern) `(car ,parameter)
                            (match-maker (cdr pattern) `(cdr ,parameter)
                                         true-exp false-exp)
                            false-exp)
              ,false-exp))))

;;; Sample
(define (standard-to-mit form)
  (match form ('define fn-name ('lambda args . body))
    `(define (,fn-name . ,args) . ,body)
    form))

