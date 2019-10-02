;;; $Id: meroon.ss,v 1.3 1992/09/08 08:09:59 queinnec Exp $
;;; Copyright (c) 1990, 91, 92 by Christian Queinnec. All rights reserved.

;;;                            *********************************
;;;                            Chez Scheme Adaptation for Meroon
;;;                            *********************************

;;; This file contains the necessary adaptation for Chez Scheme
;;; based on the adaptions of Jon Rossie (jrossie@leopard.cs.indiana.edu).

;;; This program is distributed in the hope that it will be useful.
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o credit to the authors is acknowledged following current
;;;        academic behaviour
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Bug descriptions, use reports, comments or suggestions are welcome.
;;; Send them to    
;;;   <queinnec@polytechnique.fr>   or to:   <Christian.Queinnec@inria.fr>
;;;                                 or to:
;;;   Christian Queinnec                     Christian Queinnec
;;;   Laboratoire d'Informatique de l'X      INRIA -- Rocquencourt
;;;   Ecole Polytechnique                    Domaine de Voluceau, BP 105
;;;   91128 Palaiseau                        78153 Le Chesnay Cedex
;;;   France                                 France

;;;************************************************
;;;    Small, Efficient and Innovative Class System
;;;       Christian Queinnec  
;;;   \'Ecole Polytechnique & INRIA-Rocquencourt
;;;************************************************

;;; This is incorrect but gives a flavor of what does define-oo-macro.
(extend-syntax (define-oo-macro)
  ((_ (name . variables) body ...)
   (extend-syntax (name)
     ((_ . variables) 
      (begin body ...) ) ) ) )

(define (oo-concat . names)
  (string->symbol
   (apply string-append
          (map (lambda (thing)
                    (cond ((symbol? thing) 
                           (symbol->string thing) )
                          ((string? thing) thing)
                          ((number? thing)
                           (number->string thing) )
                          (else (error "non coercible to String"
                                        thing )) ) )
                  names ) ) ) )

(define (flat l)
  (define (flatten l r)
    (cond ((pair? l)
           (flatten (car l) (flatten (cdr l) r)) )
          ((null? l) r)
          (else (cons l r)) ) )
  (flatten l '()) )

(define (mapcan f l)
  (if (pair? l)
      (append (f (car l)) (mapcan f (cdr l)))
      '() ) )

(define oo-apply apply)

(define oo-error 
  (lambda (msg . culprits)
    (error 'Meroon "~A ~A" msg culprits) ) )

(extend-syntax (oo-set!)
  ((oo-set! variable form)
   (set! variable form) ) )

;;; End of meroon.ss
