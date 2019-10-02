(define the-empty-stream nil)

(define (empty-stream? n) (null? n))

(define (filter pred stream)
    (cond ((empty-stream? stream) the-empty-stream)
	      ((pred (head stream))
		   (cons-stream (head stream)
		                (filter pred (tail stream))))
		  (else (filter pred (tail stream)))))

(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ 1 n))))

(define (nth-stream n s)
    (if (zero? n)
        (head s)
		(nth-stream (- n 1) (tail s))))

(define (sieve stream)
    (cons-stream
	 (head stream)
	 (sieve (filter
	         (lambda (x) (not (zero? (remainder x (head stream)))))
			 (tail stream)))))

(define primes (sieve (integers-starting-from 2)))

;(nth-stream 50 primes) ; 51st prime
