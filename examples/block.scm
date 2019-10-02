(macro block (lambda (l)
	(let ((name (cadr l))
		  (forms (cddr l)))
	`(call-with-current-continuation
		(lambda (,name) ,@forms)))))

(macro return-from (lambda (l)
	(let ((name (cadr l))
		  (value (caddr l)))
	`(,name ,value))))

(define identity (lambda (x) x))

;(define (current-continuation)
;	(call-with-current-continuation
;	identity))

(define %catchers% (make-stack))

(macro catch (lambda (l)
	(let ((tag (cadr l))(forms (cddr l)))
		`(call-with-current-continuation
			(lambda (-c-)
				((%catchers% 'push)(cons ,tag -c-))
				(let ((ans (begin ,@forms)))
					(%catchers% 'pop)
					ans))))))

(define (throw tag value)
	(let ((continuation (search-catcher tag)))
		(continuation value)))

(define (search-catcher tag)
	(if (%catchers% 'empty?)
    	(error "Tag name not found: " tag)
		(let ((ans (%catchers% 'pop)))
			(if (eq? (car ans) tag)
				(cdr ans)
				(search-catcher tag)))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (fun1 x)
	(catch 'trap (display 'pfun1)(newline)(+ 3 (fun2 x))))

(define (fun2 y)
	(catch 'trap (display 'pfun2)(newline)(* 5 (fun3 y))))

(define (fun3 z)
	(%catchers% 'print)
	(newline)
	(throw 'trap z))

(fun1 7)
(%catchers% 'print)

(define (fun2 y)
	(%catchers% 'print)
	(newline)
	(catch 'snare (display 'pfun2)(newline)(* 5 (fun3 y))))

(fun1 7)
(%catchers% 'print)

(define (fun3 z)
	(%catchers% 'print)
	(newline)
	2)

(fun1 7)
(%catchers% 'print)