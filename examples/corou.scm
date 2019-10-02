
    (define (print x)
        (display x)
        (newline))

    (define (bar)
        (let ((qwe nil))
            (print 'init)
            (call/cc (lambda (-c-) (set! qwe -c-)))
            (print 'bar)
            qwe))

    (define (foo)
        (let ((qwe nil))
            (set! qwe (bar))
            (print 'foo)
            (qwe nil)))

