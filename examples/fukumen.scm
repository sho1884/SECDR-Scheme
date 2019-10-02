;     This function will solve a specific type of word puzzle.  The type of
; puzzle is the following.  Given a set of words, where the last word is
; treated as a sum, and the previous words are treated as summands, find a
; mapping between the letters in all of the words and the digits, such that
; all of the same letters correspond to the same digit, and such that no
; two different letters correspond to the same digit.  When the digits are
; substituted for the letters, the corresponding addition must work out
; correctly.  Here is an example:
;
;        I         1      I = 1
;       BB        99      B = 9
;      ---       ---      L = 0
;      ILL       100
;
;     The program will solve such a puzzle in any base, although base 10
; is the most common.  The command (swp 10 '((I) (B B) (I L L))) would 
; generate the solution to the problem above.  The number of summand words 
; in not limited to 2.  Here is a more difficult example:
;
;     SEND        This is an interesting one to solve without the help
;     MORE        of the computer.
;    -----
;    MONEY
;
; The function is called as follows: (swp base list-of-words)
; For example, to find the solution to the puzzle above use
;     (swp 10 '((s e n d) (m o r e) (m o n e y)))
;
;                Written By Truman Collins
;                12/4/1988
;                Developed using PC Scheme 3.0
;
;                Re-written By Atsushi Moriwaki
;                2/7/1990
;                For SECDR Scheme or general Scheme
;

(define (swp base words)

  ; This function will return an empty mapping list between letters and digits
  ; It will contain the range from min to max.

  (define (create-empty-mapping min max)
    (define (fill current lis)
      (if (< current min)
          lis
          (fill (- current 1) (cons current lis))))
    (cons (fill max '()) '(())))


  ; This function will return the mapping after adding the given letter
  ; and number pair to the given mapping.

  (define (add-to-mapping mapping letter number)
    (define (remove-from-avails avail new-avails)
      (if (null? avail)
          new-avails
          (if (= (car avail) number)
              (remove-from-avails (cdr avail) new-avails)
              (remove-from-avails (cdr avail) (append new-avails
                                                      (list (car avail)))))))
    (let ((avail (car mapping))
          (maps  (cadr mapping)))
      (list (remove-from-avails avail '())
            (cons (cons letter number) maps))))


  ; This function will search the given mapping for the letter, and if it
  ; is found, it will return the corresponding number, otherwise it will
  ; return null.

  (define (letter-in-mapping? letter mapping)
    (define (look-for-letter maps)
      (if (null? maps)
          '()
          (if (equal? (caar maps) letter)
              (cdar maps)
              (look-for-letter (cdr maps)))))
    (look-for-letter (cadr mapping)))


  ; This function will return the next available number after the number
  ; passed in.  It returns null if there are no more.

  (define (next mapping number)
    (let ((rest-of-list (member number (car mapping))))
      (if (null? (cdr rest-of-list))
          '()
          (cadr rest-of-list))))


  ; This function will return the first number available in the given mapping.

  (define (first mapping)
    (let ((avail (car mapping)))
      (if (null? avail)
          '()
          (car avail))))


  ; This function will return the actual list of mapping pairs within the
  ; mapping.

  (define (get-mapping mapping)
    (cadr mapping))


  ; This function will print the mapping on two lines with the letters
  ; on top, and the corresponding numbers on the bottom.

  (define (print-mapping mapping)
    (for-each (lambda (x) (display (car x))
                          (display ":")
                          (display (cdr x))
                          (display " "))
               (get-mapping mapping))
     (newline))

  ; This function will reformat the set of words in the input of the puzzle
  ; so that they are in a better configuration for solving the puzzle.
  ; The call (reformat '((S E N D) (M O R E) (M O N E Y))) would return
  ; ((M) (O M S) (N O E) (E R N) (Y E D))

  (define (reformat list-to-reformat)

    (define (weed-nulls lis)
      (if (null? lis)
          lis
          (let ((temp (car lis)))
            (if (null? temp)
                (weed-nulls (cdr lis))
                (cons temp (weed-nulls (cdr lis)))))))

    (define (re-orient lis)
      (if (null? lis)
          lis
          (cons (map car lis) (re-orient (weed-nulls (map cdr lis))))))

    (reverse (map reverse (re-orient (map reverse list-to-reformat)))))


  (let* ((letters words)
         (non-zeros (map car letters))
         (max-carry (- (length letters) 2))
         (high-limit (- base 1))
         (reformatted (reformat letters)))

    ; This function will return the first number that can be used for the given
    ; letter.  If it is on the far left side of a word, it cannot be 0.

    (define (get-first-possible mapping letter)
      (let ((first-in-mapping (first mapping)))
        (if (and (= first-in-mapping 0) (member letter non-zeros))
            (next mapping 0)
            first-in-mapping)))


    ; This function will research the remaining columns of a puzzle.

    (define (research-remainder needed-carry mapping remainder)
      (if (null? remainder)
          (if (= needed-carry 0)
              (print-mapping mapping))
          (let* ((current-column (car remainder))
                 (rest-of-cols (cdr remainder))
                 (sum-letter (car current-column))
                 (summands (cdr current-column))
                 (val (letter-in-mapping? sum-letter mapping)))
            (if (null? val)
                (let ((low (get-first-possible mapping sum-letter)))
                  (research-sum-letter sum-letter low needed-carry mapping
                                       summands rest-of-cols))
                (let ((needed-sum (+ (* needed-carry base) val)))
                  (research-summands needed-sum summands mapping
                                     rest-of-cols))))))


  ; This function will research a sum letter trying all different
  ; possibilities.

  (define (research-sum-letter letter try-val needed-carry
                               mapping summands rest-of-cols)
    (if (null? try-val)
        '()
        (let ((new-mapping (add-to-mapping mapping letter try-val))
              (needed-sum (+ (* needed-carry base) try-val)))
          (research-summands needed-sum summands new-mapping
                             rest-of-cols)
          (research-sum-letter letter (next mapping try-val) needed-carry
                               mapping summands rest-of-cols))))


  ; This function will research the remainder of a column of summands.

  (define (research-summands needed-sum summands mapping rest-of-cols)
    (if (null? summands)
        (if (<= needed-sum max-carry)
            (research-remainder needed-sum mapping rest-of-cols)
            '())
        (let* ((new-letter (car summands))
               (rest-of-summands (cdr summands))
               (val (letter-in-mapping? new-letter mapping)))
          (if (null? val)
              (let ((low (get-first-possible mapping new-letter)))
                (research-summand-letter new-letter low
                                         needed-sum mapping
                                         rest-of-summands rest-of-cols))
              (let ((new-needed-sum (- needed-sum val)))
                (if (>= new-needed-sum 0)
                    (research-summands new-needed-sum
                                       rest-of-summands mapping
                                       rest-of-cols)))))))


; This function will research a summand letter.

    (define (research-summand-letter letter try-val needed-sum mapping
                                     rest-of-summands rest-of-cols)
      (if (null? try-val)
          '()
          (if (> try-val needed-sum)
              '()
              (let ((new-mapping (add-to-mapping mapping letter try-val))
                    (new-needed-sum (- needed-sum try-val)))
                (research-summands new-needed-sum rest-of-summands
                                   new-mapping rest-of-cols)
                (research-summand-letter letter (next mapping try-val)
                                         needed-sum mapping rest-of-summands
                                         rest-of-cols)))))

    (research-remainder 0 (create-empty-mapping 0 (- base 1)) reformatted)
    (display "That's all.")
    )
  )

