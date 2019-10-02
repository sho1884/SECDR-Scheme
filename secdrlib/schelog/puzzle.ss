;;;                            schelog
;;;                 An Embedding of Prolog in Scheme
;;;          Written by Dorai Sitaram, Rice University, 1989
;;;     Permission is granted for unrestricted non-commercial use

;;; This is the puzzle solver described in Sterling&Shapiro, p. 214

;;; It is a "trivial" (S&S's words, and I don't disagree) piece of code
;;; that successively solves each clue and query, which are expressed
;;; as Prolog goals and are executed with the meta-variable facility.

;;; The code in "real" Prolog, for comparison, is:
;;;   solve_puzzle(Clues, Queries, Solution) :- solve(Clues), solve(Queries).

;;;   solve([Clue|Clues]) :- Clue, solve(Clues).
;;;   solve([]).


(define %solve-puzzle
  (rel (Clues Queries Solution)
    [(Clues Queries Solution)
     (%solve Clues)
     (%solve Queries)]))

(define %solve
  (rel (Clue Clues)
    [([cons Clue Clues])
     Clue 
     (%solve Clues)]
    [([])]))

;;; say (tst <name-of-puzzle>) to get the solution to the puzzle.


(define tst
  (lambda (puzzle)
    (letref (Clues Queries)
      (which (Solution)
	     (puzzle Clues Queries Solution)
	     (%solve-puzzle Clues Queries Solution)))))
