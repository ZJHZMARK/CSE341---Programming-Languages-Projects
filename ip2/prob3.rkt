#lang racket

;; Problem 3: Continuations and Backtracking

;; A continuation is a function that represents "the rest of the
;; computation". Continuations are surprisingly powerful. They can be
;; used to simulate or implement exceptions, break/continue from
;; inside a loop, and even backtracking search.

;; As a first example, we can rewrite some familiar functions into
;; so-called continuation-passing style. Here is an example, based on
;; our old friend, the factorial function.

(define (fact-cps n)
  (define (go n k)
    (if (zero? n)
        (k 1)
        (go (- n 1) (lambda (ans) (k (* n ans))))))
  (go n (lambda (x) x)))

;; Notice the inner helper function go takes an additional argument,
;; called k, which is the continuation. Instead of ever returning an
;; answer, go always just calls a suitable continuation. In the base
;; case, it calls k with 0. In the recursive case, it calls itself
;; tail recursively but with a modified continuation. This new
;; continuation takes in the recursive answer, multiplies it by n and
;; then calls k with the result.
;;
;; The outer function passes the identity function to `go` as the
;; initial continuation. You can think of this as saying "there's
;; nothing left to do, just return the answer now."
;;
;; All of this is sort of weird and pointless at the moment, but you
;; should at least convince yourself that fact-cps computes the
;; factorial. Later, we will see why CPS is useful.

;; Problem 3a: Write a version of sum that uses CPS internally,
;; similar to factorial above. Use an inner helper function similar to
;; above.

(define (sum-cps l)
  (define (go s k)
    (if (null? s)
        (k 0)
        (go (cdr s) (lambda (ans) (k (+ (car s) ans))))))
  (go l (lambda (x) x)))

;; The first interesting thing we can do with continuations is use
;; them to implement a sort of pattern matching.

;; Problem 3b: Write a version of assoc that takes *two*
;; continuations. If the element is found, the first continuation is
;; called with the result. If the element is not found, the secound
;; continuation is called with no arguments.
(define (assoc-cps v l k_found k_notfound)
  (cond [(false? (assoc v l)) (k_notfound)]
        [#t (k_found (assoc v l) k_notfound)]))

;; Then write a function assoc-default that takes a default value and
;; uses assoc-cps to either return the successfully found value, or
;; the default value.
(define (assoc-default v_default v l)
  (assoc-cps v l (lambda (x) x) (lambda () v_default)))

;; Continuations can also be used to implement backtracking search. We
;; will develop this idea in the rest of this problem.
;;
;; Our library will center around the idea of a "choice". Intuitively,
;; a choice represents a computation that may produce zero, one, or
;; even multiple values. Choices also support backtracking. During the
;; course of evaluation, the choice may produce a value, which is then
;; passed to the rest of the program. Later, the program might decide
;; that no further progress is possible. In that case, backtracking
;; occurs, and the choice can then return another value to try
;; again. This process continues until all values are exhausted.
;;
;; Internally, choices are implemented using continuations. A choice
;; is a function that takes two continuations: a success continuation
;; and a failure continuation.
;;
;; The success continuation represents the "rest of the computation in
;; the forward direction", while the failure continuation represents
;; the "rest of the computation in the backward direction".
;;
;; Calling the failure continuation is backtracking. Once we
;; backtrack, we will never come back "forward" through this point,
;; and we have no further information to report to the computation
;; "behind us", so failure continuation takes no arguments.
;;
;; Calling the success continuation means that the current computation
;; has succeeded in finding a value that the computation in the
;; forward direction can try to work with.
;;
;; If the forward computation decides to backtrack, this will involve
;; passing back through the current computation. At that point, the
;; current computation should try to find another value, and then
;; proceed forward again. Or, if no further values are possible, then
;; the current computation should backtrack further.
;;
;; All this is to say that the success continuation will take a value
;; *and a failure continuation*. This is pretty funky, since the
;; continuation takes a continuation. But this is required to properly
;; implement backtracking, since the computation in the forward
;; direction needs to have a way to pass back through the current
;; point.
;;
;; To summarize, a choice is a function that takes a success
;; continuation and a failure continuation. The success continuation
;; expects as arguments a value to try and a failure continuation to
;; use for backtracking. Failure continuations expect no arguments.

;; The first thing we will implement is a way to "run" a choice at the
;; top-level, to see what values are inside, so to speak. To run a
;; choice, we need to pass in continuations, and we will pick
;; functions that report success and failure by printing
;; messages.

;; There are two variants of the run functions.

;; This variant runs the choice to find its first value.
(define (choice-run-one f)
  (f debug-success-one debug-failure))

;; On the other hand, this variant backtracks to explore *all* values
;; in the choice.
(define (choice-run-all f)
  (f debug-success-all debug-failure))

;; The run functions use the following three useful continuations.

(define (debug-failure)
  (error "Top-level failure!"))

(define (debug-success-one val k_fail)
  (pretty-print (list 'success val)))

(define (debug-success-all val k_fail)
  (pretty-print (list 'success val))
  (k_fail))


;; Problem 3c

;; The most basic choice is among finitely many explicitly given
;; arguments. This function is curried, where the first step takes
;; arbitrarily many arguments and chooses between them. (The second
;; step of the curriying is an implementation detail "going into the
;; monad".)
;;
;; For example, calling (choice) (ie, with no arguments) represents
;; failure and causes backtracking.
;;
;; (choice 1 2 3) is a computation with the three values '(1 2 3).
;;
;; To choose an element of a list `l`, you can use (apply choice l).
;;
;; Besides the usual trickery to support arbitrarily many arguments,
;; the implementation mostly consists of careful continuation
;; management and recursion over the list. If the list is empty, it
;; backtracks (calls the failure continuation). Otherwise, it succeeds
;; with the car of the list and a continuation that will try the
;; elements of the cdr of the list later.
;;
;; Sample solution is 6 lines.
(define ((choice . args) k_succ k_fail)
  (letrec ([f (lambda (l) (if (null? l)
                              (k_fail)
                              (k_succ (car l) (lambda() (f (cdr l)))) ))])
    (f args)))


;; Tests for choice

; (choice-run-all (choice 1 2 3))  ;; should show three successes and a failure


;; Problem 3d: Choice Combinators

;; The function `choice` above gives us a good way to make choices,
;; but to construct larger computations, we need ways of combining
;; choices together. These combinators will be somewhat analogous in
;; flavor to the parser combinators on hw3 (though very different in
;; implementation).


;; Start by defining choice-map, which takes a function f and a choice
;; c and returns the choice that acts like c, except with f applied to
;; all its successes.
;;
;; The implementation should call c with a modified success
;; continuation that applies f before calling the original success
;; continuation. The failure continuation can be passed through
;; unmodified.
;;
;; Sample solution is 2 lines.
(define ((choice-map f c) k_succ k_fail)
  (c (lambda(x y) (k_succ (f x) y)) k_fail))

;; Implement choice-map2, which is like choice-map, except that
;; it supports two choices (so f expects two arguments as well).
;;
;; Sample solution is 4 lines.
(define ((choice-map2 f c1 c2) k_succ k_fail)
  (c1 (lambda (x1 y1) (c2 (lambda (x2 y2) (k_succ (f x1 x2) y2)) y1)) k_fail))

;; Implement choice-compose, which takes a choice c and a
;; function f, and returns a new choice that passes the values
;; returned by c into f. More precisely, f is a function that expects
;; a value from c and returns *another choice*.
;;
;; The implementation calls c. If c succeeds, it passes the result to
;; f. If f later decides to backtrack, then choice-compose should
;; arrange for another value from c to be tried. If at any point all
;; values from c are exhausted, choice-compose backtracks.
;;
;; Sample solution is 3 lines.
(define ((choice-compose c f) k_succ k_fail)
  (c

   (lambda (x y)
       ;CHOICE  ;SUCCESS CONTINUATION  ;BACKTRACK CONTINUATION
       ( (f x) k_succ (lambda() (f (y)) k_succ k_fail) ) )
     
       k_fail))

(define (compose_test x)
  (if (number? x)
    (choice (+ x 1) (+ x 2) (+ x 3))
    x))

;; Implement choice-or, which takes two choices and returns a choice
;; representing the union of their values.
;;
;; Sample solution is 2 lines.
(define ((choice-or m1 m2) k_succ k_fail)
  (m1 k_succ (lambda() (m2 k_succ k_fail))))

;(choice-run-all (choice-or (choice 1 2 3) (choice 4 5 6)))
;; Implement choice-require, which takes a boolean. If the boolean is
;; true, choice-require succeeds with true. Otherwise, it backtracks.
;;
;; Sample solution is 3 lines.
(define ((choice-require p) k_succ k_fail)
  (if p
      (k_succ p k_fail)
      (k_fail)))

;; Problem 3e: Using choices to solve Sudoku

;; Our library of choices is great for implementing backtracking seach
;; algorithms. For example, one way to write a solver for many kinds
;; of logic puzzle is to simply try all possibilities. We will do this
;; for Sudoku puzzles. (If you are not familiar with the rules of
;; Sudoku, you may want to look them up briefly.)
;;
;; The function solve-sudoku takes a Sudoku board as an argument and
;; returns a choice representing all possible solutions to the
;; board.
;;
;; A board is represented as a list of cells, where a cell is a number
;; between 1 and n if it is filled in, or 0 if it is blank.
;;
;; We have implemented low-level board manipulation and validity
;; checking functions for you, and you do not need to know how they
;; work.
;;
;; All that remains is to construct the choice that loops over the
;; board and at each cell, makes a choice, fills it in, and then
;; checks if the board remains valid. You should use the combinators
;; from the previous problem. You should *not* need to use any
;; continuations explicitly; they will all be managed behind the
;; scenes by the combinators.

(define (solve-sudoku sq)
  (define n (sqrt (length sq)))
  (unless (exact? n) (error "sudoku board of non-square length"))

  (define k (sqrt n))
  (unless (exact? k) (error "sudokup board with non-square side length"))

  ;; We start with a bunch of library functions to manipulate boards
  ;; in various indexing schemes.

  (define (blank? x)
    (zero? x))

  (define (square-set sq idx val)
    (list-set sq idx val))

  (define (square-ref sq idx)
    (list-ref sq idx))

  (define (idx-row idx)
    (quotient idx n))

  (define (idx-col idx)
    (remainder idx n))

  (define (idx-box idx)
    (let* ([r (idx-row idx)]
           [c (idx-col idx)]
           [br (quotient r k)]
           [bc (quotient c k)])
      (+ (* br k) bc)))

  (define (make-idx-row-column r c)
    (+ (* r n) c))

  (define (make-idx-box-i b i)
    (let ([br (* (quotient b k) k)]
          [bc (* (remainder b k) k)]
          [ir (quotient i k)]
          [ic (remainder i k)])
      (make-idx-row-column
       (+ br ir)
       (+ bc ic))))

  ;; Predicates that check whether a particular row, box, or column is
  ;; valid (ie, does not contain duplicate entries).

  (define (row-distinct? sq r x)
    (for/and ([c (in-range n)])
      (let ([y (square-ref sq (make-idx-row-column r c))])
        (or (blank? y) (not (equal? x y))))))

  (define (col-distinct? sq c x)
    (for/and ([r (in-range n)])
      (let ([y (square-ref sq (make-idx-row-column r c))])
        (or (blank? y) (not (equal? x y))))))

  (define (box-distinct? sq b x)
    (for/and ([i (in-range n)])
      (let ([y (square-ref sq (make-idx-box-i b i))])
        (or (blank? y) (not (equal? x y))))))

  ;; Decides whether x is a valid guess to fill in blank cell at index
  ;; i by checking whether the row, column, or box containing i
  ;; already contains x.
  (define (valid-guess? sq i x)
    (and (row-distinct? sq (idx-row i) x)
         (col-distinct? sq (idx-col i) x)
         (box-distinct? sq (idx-box i) x)))


  ;; Now, to the main computation. Loop over all indices of the
  ;; board. At each index, lookup value in the cell at that index
  ;; using the provided square-ref function. If cell is already filled
  ;; in, just move on to the next index. Otherwise, if it is blank,
  ;; nondeterministically guess a value. Then assert that the guess
  ;; keeps the board valid by using the functions choice-require and
  ;; the provided valid-guess?. Finally, fill in the guess and move on
  ;; to the next index.
  ;;
  ;; The provided function choice-in-range is helpful for choosing a
  ;; number between 1 and n, as in:
  ;;     (choice-in-range 1 (+ n 1))
  ;;
  ;; Do not mutate the board, but update it "functionally" using the
  ;; provided square-set function. This is essential to avoid having
  ;; to "undo" side effects during backtracking.
  (define (loop sq i)
    (if (>= i (* n n))
        (choice sq)
        (if (blank? (square-ref sq i))
          
                (choice-compose (choice-compose (choice-in-range 1 (+ n 1)) choice)
                                (lambda(g)
                                  (choice-compose
                                  (choice-require (valid-guess? sq i g))
                                  (lambda(x) (loop (square-set sq i g) (+ i 1))))))
            (loop sq (+ i 1)))))

  ;; We start the computation off at the first index.
  (loop sq 0))

;; Here is an example Sudoku board for testsing.
(define board
  (apply
   append
   '((1 0 4 0 3 2 0 0 0)
     (0 2 0 0 7 6 9 4 1)
     (0 0 0 0 0 0 3 0 0)
     (0 0 3 2 1 0 7 5 0)
     (0 0 0 0 0 0 0 0 0)
     (0 5 1 0 4 9 6 0 0)
     (0 0 8 0 0 0 0 0 0)
     (6 4 7 1 8 0 0 9 0)
     (0 0 0 3 9 0 8 0 4))))

; solve the above board as follows:
;
;     (choice-run-all (choice-map square->list-of-lists (solve-sudoku board)))
;
; note that the use of choice-run-all is also implicitly checking that
; the initial board has a unique solution!


; Here is the right answer.
(define solution
  (apply
   append
   '((1 7 4 9 3 2 5 8 6)
     (3 2 5 8 7 6 9 4 1)
     (8 9 6 4 5 1 3 2 7)
     (4 6 3 2 1 8 7 5 9)
     (7 8 9 5 6 3 4 1 2)
     (2 5 1 7 4 9 6 3 8)
     (9 3 8 6 2 4 1 7 5)
     (6 4 7 1 8 5 2 9 3)
     (5 1 2 3 9 7 8 6 4))))



;; Provided helper code below.


;; Convert a Racket stream to a choice.
(define (stream->choice s)
  (if (stream-empty? s)
      (choice)
      (choice-or (choice (stream-first s))
                 (stream->choice (stream-rest s)))))

;; Like Racket's fancy in-range function.
(define (choice-in-range . args)
  (stream->choice (apply in-range args)))

;; Convert a list of length n * n into a list of lists of length n,
;; for easier reading.
(define (square->list-of-lists sq)
  (define n (sqrt (length sq)))

  (for/list ([i (in-range n)])
    (take (drop sq (* i n)) n)))
