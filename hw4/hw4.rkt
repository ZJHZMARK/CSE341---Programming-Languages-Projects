#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;Problem 1
(define (sequence g l h)
  (if (< h l)
      null
      (cons l (sequence g (+ l g) h))))

;Problem 2
(define (string-append-map xs suffix)
  (if (null? xs)
      null
      (map (lambda(i) (string-append i suffix)) xs)))

;Problem 3
(define (list-nth-mod xs n)
  (cond
      [(< n 0) (error "list-nth-mod: negative number")]
      [(null? xs) (error "list-nth-mod: empty list")]
      [#t (car(list-tail xs (remainder n (length xs))))]))

;Problem 4
(define (stream-for-k-steps s k)
 (letrec ([f (lambda (s k)
              (let ([pr (s)])
                (if (= k 0)
                     (list (car pr))
                     (cons (car pr) (f (cdr pr) (- k 1))))))]) 
    (f s k)))

;Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 6) 0)
                    (cons (- 0 x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])                               
    (lambda () (f 1))))

;Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (equal? "dog.jpg" x)
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))))])                               
    (lambda () (f "dan.jpg"))))

;Problem 7
(define (stream-add-one s)
  (letrec ([f (lambda (s)
              (let ([pr (s)])
                (cons (cons 1 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

;Problem 8 (and challenge problem 11) 
(define (cycle-lists xs ys)
  (letrec ([f (lambda (xl yl)
               (cond
                 [(and (null? xl) (null? yl)) (f xs ys)] ;If both are empty
                 [(and (null? xl) (not (null? yl))) (f xs yl)] ;If first list is empty
                 [(and (not (null? xl)) (null? yl)) (f xl ys)] ;If second list is empty
                 [#t (cons (cons (car xl) (car yl)) (lambda () (f (cdr xl) (cdr yl))))]))])                                                              
    (lambda () (f xs ys))))

;Problem 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (v vec n)
                (if (= n (vector-length vec))
                    #f
                    (cond
                      [(not (pair? (vector-ref vec n))) (f v vec (+ n 1))]
                      [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                      [#t (f v vec (+ n 1))])))])
              (f v vec 0)))

;Problem 10
(define (caching-assoc xs n)
  (letrec([memo (make-vector n)]
          [c-pos 0]
          [f (lambda (v)
               (let ([ans (vector-assoc v memo)])
                 (if ans 
                     (begin (print "cache") ans)
                     (let ([new-ans (assoc v xs)])
                       (if new-ans
                           (begin
                             (vector-set! memo c-pos new-ans)
                             (if (= c-pos (- n 1)) (set! c-pos 0) (set! c-pos (+ c-pos 1)))
                             (begin (print "new") new-ans))
                           new-ans)))))])
    f))
