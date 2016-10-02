;; Quadratic primes
(define-module (euler solved p027))
(use-modules (srfi srfi-1)
             (euler primes)
             (euler utils)
             (wak foof-loop)
             (wak foof-loop nested))

(define-public (p027-solve)
  (display-quadratic
   (quadratic-w/most-consecutive-primes 1000 1001)))

(define (display-quadratic quadratic)
  (format #f "n^2 + ~sn + ~s has ~s quadratic prime(s)"
          (second quadratic)
          (third quadratic)
          (first quadratic)))

(define* (quadratic-w/most-consecutive-primes a-max
                                           #:optional (b-max a-max))
  (reduce keep-quadratic-with>consecutive-primes
          min-quadratic
          (collect-consecutive-quadratic-primes a-max b-max)))

(define (keep-quadratic-with>consecutive-primes quad1 quad2)
  (let ((prime-count1 (car quad1))
        (prime-count2 (car quad2)))
    (if (> prime-count1 prime-count2) quad1 quad2)))

(define min-quadratic '(0 0 0))

;;; TODO: could have a better name, if not use a comment
(define (collect-consecutive-quadratic-primes a-max b-max)
  (collect-list (for a (up-from (- a-max) (to a-max)))
                (for b (up-from (- b-max) (to b-max)))
                (list (consecutive-quadratic-primes a b)
                      a b)))

(define (consecutive-quadratic-primes a b)
  (loop ((for n (up-from 0))
         (while (prime? (quadratic n a b))))
        => n))

(define (quadratic n a b)
  (+ (* n n) (* a n) b))
