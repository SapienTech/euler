;; Quadratic primes
(define-module (euler solved p027))
(use-modules (euler primes)
             (euler utils)
             (wak foof-loop)
             (wak foof-loop nested))

(define-public (p027-solve)
  (max-consecutive-primes 1000 1001))

(define* (max-consecutive-primes a-range #:optional (b-range a-range))
  (apply max
         `(,list>
           ,@(collect-list
              (for a (up-from (- a-range) (to a-range)))
              (for b (up-from (- b-range) (to b-range)))
              (list (consecutive-primes a b) a b)))))

;;; TODO: make more general list>
(define (list> l1 l2)
  (> (car l1) (car l2)))

(define (consecutive-primes a b)
  (loop ((for n (up-from 0))
         (while (prime? (quadratic n a b))))
        => n))

(define (quadratic n a b)
  (+ (* n n) (* a n) b))
