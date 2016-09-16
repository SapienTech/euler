;; Permuted multiples

(define-module (euler solved p052))

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (wak foof-loop)
             (euler utils))

(define* (solve #:optional (multipliers (iota 5 2)))
  (min-permuted-multiple multipliers))

;;; TODO: better name/move logic to solve
(define (min-permuted-multiple multipliers)
  (loop continue ((for multiplicand (up-from 1)))
        (if (all-products-permutations? multiplicand multipliers)
            multiplicand
            (continue))))

(define (all-products-permutations? multiplicand multipliers)
  (fold-and (cut permuted-multiple? multiplicand <>)
            multipliers))

(define (permuted-multiple? num multiplier)
  (integer-permutation? num (* num multiplier)))

(define (integer-permutation? n1 n2)
  (and (= (number-length n1) (number-length n2))
       (digits=? n1 n2)))
