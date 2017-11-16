;; Quadratic primes
(ns (euler solved p027))
(use (argyle)
     ((euler primes) :select (prime?))
     (euler utils)
     ((srfi srfi-1) :select (reduce zip)))

(def p027-solve (b-max :o (c-max :or b-max))
  (->> (reduce >primes
               default-quad
               (gen-quads b-max c-max))
       prn-quad))

(data quad (a b c primes)
  :init (quad a b c)
  :app (fn (n)
         (+ (* (quad-a self) (^ n 2))
            (* (quad-b self) n)
            (quad-c self))))

(def default-quad (quad 0 0 0))

(def >primes (a b)
  (= a (calc-primes a))
  (= b (calc-primes b))
  (if (> (quad-primes a) (quad-primes b))
      a b))

(def gen-quads (b-max c-max)
  (collect-list (for b (up-from (- b-max) (to b-max)))
                (for c (up-from (- c-max) (to c-max)))
                (quad 1 b c)))

(def calc-primes (quad)
  (if (quad-primes quad) quad
      (loop ((for n (up-from 0))
             (while (prime? (quad n))))
        => (quad-primes! quad n))))

(def prn-quad (quad)
  (app format "n^2 + ~sn + ~s has ~s quadratic prime(s)"
       (map (\\ _ quad)
            `(,quad-b ,quad-c ,quad-primes))))
