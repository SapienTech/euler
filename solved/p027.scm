;; Quadratic primes
(ns (euler solved p027))
(use (argyle)
     (euler primes)
     (euler utils)
     ((srfi srfi-1) :select (reduce)))

(def p027-solve (a-max :o (b-max :or a-max))
  (->> (reduce >primes
               `((quad 0 0 0) 0)
               (map calc-primes (quads a-max b-max)))
       prn-quad))

(data quad (a b c)
  :app (fn (n)
         (+ (* n n)
            (* (quad-a self) n)
            (quad-b self))))

(def prn-quad ((quad primes))
  (app format #f "~sn^2 + ~sn + ~s has ~s quadratic prime(s)"
         `(,@(map (\\ _ quad)
                  `(,quad-a ,quad-b ,quad-c))
           ,primes)))

(def >primes (((_ p1) :as a) ((_ p2) :as b))
  (if (> p1 p2) a b))

(def quads (a-max b-max)
  (collect-list (for a (up-from (- a-max) (to a-max)))
                (for b (up-from (- b-max) (to b-max)))
                (quad 1 a b)))

(def calc-primes (quad)
  (loop ((for n (up-from 0))
         (while (prime? (quad n))))
      => `(,quad ,n)))
