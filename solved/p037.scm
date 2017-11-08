;; Truncable primes
(ns (euler solved p037))
(use (argyle)
     ((euler primes) #:select (prime?)))

;;; find sum of only 11 truncable primes
(def solve ()
  (app + (truncatable-primes)))

(def truncatable-primes ()
  (loop ((for n (up-from 10))
         (where primes '()
                (if (and (prime? n) (truncatable? n))
                    (cons n primes)
                    primes))
         (while (< (len primes) 11)))
    => primes))

;;; Expects n to be prime
(def truncatable? (n)
  (and-map prime? `(,@(truncables left-truncate n)
                    ,@(truncables right-truncate n))))

(def truncables (truncator n)
  (if (< n 10) '()
      (let n (truncator n)
        (cons n (truncables truncator n)))))

(def left-truncate (n) (num (str-drop (str n) 1)))
(def right-truncate (n) (num (str-take (str n)
                                       (1- (len (str n))))))
