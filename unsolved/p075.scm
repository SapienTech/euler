;; Singular integer right triangles

;; Well we could simply enumerate a^2 + b^2, check to see if that is a perfect square...

;; Okay, we will go through all a and b's recording each value in a hashtable.
;; If value doesn't exits, add it and set to true, otherwise set to false.
;; The issue with this is that we may get some false positives if we don't ensure that a^2 + b^2 = c^2
;; does not work to simpliy loop through a and b
;; How can we know when this works... well c must be greater than a + b and if c must be an integer, then, it must be the case that a and b are integers... and that there sqrt-sum must be an integer... so we are dealing with perfect squares, 


;; Well it seems that we can do some kind of sieve...
;; How about some kind of binary search?

(define-module (unsolved p075))

(define (calc-test range)
  (do [(i 0 (1+ i))]
      [(> i (/ range 3))]
    (do [(j 0 (1+ j))]
	[(> j i)]
      (* i j))))

(define (singular-right-triangles range)
  (define triangle-map (make-hash-table range))
  0)
