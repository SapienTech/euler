;; Distinct Prime Factors

(define-module (solved p047))

(use-modules (euler factor)
	     (srfi srfi-1))

(define (n-consecutive-vals-w/-k-distinct-prime-factors n k)
  (let lp ([i 1] [acc '()])
    (cond
     [(= n (length acc)) acc]
     [(= k (length (delete-duplicates (trial-factor i))))
      (lp (1+ i) (cons i acc))]
     [else (lp (1+ i) '())])))
