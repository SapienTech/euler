;; Largest exponential
;; Pretty easy since guile can compare large numbers effectively

(define-module (solved p099))

(use-modules (euler utils)
	     (rnrs io ports))

(define (line-of-largest-exponent-in-file file)
  (1+ (list-maximum-index >
       (expt-log-pairs (get-pairs file)))))

;; What I  did originally, but takes much longer
(define (exponentiate-pairs pairs)
  (map (λ (pair idx) (apply expt pair))
       pairs
       (iota (length pairs))))

(define (expt-log-pairs pairs)
  (map (λ (pair) (* (cadr pair) (log (car pair)))) pairs))

(define (get-pairs file)
  (let ([port (open-input-file file)])
    (let lp ([line (get-line port)] [acc '()] )
      (if (eof-object? line) (reverse acc)
	  (lp (get-line port)
	      (cons
	       (map string->number (string-split line #\,))
	       acc))))))
