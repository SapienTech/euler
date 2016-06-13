(define-module (euler generators))

(define-public (make-convergent-frac-generator)
  (let ((*k* #f))
    (lambda (n)
      (let ((term (if *k* (*k* n) n)))
	(call-with-prompt
	  'expand
	  (lambda ()
	    (if *k*
		(*k* (+ n (/ 1 (abort-to-prompt 'expand))))
		(+ n (/ 1 (abort-to-prompt 'expand)))))
	  (lambda (k)
	    (set! *k* k)))
	term))))

(define-public (linear-congruential-generator values)
  (define expt1 (expt 2 20))
  (define expt2 (/ expt1 2))
  (let loop ([k 1] [t 0] [generated-values '()])
    (if (> k values) (reverse generated-values)
	(let ([t+1 (modulo (+ (* t 615949) 797807)
			   expt1)])
	  (loop (1+ k) t+1
		(cons (- t+1 expt2)
		      generated-values))))))
