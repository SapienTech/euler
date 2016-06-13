;; Powerful digit sum

(define-module (solved p056))

(use-modules (srfi srfi-1)
	     (euler utils))

(define (find-max-digital-sum limit)
  (reduce max 0
	(map (lambda (n)
	       (reduce + 0 (number->digits n)))
	     (get-expt-nums limit))))

(define (get-expt-nums limit)
  (let generator ([a 1] [b 1] [expt-nums '()])
    (cond
     [(>= a limit) expt-nums]
     [(>= b limit) (generator (1+ a) 1 expt-nums)]
     [else (generator a (1+ b) (cons (expt a b) expt-nums))])))
