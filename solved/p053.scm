;; Combinatoric selections

(define-module (solved p053))

(use-modules (euler math))

(define (get-combinations>val-with-n min-val n)
  (filter (lambda (combination)
	    (> combination min-val))
	  (get-combinations n)))

(define (get-combinations limit)
  (let generator ((n 1) (r 1) (combinations '()))
    (cond
     ((> n limit) combinations)
     ((> r n) (generator (1+ n) 1 combinations))
     (else (generator n (1+ r) (cons (combination n r) combinations))))))

(define (combination n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))
