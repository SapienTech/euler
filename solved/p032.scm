(define-module (euler solved p032))

(use-modules (srfi srfi-1)
             (euler utils))

(define (find-n-pandigital-products n)
  (let multiply-loop ((i 2) (j 1) (candidates '()))
    (let ((product (* i j)))
      (cond
       ((> (number-length i) (ceiling (/ n 2)))
	(delete-duplicates candidates))
       ((>= j i) (multiply-loop (1+ i) 1 candidates))
       ((> (multiply-length i j product) n) (multiply-loop (1+ i) 1 candidates))
       ((n-pandigital? (list i j product) n)
	(multiply-loop i (1+ j) (cons product candidates)))
       (else (multiply-loop i (1+ j) candidates))))))


(define (multiply-length i j product)
  (string-length (string-concatenate (map number->string (list i j product)))))

(define (n-pandigital? lst n)
  (lset= string=? (map number->string (iota 9 1))
	 (map string 
	      (string->list (string-concatenate
			     (if (number? (car lst))
				 (map number->string lst)
				 lst))))))
