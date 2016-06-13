;; Pandigital products

(define-module (another test))

(use-modules (srfi srfi-1))

(define (find-n-pandigital-products n)
  (let multiply-loop ((i 2) (j 1) (candidates '()))
    (let ((product (* i j)))
      (cond
       ((> (string-length (number->string i)) (ceiling (/ n 2)))
	(delete-duplicates candidates))
       ((>= j i) (multiply-loop (1+ i) 1 candidates))
       ((> (multiply-length i j product) n) (multiply-loop (1+ i) 1 candidates))
       ((pandigital? (list i j product) n)
	(multiply-loop i (1+ j) (cons product candidates)))
       (else (multiply-loop i (1+ j) candidates))))))


(define (multiply-length i j product)
  (string-length (string-concatenate (map number->string (list i j product)))))

(define (pandigital? lst n)
  (lset= string=? (map number->string (iota 9 1))
	 (map string 
	      (string->list (string-concatenate
			     (if (number? (car lst))
				 (map number->string lst)
				 lst))))))
