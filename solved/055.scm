;; Lychrel numbers

(use-modules (euler utils))

(define (number-of-lychrel-numbers-under-n n)
  (let loop ((i 0) (acc 0))
    (if (>= i n) acc
	(loop (1+ i)
	      (if (lychrel? i) (1+ acc) acc)))))

(define (lychrel? n)
  (let loop ((i 2) (curr-n (+ n (number-reverse n))))
    (cond
     ((>= i 50) #t)
     ((palendromic? (number->string curr-n)) #f)
     (else (loop (1+ i) (+ curr-n (number-reverse curr-n)))))))

(display (number-of-lychrel-numbers-under-n 10000))
(newline)

