;; Pentagon numbers

(define-module (unsolved p044))

(define pentagonal-list '())
(set! pentagonal-list
  (assoc-set! pentagonal-list "1" 100))
(display pentagonal-list)

(define (min-pair-that-satisfies proc)
  (let loop ((i 2) (j 1) (future-min 1) (curr-min 0))
    (cond
     ((>= j i) (loop (1+ i) 1 (get-future-min i) curr-min))
     ((and (not (zero? curr-min)) (> future-min curr-min)) curr-min)
     (else
      (loop i
	    (1+ j)
	    future-min
	    (let* ((p-i (pentagonal-number i))
		  (p-j (pentagonal-number j))
		  (p-sum (+ p-i p-j))
		  (p-diff (abs (- p-i p-j))))
	      (if (and (pentagonal? p-sum) (pentagonal? p-diff) (< p-diff curr-min))
		  p-diff
		  curr-min)))))))

(define (get-future-min i)
  (abs (- (pentagonal-number i) (pentagonal-number (1+ i)))))

(define (pentagonal? v)
  (let loop ((n 0))
    (let ((curr-pent (pentagonal-number n)))
      (cond
       ((= curr-pent v) #t)
       ((> curr-pent v) #f)
       (else (loop (1+ n)))))))

(define (pentagonal-number n)
  (/ (* n (- (* 3 n) 1)) 2))
