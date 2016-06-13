;; Counting range

;;; Answer is: 7295372

(define (make-already-seen-checker)
  (let ((hash-map (make-hash-table 100000000)))
    (lambda (n)
       (if (hash-ref hash-map (number->string n))
	   #t
	   (begin
	     (hash-set! hash-map (number->string n) n)
	     #f)))))


(define seen-before? (make-already-seen-checker))

(define (values-in-range min max max-denom)
  (let loop ((num 1) (denom 2) (acc 0))
    (let ((curr-frac (/ num denom)))
      (cond
       [(> denom max-denom) acc]
       [(>= num denom) (loop 1 (1+ denom) acc)]
       [(and (> curr-frac min)
	     (< curr-frac max)
	     (not (seen-before? curr-frac)))
	(loop (1+ num) denom (1+ acc))]
       [else (loop (1+ num) denom acc)]))
