;; Powerful digit counts

(define-module (solved p063))

(define (get-n-digit-n-powers)
  (define (power-loop power acc)
    (if (< 10000 power) ; Just using this arbitrary limit...
	acc
	(base-loop power 1 acc)))

  (define (base-loop power base acc)
    (let ([power-length (string-length (number->string (expt base power)))])

      (if (> power-length power)
	  (power-loop (1+ power) acc)
	  (base-loop power (1+ base)
		     (if (= power-length power) (1+ acc) acc)))))
  (power-loop 1 0))

(define (n-digit-n-power? number)
  (let ([n (string-length (number->string number))])
    (let loop ([i 1])
      (let ([power (expt i n)])
	(cond
	 [(> power number) #f]
	 [(= power number) #t]
	 [else (loop (1+ i))])))))
