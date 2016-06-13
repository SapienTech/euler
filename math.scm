(define-module (euler math))

(define-public (factorial n)
  (let loop ((i 1) (acc 1))
    (if (> i n) acc
	(loop (1+ i) (* i acc)))))

;; Fast enough for single calculations
(define-public (proper-divisors n)
  (let lp ([i (inexact->exact (floor (/ n 2)))] [divisors '()])
    (if (zero? i) divisors
	(lp (1- i)
	    (if (zero? (modulo n i))
		(cons i divisors)
		divisors))))) 


;; I'm curious what's faster: setting, or remaking...
;; I should really learn how to do speed tests...
(define-public (proper-divisors-in-range limit)
  (define divisor-array (make-array '() (1+ limit)))
  (define (add-divisor-lp  divisor)
    (let lp ([div-multiple (+ divisor divisor)])
      (unless (> div-multiple limit)
	(array-set! divisor-array
		    (cons divisor
			  (array-ref divisor-array
				     div-multiple))
		    div-multiple)
	(lp (+ div-multiple divisor)))))
  (do [(i 1 (1+ i))]
      [(> i limit) divisor-array]
    (add-divisor-lp i)))

(define-public (relative-primes n)
  (let loop ([i 1] [acc '()])
    (if (> i n) acc
	(loop (1+ i)
	      (if (= 1 (gcd n i))
		  (cons i acc)
		  acc)))))

;; DEPRECATED, it seems like guile already implemented a fast expt function
(define-public (expt-fast base step)
  (let lp ([base base] [step step] [acc 1])
    (cond
     [(zero? step) 1]
     [(= 1 step) (* acc base)]
     [else
      (lp (* base base)
	  (if (even? step) (/ step 2)
	      (/ (1- step) 2)) 
	  (if (even? step) acc
	      (* acc base)))])))
