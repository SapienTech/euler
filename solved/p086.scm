;; Cuboid route

(define-module (unsolved p086))

;; a = height, b = length, c = width
(define (find-min-path a b c)
  (let ([x (/ (* a b) (+ a c))])
    (+ (sqrt (+ (expt a 2) (expt x 2)))
       (sqrt (+ (expt c 2) (expt (- b x) 2))))))


;; Okay i see something interesting with gcd...
;; All new values that have integer min paths have three numbers that are relatively prime to eachother
;; Going to work on the common divisors, (totient stuff) and put it in the math module
;; I haven't been able to figure out the totient stuff, so work on this problem will wait
(define-public (cuboids-with-integer-min-path M)
  (let lp ([a 1] [b 1] [c 1] [a-gcd #f] [acc 0])
    (cond
     [(> a M) acc]
     [(> b a) (lp (1+ a) 1 1 acc)]
     [(> c b) (lp a (1+ b) 1 acc)]
     [else
      (lp a b (+ (modulo ))
	  (if (integer? (min (find-min-path a b c)
			     (find-min-path c a b)
			     (find-min-path b c a)))
	      (begin
		(display (list a b c))
		(newline)
		(1+ acc))
	      acc))])))
