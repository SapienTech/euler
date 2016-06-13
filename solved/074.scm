;;; Digit factorial chains

;; TODO: modify names (! for state), add comments, move useful proc to utils.scm
;; Used features: hash-table, define*, match

(use-modules (srfi srfi-1)
	     (ice-9 match)
	     (euler utils)
	     (euler math))

(define (find-n-digit-factorial-chain-under-k n k)
  (fold (lambda (digit acc)
	  (if (= n (get-chain-length digit))
	      (cons (get-val digit) acc)
	      acc))
	'()
	(get-factorial-chain-digits-under k)))

(define (get-factorial-chain-digits-under k)
  (do [(i 1 (1+ i))]
      [(>= i k) (filter-map->list/w-digits-under k)]
    (find-cycle-loop i '())))

(define (filter-map->list/w-digits-under k)
  (hash-fold
   (lambda (key digit acc)
     (if (< (get-val digit) k)
	 (cons digit acc)
	 acc))
   '()
   digit-map))

(define digit-map (make-hash-table 100))

(define (find-cycle-loop curr-val curr-chain)
  (let ((digit? (hashq-ref digit-map curr-val)))
    (match digit?
      [#f ; -> not seen yet
       (let ([digit (make-digit curr-val)])
	 (hashq-set! digit-map curr-val digit)
	 (find-cycle-loop (get-fact-sum digit)
			  (cons digit curr-chain)))]
      [(_ _ #f) ; -> seen before in current loop
       (update-chain-and-cycle digit? curr-chain)]
      [(_ _ chain-length) ; -> seen before in a previous loop
       (update-chain curr-chain chain-length)])))

(define (update-chain-and-cycle digit curr-chain)
  (call-with-values
      (lambda ()
	(split-at curr-chain
		  (1+ (find-index curr-chain digit))))
    (lambda (cycle chain)
      (update-cycle cycle)
      (update-chain chain (length cycle)))))

;; setting chain-length for values in the chain's cycle
(define (update-cycle cycle)
  (let ([cycle-length (length cycle)])
    (update-digits cycle
		   (make-list cycle-length cycle-length))))

;; setting chain-length for values in the chain before a cycle
(define (update-chain chain cycle-length)
  (update-digits chain (iota (length chain) (1+ cycle-length))))

(define (update-digits digits chain-lengths)
  (for-each
   (lambda (digit chain-length)
     (hashq-set! digit-map
		 (get-val digit)
		 (set-chain-length digit chain-length)))
   digits chain-lengths))

;; Definitions for digit structure

(define* (make-digit n #:optional (fact-sum (factorial-sum n)) (chain-length #f))
  (list n fact-sum chain-length))

(define (factorial-sum n)
  (reduce + 0
	  (map factorial (number->digits n))))

(define (get-chain-length digit)
  (last digit))

(define (set-chain-length digit chain-length)
  (make-digit (get-val digit)
	      (get-fact-sum digit)
	      chain-length))

(define (get-fact-sum digit)
  (cadr digit))

(define (get-val digit)
  (car digit))

;; For testing individual numbers
(define (t i)
  (hash-clear! digit-map)
  (find-cycle-loop i '())
  (hashq-ref digit-map i))
