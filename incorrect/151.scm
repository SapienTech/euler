;; Paper sheets of standard size: an expected value problem


;; TODO: rewrite to make sense of batches. for instance, if we run out of paper before the last batch, then we should count the last paper as a hit.


;; TODO: how should this problem be written? I want to keep track of the cache outside of the functions so I can add it in later, but this leads to a lot of ugly code... (a lot of passing around the cache
(use-modules (srfi srfi-1))
(use-modules (ice-9 receive))

(define (solve)
  (expected-value (sheet-problem 15 5)))

(define (expected-value soln-set)
  (display soln-set)
  (if (not (zero? (cadr soln-set)))
      (exact->inexact (/ (car soln-set) (cadr soln-set)))
      0))

(define (sheet-problem total-batches number-of-cuts)

  (define cache '())

  (define (can-use-sheet? sheet)
    (= sheet number-of-cuts))
  
  (define (cut-sheet sheet)
    (cut-sheet-to-size sheet number-of-cuts))
  
  (define (last-batch? curr-batch)
    (= total-batches curr-batch))
  
  (define (batch-loop envelope curr-batch soltn-set)
    (cond
     ((or (last-batch? curr-batch)
	  (empty? envelope))
      soltn-set)
     ;; Not sure if we need this condition now...
     ((only-one-sheet? envelope)
      (if (can-use-sheet? (get-sheet envelope))
	  soltn-set
	  (batch-loop (cut-sheet (get-sheet envelope))
		      (1+ curr-batch)
		      (add-hit soltn-set)))); not sure if we add a hit here...
     (else (pick-sheets-loop envelope curr-batch (add-miss soltn-set)))))

  ;; TODO: consider not having an inner loop?
  ;; Does cache get remembered?
  (define (pick-sheets-loop envelope curr-batch soltn-set)
    (define (get-sub-soltn-set sheet rest-of-envelope)
      (let* ((n-envelope
	      (if (can-use-sheet? sheet) rest-of-envelope
		  (append (cut-sheet sheet) rest-of-envelope)))
	     (key (envelope->key n-envelope)))
	(if (assoc key cache) (assoc-ref cache key)
	    (begin
	      (set! cache
		(acons key
		       (batch-loop n-envelope (1+ curr-batch) '(0 0))
		       cache))
	      (assoc-ref cache key)))))
    (let pick-sheet-loop ((i 0) (curr-soltn-set soltn-set))
      (if (= i (sheets-in-envelope envelope)) curr-soltn-set
	  (receive (sheet rest-of-envelope)
	      (pick-sheet i envelope)
	    (pick-sheet-loop (1+ i)
			     (combine-soltn-sets
			      curr-soltn-set
			      (get-sub-soltn-set sheet rest-of-envelope)))))))
  (batch-loop (init-envelope number-of-cuts) 1 '(0 0)))

(define (pick-sheet i envelope)
  (values (list-ref envelope i) (append (take envelope i) (drop envelope (1+ i)))))

(define (init-envelope smallest-sheet-size)
  (cut-sheet-to-size 1 smallest-sheet-size))

;; TODO: not convinced by this name
;; TODO: see if there is a better way to express this...
(define (cut-sheet-to-size sheet final-size)
  (if (= sheet final-size)
      (list sheet)
      (receive (half1 half2)
	  (cut-sheet-in-half sheet)
	(cons half1 (cut-sheet-to-size half2 final-size)))))

(define (cut-sheet-in-half sheet)
  (values (1+ sheet) (1+ sheet)))

; (display (cut-sheet-to-size 1 5))

(define (only-one-sheet? envelope)
  (= 1 (length envelope)))

(define (sheets-in-envelope envelope)
  (length envelope))

(define (empty? envelope)
  (null? envelope))

(define (get-sheet envelope)
  (car envelope))

(define (get-sheets envelope)
  envelope)

(define (add-hit soltn-set)
  (map 1+ soltn-set))

(define (add-miss soltn-set)
  (list (car soltn-set)
	(1+ (cadr soltn-set))))


(define (envelope->key envelope)
  (fold string-append "" (map number->string (sort envelope <))))

;(display (envelope->key '(2 1 3)))

(define (combine-soltn-sets set1 set2)
  (map + set1 set2))

;(display (combine-soltn-sets '(1 2) '(2 2)))

(display (solve))


;; Thise examples show how we can't rely on passing the cache in as a parameter since it is always pass by value
(define (cache-test1)
  (define (outer-loop cache)
    (set! cache (acons "hi" (inner-loop cache) cache))
    (display cache)
    0)
  (define (inner-loop cache)
    (set! cache (acons "there" "answer2" cache))
    cache)
  (outer-loop '()))

;; (cache-test1)

(define (cache-test2)
  (define (outer-loop cache)
    (begin
      (inner-loop cache)
      (display cache)
      0))
  (define (inner-loop cache)
    (set! cache (acons "there" "answer2" cache))
    cache)
  (outer-loop '()))

;; (cache-test2)
;;Okay seriously wtffff
(define (cache-test3)
  (define (outer-loop cache)
    (let ((n-value (inner-loop cache))) ; lets hope one of the side effect is that the cache has changeddddddddd
      (newline)
      (display "cache after n-value")
      (display cache)
      (newline)
      (set! cache (acons "hi" n-value cache))
      (newline)
      (display "cache after set")
      (display cache)
      (newline)0))
  (define (inner-loop cache)
    (set! cache (acons "there" "answer2" cache))
    cache)
  (outer-loop '()))

;; (cache-test3)


(define (cache-test4)
  (letrec* ((cache '())
	 (outer-loop
	  (lambda ()
	    (newline)
	    (display "cache after n-value")
	    (display cache)
	    (newline)
	    (set! cache (acons "hi" (inner-loop) cache))
	    (newline)
	    (display "cache after set")
	    (display cache)
	    (newline)0))
	 (inner-loop
	  (lambda ()
	    (set! cache (acons "there" "answer2" cache))
	    "answer1")))
    (outer-loop)))

;; (cache-test4)


(define (cache-test5)
  (let ((cache '()))
    (define (outer-loop)
      (begin
	(set! cache (acons "hi" (inner-loop) cache))
	(display cache)
	0))
    (define (inner-loop)
      (set! cache (acons "there" "answer2" cache))
      "answer1")
    (outer-loop)))


;;(newline)
;;(cache-test5)

    ;; HUH, this is really sutble stuff!!!
