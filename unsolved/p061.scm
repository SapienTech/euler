; Cyclic Figurate Numbers

(define-module (unsolved p061))

(use-modules (srfi srfi-1)
	     (ice-9 control)
	     (ice-9 receive))

(define (triangle n)
  (/ (* n (+ n 1)) 2))

(define (square n)
  (* n n))

(define (pentagonal n)
  (/ (* n (- (* 3 n) 1)) 2))

(define (hexagonal n)
  (* n (- (* 2 n) 1)))

(define (heptagonal n)
  (/ (* n (- (* 5 n) 3)) 2))

(define (octagonal n)
  (* n (- (* 3 n) 2)))


(define (get-n-digit-set proc n)
  (let loop ((i 1) (set '()))
    (let* ((curr-val (proc i))
	   (curr-val-length (string-length (number->string curr-val))))
      (if (> curr-val-length n)
	  set
	  (loop (1+ i)
		(if (< curr-val-length n)
		    set
		    (cons curr-val set)))))))

(define (get-n-digit-sets proc-list n)
  (map (lambda (proc)
	 (get-n-digit-set proc n))
       proc-list))


;; really not sure about this program style...

(define (find-six-cyclic-4-digit-numbers-in proc-list return)
  (define sets (reverse (get-n-digit-sets proc-list 4)))
  (define (min-set-loop min-set rest)
    (if (null? min-set) '()
	(begin ;; can i put the return somewhere else?
	  (find-cyclic-nums-loop (list (car min-set)) rest)
	  (min-set-loop (cdr min-set) rest))))
  (define (find-cyclic-nums-loop cyclic-set candidate-sets)
    (let loop ((i 0))
      (cond
       ((= (length cyclic-set) (length sets)) ;; review cond behavior
	(if (cyclic? (first cyclic-set) (last cyclic-set))
	    (begin (display cyclic-set) (newline) (return cyclic-set)) cyclic-set))
       ((>= i (length candidate-sets)) cyclic-set) ;; returning to previous function
       (else (begin
	       (find-next-cyclic-num cyclic-set
				     (list-ref candidate-sets i) 
				     (append (take candidate-sets i) ;; double check this
					     (drop candidate-sets (1+ i))))
	       (loop (1+ i)))))))
  ;; should i use fold here?
  (define (find-next-cyclic-num cyclic-set candidate-set rest)
    (let loop ((candidate-set candidate-set))
      (cond
       ((null? candidate-set) #f)
       ((cyclic? (car cyclic-set) (car candidate-set))
	(begin
	  (find-cyclic-nums-loop (cons (car candidate-set) cyclic-set) rest)
	  (loop (cdr candidate-set))))
       (else (loop (cdr candidate-set))))))

  (receive (min-set rest)
      (extract-min-set sets)
      (display (length rest))
      (newline)
      (min-set-loop min-set rest)
      ))
	

(define (cyclic? val1 val2)
  (string=? (string-take (number->string val1) 2)
	   (string-take-right (number->string val2) 2)))

(define (set-analysis cyclic-set sets)
  (let loop ((cyc-set cyclic-set))
    (if (null? cyc-set) (begin (display cyclic-set) (newline))
	(if (unique? (car cyc-set) sets)
	    (loop (cdr cyc-set))
	    (display "failed")))))

(define (unique? value sets)
  (display 
	   (map (lambda (set)
		  (fold
		   (lambda (val acc)
		     (+ acc (if (= val value) 1 0)))
		   0
		   set))
		sets))
  (newline)
  (<= 1
     (fold +
	   0
	   (map (lambda (set)
		  (fold
		   (lambda (val acc)
		     (+ acc (if (= val value) 1 0)))
		   0
		   set))
		sets))))
  
(define (extract-min-set sets)
  (let ((ordered-sets
	 (fold (lambda (set ordered-sets)
		   (if (< (length set) (length (car ordered-sets)))
		       (cons set ordered-sets)
		       (append ordered-sets (list set))))
	       (list (car sets))
	       (cdr sets))))
    (values (car ordered-sets) (cdr ordered-sets))))
	  
(define (something?)
 (fold + 0
       (call/ec
	(lambda (return)
	  (find-six-cyclic-4-digit-numbers-in
	   (list triangle square pentagonal hexagonal heptagonal octagonal)
	   return)))))

(define (something-else?)
  (let ((candidate-sets '(1 2 3 4)))
    (display (list-ref candidate-sets 2)) (newline)
    (append (take candidate-sets 2) ;; double check this
	    (drop candidate-sets (1+ 2)))))
