;; Ordered fractions (71->73)
(use-modules (srfi srfi-1))

(define (ordered-fractions max-denom)
  (let loop ((num 1) (denom 2) (acc '()))
     (cond
      ((>= denom max-denom) acc)
      ((>= num denom)
       ;(if (zero? (modulo denom 1000)) (begin (display "denom: ") (display denom) (newline)) #f)
       (loop 1 (1+ denom) acc))
      (else (loop (1+ num) denom (cons num acc))))))

(define (left-of-frac frac max-denom)
  (let loop ((num 1) (denom 2) (curr-best 0))
    (cond
     ((>= denom max-denom) curr-best)
     ((>= num denom) (loop 1 (1+ denom) curr-best))
     (else
      (loop
       (1+ num) denom
       (let ((best-diff (- frac curr-best))
	     (curr-diff (- frac (/ num denom))))
	 (cond
	  ((zero? curr-diff) curr-best)
	  ((< curr-diff 0) curr-best)
	  ((> curr-diff best-diff) curr-best)
	  (else (/ num denom)))))))))


(define (enumeration-test max-denom)
  (let loop ((num 1) (denom 2))
    (cond
     ((> denom max-denom) #f)
     ((> num denom)
      (if (zero? (modulo denom 10000)) (begin (display denom) (newline)) #f)
      (loop 1 (1+ denom)))
     (else (loop (1+ num) denom)))))

(display (enumeration-test (expt 10 6)))

(define (fractions-between-fast min-frac max-frac max-denom)
  (let loop ((num 1) (denom 2) (fracs-between '()))
    (cond
     ((> denom max-denom) fracs-between)
     ((> num denom) (loop 1 (1+ denom) fracs-between))
     ((let ((curr-frac (/ num denom)))
	(and (> curr-frac min-frac)
	     (< curr-frac max-frac)
	     (not (memq curr-frac fracs-between))))
      (display (length fracs-between)) (newline)
      (loop (1+ num) denom (cons (/ num denom) fracs-between)))
     (else (loop (1+ num) denom fracs-between)))))
  
(define (fractions-between min-frac max-frac max-denom)
  (define ordered-fracs (delete-duplicates-sorted
			 (sort (ordered-fractions max-denom) <)))
  (let loop ((curr-lst ordered-fractions) (fracs-between '()))
    (if (null? curr-lst) fracs-between
	(let ((curr-frac (car ordered-fractions)))
	  (cond
	   ((> curr-frac max-frac) fracs-between)
	   ((and (> curr-frac min-frac) (< curr-frac max-frac))
	    (loop (cdr ordered-fractions) (cons curr-frac fracs-between)))
	   (else (loop (cdr ordered-fractions) fracs-between)))))))

(define (number-of-generated-fractions max-denom)
  (define fraction-dict '())
  0)

(define (delete-duplicates-sorted lst)
  (let loop ((curr-lst lst) (unique-lst '()))
    (cond
     ((null? curr-lst) unique-lst)
     ((null? (cdr curr-lst)) (cons (car curr-lst) unique-lst))
     (else (loop (cdr curr-lst)
		 (if (= (car curr-lst) (cadr curr-lst))
		     unique-lst
		     (cons (car (curr-lst)) unique-lst)))))))

;; (display (left-of-frac (/ 3 7) (expt 10 4)))
;;(display (left-of-frac (/ 3 7) (expt 10 6)))
;;(display (fractions-between-fast (/ 1 3001) (/ 1 3000) 12000))

