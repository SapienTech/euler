;; Double-base palindromes

(define-module (solved p036))

(use-modules (euler utils)
	     (srfi srfi-1))

(define (find-double-base-palendromes limit)
  (let loop ((i 1) (palendromes '()))
    (if (= i limit) palendromes
	(loop (1+ i)
	      (if (and (palendromic? (number->string i))
		       (palendromic? (number->binary i)))
		  (cons i palendromes)
		  palendromes)))))

