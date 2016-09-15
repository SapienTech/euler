(define-module (euler utils))

(use-modules (srfi srfi-1)
             (wak foof-loop))

(define-public (lst-index lst item)
  (let loop ([i 0] [curr-lst lst])
    (cond
     [(null? curr-lst) #f]
     [(equal? item (car curr-lst)) i]
     [else (loop (1+ i) (cdr curr-lst))])))

(define-public (number->digits n)
  (map string->number
       (map string (string->list (number->string n)))))

(define-public (digits->number digits)
  (string->number
       (string-concatenate
	(map number->string digits))))

(define-public (number-length n)
  (string-length (number->string n)))

(define-public (digits>? n1 n2)
  (> (number-length n1) (number-length n2)))

(define-public (palendromic? s)
  (string=? s (string-reverse s)))

(define-public (pandigital? n)
  (let ([digits (number->digits n)])
    (lset= = digits (iota (length digits) 1))))

(define-public (truth-and-fold proc lst)
  (fold (lambda (item acc)
          (and acc (proc item)))
        #t
        lst))

(define-public (truth-or-fold proc lst)
  (let lp ([lst lst])
    (cond
     [(null? lst) #f]
     [(proc (car lst)) #t]
     [else (lp (cdr lst))])))

;; TODO: eventually generalize this using a macro
(define-public (number-append n1 n2)
  (string->number
   (string-append (number->string n1)
		  (number->string n2))))

(define-public (number-reverse n)
  (string->number (string-reverse (number->string n))))

(define-public (permutation? n1 n2)
  (equal? (sort (number->digits n1) <)
	  (sort (number->digits n2) <)))

(define-public (list-comp start end)
  (let lp ([curr start] [acc '()])
    (if (> start end) (reverse acc)
	(lp (1+ start) (cons curr acc)))))

;; Find the index of value that is max based on compare-proc
(define-public (list-maximum-index compare-proc vals)
  (let lp ([curr-index 1] [curr-max (car vals)] [curr-max-index 0] [vals (cdr vals)])
    (cond
     [(null? vals) curr-max-index]
     [(compare-proc (car vals) curr-max)
      (lp (1+ curr-index) (car vals) curr-index (cdr vals))]
     [else (lp (1+ curr-index) curr-max curr-max-index (cdr vals))])))

(define-public (number->binary n)
  (let ([array-length (inexact->exact (floor (/ (log n) (log 2))))])
    (let loop ([i array-length] [remdr n] [b-list '()])
      (if (< i 0) (string-concatenate (reverse b-list))
	  (let ([n-remdr (expt 2 i)])
	    (if (<= n-remdr remdr)
		(loop (1- i) (- remdr n-remdr) (cons "1" b-list))
		(loop (1- i) remdr (cons "0" b-list))))))))

;;; Add hash-table foof-loop support
(define-public (categorize list cat-proc vector-size) 
  (loop ((for elt (in-list list))
         (with index 0 (cat-proc elt))
         (with vec (make-vector (1+ vector-size) '())
               (begin
                 (vector-set! vec index
                              (cons elt
                                    (vector-ref vec index)))
                 vec)))
        => vec))

(define (min-length vs)
  (let loop ((vs (cdr vs))
             (result (vector-length (car vs))))
    (if (null? vs)
        result
        (loop (cdr vs) (min result (vector-length (car vs)))))))

(define-syntax-rule (assert-procedure f who)
  (unless (procedure? f)
    (error-from who "expected procedure, got" f)))

(define-syntax-rule (assert-vector v who)
  (unless (vector? v)
    (error-from who "expected vector, got" v)))

(define-syntax-rule (assert-vectors vs who)
  (let loop ((vs vs))
    (unless (null? vs)
      (assert-vector (car vs) who)
      (loop (cdr vs)))))

(define (error-from who msg . args)
  (apply error
         (string-append (symbol->string who) ": " msg)
         args))

(define-public vector-map-no-idx
  (case-lambda
    ((f v)
     (assert-procedure f 'vector-map)
     (assert-vector v 'vector-map)
     (let* ((len (vector-length v))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (f (vector-ref v i)))
           (loop (+ i 1))))
       result))
    ((f v1 v2)
     (assert-procedure f 'vector-map)
     (assert-vector v1 'vector-map)
     (assert-vector v2 'vector-map)
     (let* ((len (min (vector-length v1) (vector-length v2)))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (f (vector-ref v1 i) (vector-ref v2 i)))
           (loop (+ i 1))))
       result))
    ((f . vs)
     (assert-procedure f 'vector-map)
     (assert-vectors vs 'vector-map)
     (let* ((len (min-length vs))
            (result (make-vector len)))
       (let loop ((i 0))
         (unless (= i len)
           (vector-set! result i (apply f (vectors-ref vs i)))
           (loop (+ i 1))))
       result))))

(define (vectors-ref vs i)
  (let loop ((vs vs) (xs '()))
    (if (null? vs)
        (reverse! xs)
        (loop (cdr vs) (cons (vector-ref (car vs) i)
                             xs)))))
