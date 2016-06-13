;; Integer right triangles

(use-modules (srfi srfi-1))

(define (find-most-triangles-with-same-perimeter max-perimeter)
  (let ((lst (vector->list (get-right-triangle-perimeter-list max-perimeter))))
    (reduce (lambda (curr-pair max-pair)
	      (if (> (car curr-pair) (car max-pair)) curr-pair max-pair))
	    '()
	    (zip lst (iota (length lst))))))
	   
(define (get-right-triangle-perimeter-list max-perimeter)
  (let ((perimeter-vector (make-vector (1+ max-perimeter) 0))
	(triangles (get-right-triangles max-perimeter)))
    (for-each (lambda (triangle)
		(let ((perimeter (reduce + 0 triangle)))
		  (vector-set! perimeter-vector
			       perimeter
			       (1+ (vector-ref perimeter-vector perimeter)))))
	      triangles)
    perimeter-vector))

(define (get-right-triangles max-perimeter)
  (let loop ((a 1) (b 1) (triangles '()))
    (let ((c (sqrt (+ (* a a) (* b b)))))
      (cond
       ((> b (ceiling (/ max-perimeter 2))) triangles)
       ((> (+ a b c) max-perimeter) (loop 1 (1+ b) triangles))
       ((> a b) (loop 1 (1+ b) triangles))
       ((not (= c (floor c))) (loop (1+ a) b triangles))
       (else (loop (1+ a) b (cons (list a b c) triangles)))))))


(display (find-most-triangles-with-same-perimeter 1000))
