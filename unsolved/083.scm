;; Path sum: four ways
;; Using dijkstras algorithm to find shortest path from top left to bottom right

(use-modules (ice-9 receive)
	     (rnrs io ports)
	     (srfi srfi-1))


(define (dijkstras graph start directions)

  (define min-path-array (make-array #f (array-length graph) (array-length graph)))

  (define direction-proc-lst (filter-map (lambda (yes? dir-proc)
					   (if yes? dir-proc #f))
					 directions
					 base-dir-procs))

  (define (vertex-loop vertices)
    (if (null? vertices) min-path-array
	(vertex-loop (update-path-array vertices))))

  (define (update-path-array perimeter-vertices)
    (receive (min-vertex rest) (get-min-vertex perimeter-vertices)
       (for-each
	(lambda (neighbor)
	  (array-set!
	   min-path-array
	   (let* ((neighbor-val (array-ref min-path-array
					   (car neighbor) (cadr neighbor)))
		  (min-v-val (array-ref min-path-array
					(car min-vertex) (cadr min-vertex)))
		  (min-v->neigh-val (array-ref graph
					       (car neighbor) (cadr neighbor))))
	     (if (and neighbor-val min-v-val (< neighbor-val (+ min-v-val min-v->neigh-val)))
		 neighbor-val (if min-v-val (+ min-v-val min-v->neigh-val) 0)))
	   (car neighbor) (cadr neighbor)))
	(get-neighbors graph direction-proc-lst min-vertex))
       rest))
	    
  (define (get-min-vertex perimeter-vertices)
    (define vert-array (list->array 1 perimeter-vertices))
    (let loop ((i 1)
	       (min-vert (array-ref vert-array 0))
	       (rest '()))
      (if (>= i (array-length vert-array))
	  (values min-vert rest)
	  (let* ((curr-vert (array-ref vert-array i))
		 (curr-val (array-ref min-path-array (car curr-vert) (cadr curr-vert)))
		 (min-val (array-ref min-path-array (car min-vert) (cadr min-vert))))
	    (cond
	     ((not curr-val) (loop (1+ i) min-vert (cons curr-vert rest)))
	     ((not min-val) (loop (1+ i) curr-vert (cons min-vert rest)))
	     ((<= curr-val min-val) (loop (1+ i) curr-vert (cons min-vert rest)))
	     (else (loop (1+ i) min-vert (cons curr-vert rest))))))))
	    
  (begin
    (array-set! min-path-array
		(array-ref graph (car start) (cadr start))
		(car start) (cadr start))
    (vertex-loop (get-index-list graph))))


(define (get-index-list array)
  (define length (array-length array))
  (let loop ((i 0) (j 0) (acc '()))
    (cond
     ((>= i length) acc)
     ((>= j length) (loop (1+ i) 0 acc))
     (else (loop i (1+ j) (cons (list i j) acc))))))

(define (get-l-neighbor graph point)
  (neighbor? graph (1- (car point)) (cadr point)))
(define (get-r-neighbor graph point)
  (neighbor? graph (1+ (car point)) (cadr point)))
(define (get-u-neighbor graph point)
  (neighbor? graph (car point) (1+ (cadr point))))
(define (get-d-neighbor graph point)
  (neighbor? graph (car point) (1- (cadr point))))

(define (neighbor? graph x y)
  (if (array-in-bounds? graph x y) (list x y) #f))

(define base-dir-procs (list get-l-neighbor
			   get-r-neighbor
			   get-u-neighbor
			   get-d-neighbor))


(define (get-neighbors graph direction-proc-lst point)
  (filter-map (lambda (dir-proc)
	 (dir-proc graph point))
       direction-proc-lst))

(define default-array
  (list->array 2
	       (list
		(list 131 673 234 103 18)
		(list 201 96 342 965 150)
		(list 630 803 746 422 111)
		(list 537 699 497 121 956)
		(list 805 732 524 37 331))))

(define (make-matrix-from-file file)
  (let ((port (open-input-file file)))
    (let loop ((line (get-line port)) (lst '()))
      (if (eof-object? line) (list->array 2 (reverse lst))
	  (loop (get-line port) (cons (line->number-list line) lst))))))

(define (line->number-list line)
  (map string->number (string-split line #\,)))

(define (print-array array)
  (for-each (lambda (row)
	      (display row) (newline))
	    (array->list array)))

 
  
(print-array (dijkstras default-array '(0 0) '(#t #t #t #t)))
(newline)

(display
 (let ((min-path-matrix (dijkstras default-array ;(make-matrix-from-file "../input/p083.txt")
				   '(0 0)
				   '(#t #t #t #t))))
   (array-ref min-path-matrix
	      (1- (array-length min-path-matrix))
	      (1- (array-length min-path-matrix)))))
(newline)

;; Note this is for problem 083, but still needs work
(display
 (let ((matrix default-array))
   (reduce (lambda (val acc)
	 (if (< val acc) val acc))
	 0
	 (append-map
	  (lambda (start)
	    (let ((min-path-matrix
		   (dijkstras matrix
			      start
			      '(#f #t #t #t))))
	      (print-array min-path-matrix)
	      (newline)
	      (map (lambda (i)
		     (array-ref min-path-matrix (1- (array-length min-path-matrix)) i))
		   (iota (array-length min-path-matrix)))))
	  (map (lambda (i) (list i 0))
	       (iota (array-length matrix)))))))
 (newline)
