;; Triangle containment

(use-modules (srfi srfi-1))
(use-modules (rnrs io ports))
(use-modules (ice-9 receive))

;; Question: should I show the outer or inner function below?

(define (triangle-containment file)
  (length (filter contains-origin? (make-triangles file))))

(define (containment-test triangle)
  (contains-origin? triangle))

(define (contains-origin? triangle)
  (define
    (outer-loop sides axis-list)
    (newline)
    (display "(outer) axis-list: ")
    (display axis-list)
    (newline)
     (if (null? sides)
	 axis-list
	 (outer-loop (cdr sides)
		     (inner-loop (car sides) axis-list))))

  (define
    (inner-loop side axis-list)
    (newline)
    (display "(inner) axis-list: ")
    (display axis-list)
    (newline)
     (cond
      ((null? axis-list) '())
      ((and (list? (car axis-list))
       (intersects? (car axis-list) side))
       (cons #t (inner-loop side (cdr axis-list))))
      (else (cons (car axis-list) (inner-loop side (cdr axis-list))))))

  (define (solution-interpreter solns)
    (fold (lambda (soln acc)
	    (if (list? soln)
		(and acc #f)
		(and acc #t)))
	  #t
	  solns))

  (solution-interpreter (outer-loop (get-sides triangle) axis-list)))

(define origin '(0 0))

(define (intersects? axis line-seg)
  (let ((intersection-point (get-intersection-point axis line-seg)))
    (if (null? intersection-point)
	(and (coincident? axis line-seg)
	     (within-bounds? origin axis line-seg))
	(within-bounds? intersection-point axis line-seg))))

(define (line-line-intersection l1 l2)
  (letrec ((x1 (car (car l1))) ; TODO: use get-points function here
           (y1 (cadr (car l1)))
           (x2 (car (cadr l1)))
           (y2 (cadr (cadr l1)))

           (x3 (car (car l2)))
           (y3 (cadr (car l2)))
           (x4 (car (cadr l2)))
           (y4 (cadr (cadr l2)))
           (denom (- (* (- x1 x2) (- y3 y4))
                     (* (- y1 y2) (- x3 x4)))))
    (newline)
    (display "l1, l2: ")
    (display (list l1 l2))
    (newline)
    (display "x1->y4: ")
    (display (list x1 x2 x3 x4 y1 y2 y3 y4))
    (newline)
    (display "denom: ")
    (display denom)
    (newline)
    (if (= 0 denom)
      '() ; Note we will have more logic to handle this case
      ;; TODO: clean this up...
      (list (exact->inexact (/ (- (* (- (* x1 y2)
					(* y1 x2))
				     (- x3 x4))
				  (* (- (* x3 y4)
					(* y3 x4))
				     (- x1 x2)))
			       denom))
            (exact->inexact (/ (- (* (- (* x1 y2)
					(* y1 x2))
				     (- y3 y4))
				  (* (- (* x3 y4)
					(* y3 x4))
				     (- y1 y2)))
			       denom))))))

(define get-intersection-point line-line-intersection)

;; Intersection point tests...
(display (get-intersection-point
	  (list '(0 1) '(0 2))
	  (list '(-340 495) '(-153 910))))

;; This should be 3
(display (get-intersection-point
	  (list '(0 0) '(0 1))
	  (list '(-2 1) '(-1 2))))

;; This should be 3
(display (get-intersection-point
	  (list '(0 1) '(0 2))
	  (list '(2 1) '(1 2))))

(display (get-intersection-point
	  (list '(1 1) '(3 3))
	  (list '(1 3) '(3 1))))

(display (get-intersection-point
	  (list '(1 1) '(2 2))
	  (list '(1 2) '(2 1))))

(display (get-intersection-point
	  (list '(0 1) '(0 2))
	  (list '(2 2) '(1 2))))

(define (coincident? axis line)
  (let loop ((axis-p (car axis))
	     (line-p (car line)))
    (if (= 0 (car axis-p))
	(= 0 (car line-p))
	(loop (cdr axis-p) (cdr line-p)))))

;; TODO: consider renaming
;; Issues remain, although I think the issue is do to the intersection point logic...
(define (within-bounds? p axis edge)
  (and (within-axis? p axis)
       (within-edge? p edge)))

(define (within-axis? p axis)
  ;; NOTE: we are assuming a lot here...
  (let loop ((p p) (axis-p (car axis)))
    (if (= 0 (car axis-p))
	(loop (cdr p) (cdr axis-p))
	(or
	 (and (>= (car axis-p) 0)
	      (>= (car p) 0))
	 (and (<= (car axis-p) 0)
	      (<= (car p) 0))))))

(define (within-edge? p line-seg)
  (display "intersection point: ")
  (display p)
  (newline)
  (display "line")
  (display line-seg)
  (newline)
  (let ((this
  (receive (x1 x2 y1 y2)
      (get-coordinates line-seg)
    (and
     (if (> x1 x2)
	 (and
	  (<= (car p) x1)
	  (>= (car p) x2))
	 (and
	  (>= (car p) x1)
	  (<= (car p) x2)))
     (if (> y1 y2)
	 (and
	  (<= (cadr p) y1)
	  (>= (cadr p) y2))
	 (and
	  (>= (cadr p) y1)
	  (<= (cadr p) y2)))))))
    (display this)
    this))

(define (get-coordinates line-seg)
  (values (car (car line-seg))
	  (car (cadr line-seg))
	  (cadr (car line-seg))
	  (cadr (cadr line-seg))))

(define (get-sides triangle)
  (map (lambda (point)
         (delv point triangle))
       triangle))

(define +x-axis (list '(1 0) '(2 0)))
(define -x-axis (list '(-1 0) '(-2 0)))
(define +y-axis (list '(0 1) '(0 2)))
(define -y-axis (list '(0 -1) '(0 -2)))

(define axis-list
  (list +x-axis
	-x-axis
	+y-axis
	-y-axis))

(define (make-triangles file)
  (let ((port (open-input-file file)))
    (let loop ((line (get-line port)) (triangles '()))
      (if (eof-object? line)
        triangles
        (loop (get-line port)
              (cons (make-triangle line) triangles))))))

;; TODO: figure out what is preferred
(define (make-triangle line)
  (let loop ((points (map string->number
                          ((lambda (s) (string-split s #\,))
                           line)))
             (vertices '()))
    (if (null? points) vertices
      (loop (drop points 2)
            (cons (take points 2) vertices)))))

  ;; (letrec (
  ;;          (loop (lambda (points vertices)
  ;;                  (if (null? points) vertices
  ;;                    (loop (drop points 2)
  ;;                          (cons (take points 2) vertices))))))
  ;;   (loop (map string->number (points line)) '())))

;; (define (contains-origin1? triangle)
;;   (fold and #t 
;;         (fold (lambda (intersections acc)
;;                 (map or intersections acc))
;;               '(#f #f #f #f)
;;               (get-sides triangle))))
;; 

;; 
;; 
;; (define (intersects c1 c2)
;;     (or (and (<= 0 c1) (>= 0 c2))
;;         (and (>= 0 c2) (<= 0 c2))))
;; 
;; 
;; 
;; 
;; (display (triangle-containment "input/p102.txt"))

(define triangle1
  (list
   (list 1 2)
   (list 1 -2)
   (list -2 -2)))

(define triangle2
  (list
   (list 2 2)
   (list 2 1)
   (list 1 1)))

(define triangle3
  (list
   '(-340 495)
   '(-153 -910)
   '(835 -947)))

(define triangle4
  (list
   '(-175 41)
   '(-421 -714)
   '(574 -645)))

(define triangle5
  (list
   '(1 0)
   '(-1 0)
   '(2 0)))

(define triangle6
  (list
   '(0 0)
   '(200 200 )
   '(410 200)))

(define triangle7
  (list
   '(-1 -1)
   '(1 1 )
   '(3 4)))

(display (containment-test triangle3))

(display (containment-test triangle6))

;; (display (containment-test triangle4))
;; Hmmm, well i guess we can do a check to see if any of the points have the origin in them...

(display (triangle-containment "input/p102.txt"))
