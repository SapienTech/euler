;; New way of doing 150
;; Helpers functions to get the sides and intersection vectors

(define-module (unsolved p150h))

(use-modules (unsolved p150))

(define-public (make-sides triangle)
  (define row-length (triangle-rows triangle))
  (define (make-left-rows)
    (define vect (make-vector (1+ row-length) 0))
    (do [(j 0 (1+ j))]
	[(>= j row-length) vect]
      (do [(i j (1+ i))]
	  [(>= i row-length)]
	(vector-set!
	 vect (1+ j) (+ (vector-ref vect (1+ j))
			(vector-ref
			 (vector-ref triangle i) j))))
      (vector-set!
       vect (1+ j) (+ (vector-ref vect j)
		      (vector-ref vect (1+ j))))))

  (define (make-right-rows)
    (define vect (make-vector (1+ row-length) 0))
    (do [(i 0 (1+ i))]
	[(>= i row-length) vect]
      (do [(j 0 (1+ j))]
	  [(>= j (- row-length i))]
	(vector-set!
	 vect (1+ i) (+ (vector-ref vect (1+ i))
			(vector-ref
			 (vector-ref triangle (+ i j)) j))))
      (vector-set!
       vect (1+ i) (+ (vector-ref vect i)
		      (vector-ref vect (1+ i))))))

  
  ;; TODO: figure out best way to represent...
  (define (make-bot-rows)
    (define vect (make-vector (1+ row-length) 0))
    (do [(i 0 (1+ i))]
	[(>= i row-length) vect]
      (do [(j 0 (1+ j))]
	  [(>= j (- row-length i))]
	(vector-set!
	 vect (1+ i)
	 (+ (vector-ref vect (1+ i))
	    (vector-ref
	     (vector-ref triangle (- (1- row-length) i)) j))))
      (vector-set!
       vect (1+ i) (+ (vector-ref vect i)
		     (vector-ref vect (1+ i))))))
  (list 
   (make-left-rows) (make-right-rows) (make-bot-rows)))

;; TODO: consider creating setters and getters for sides-vector
;; I imagine that I need to create the intersection arrays on the fly...


;; Def turn this into a macro!
(define-public (make-intersections triangle) 
  (define triangle-length (triangle-rows triangle))

  (define (make-l&r-intersections)
    (define vect (make-vector (1+ triangle-length) 0))
    (vector-set! vect 0 (make-vector (1+ triangle-length) 0))
    (do [(j 0 (1+ j))]
	[(>= j triangle-length) vect]
      (let ([row-vect (make-vector (- (1+ triangle-length) j) 0)])
	(do [(i 0 (1+ i))]
	    [(>= i (- triangle-length j))]
	  (vector-set! row-vect (1+ i)
		       (+ (vector-ref (vector-ref vect j) (1+ i)) 
			  (vector-ref (vector-ref triangle (+ i j)) j))))
	(vector-set! vect (1+ j) row-vect))))

  (define (make-l&b-intersections)
    (define vect (make-vector (1+ triangle-length) 0))
    (vector-set! vect 0 (make-vector (1+ triangle-length) 0))
    (do [(j 0 (1+ j))]
	[(>= j triangle-length) vect]
      (let ([row-vect (make-vector (- (1+ triangle-length) j) 0)])
	(do [(i 0 (1+ i))]
	    [(>= i (- triangle-length j))]
	  (vector-set! row-vect (1+ i)
		       (+ (vector-ref (vector-ref vect j) (1+ i)) 
			  (vector-ref (vector-ref triangle (- triangle-length
							      (1+ i)))
				      j))))
	(vector-set! vect (1+ j) row-vect))))
  (define (make-r&b-intersections)
    (define vect (make-vector (1+ triangle-length) 0))
    (vector-set! vect 0 (make-vector (1+ triangle-length) 0))
    (do [(j 0 (1+ j))]
	[(>= j triangle-length) vect]
      (let ([row-vect (make-vector (- (1+ triangle-length) j) 0)])
	(do [(i 0 (1+ i))]
	    [(>= i (- triangle-length j))]
	  (vector-set! row-vect (1+ i)
		       (+ (vector-ref (vector-ref vect j) (1+ i)) 
			  (vector-ref (vector-ref triangle (- triangle-length
							      (1+ i)))
				      (- triangle-length (+ i (1+ j)))))))
	(vector-set! vect (1+ j) row-vect))))

  (list
   (make-l&r-intersections)
   (make-l&b-intersections)
   (make-r&b-intersections)))

;; Why not just build them first, then I don't need to deal with triangle in computation?
;; Suppose we could make that argument
;; Basically its going to update every time we recur with l-idx and r-idx
;; Need to modularize this alot...
(define (init-l&r-intersections triangle)
  (let ([intersect-vect (make-vector (triangle-rows triangle) 0)])
    (l&r-intersect-lp! triangle intersect-vect 0)
    intersect-vect))

;; TODO: switch i,j
(define (l&r-intersect-lp! triangle vect curr-idx)
  (let ([j curr-idx]
	[triangle-length (triangle-rows triangle)])
    (do [(i 0 (1+ i))]
	[(>= i (- triangle-length j))]
      (vector-set! vect i (+ (vector-ref
			      (vector-ref triangle (+ i j))
			      i)
			     (vector-ref vect i))))))

;; Hmm this seems a little hacky?
;; Yeah I don't think i want to deal with it...
;; If we don't get r-idx, then do l-idx, otherwise r-idx
(define (l/r-intersect-lp! vect triangle curr-idx)
  (let ([j curr-idx]
	[triangle-length (triangle-rows triangle)])
    (do [(i 0 (1+ i))]
	[(>= i (- triangle-length j))]
      (vector-set! vect i (+ (vector-ref
			      (vector-ref triangle (+ i j))
			      i)
			     (vector-ref vect i))))))

(define (update-l&r-intersections triangle intersections-vect curr-idx)
  (l&r-intersect-lp! triangle intersections-vect curr-idx))

(define (update-l-intersections triangle intersections-vect curr-idx)
  (l-intersect-lp! triangle intersections-vect curr-idx))
