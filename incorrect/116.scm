;; Red Green or Blue tiles

;; UNSOLVED: maybe issues with counting blank tiles

;; TODO: probably want to stick to recursion since it is more powerful and expressive...
(define (find-block-combinations row-length min-block-size max-block-size min-gap-size)
  (letrec*
     ((cache (make-list (1+ row-length) 0))
      (find-sub-combinations
        (lambda (sub-row-length)
          (if (< sub-row-length min-block-size)
            1
            (+ (call-with-cache place-blocks sub-row-length cache)
               (find-sub-combinations (1- sub-row-length))))))
      (place-blocks
        (lambda (sub-row-length)
	  (let place-block ((curr-block-size min-block-size))
	    (if (or
		 (>= curr-block-size sub-row-length)
		 (> curr-block-size max-block-size))
		0
		(+ (find-sub-combinations (- (- sub-row-length min-gap-size) curr-block-size))
		    (place-block (1+ curr-block-size))))))))
    (- (find-sub-combinations row-length) (- (1+ max-block-size) min-block-size))))

(define (call-with-cache proc index cache)
  (if (equal? 0 (list-ref cache index))
    (begin
      (list-set! cache index (proc index))
      (list-ref cache index))
    (list-ref cache index)))

(define (fill-count row-length min-block-size)
  (find-block-combinations row-length min-block-size 1))

(define (fill-count>val val min-block-size)
  (let loop ((row-length min-block-size))
    (if (< val (fill-count row-length min-block-size))
	row-length
	(loop (1+ row-length)))))

(display (find-block-combinations 51 2 4 0))
