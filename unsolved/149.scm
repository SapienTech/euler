;; Searching for maximum-subsequence
;; Dijkstras: find maximum path

(define-module (euler p149))

(use-modules (search dijkstras)
	     (utils fold))

(define (max-path-in-network network)
  (let [(network-copy network)]
    (array-fold
     (lambda (path max-path)
       (if (> path max-path) path max-path))
     (array-index-map! network
		       (lambda (i j)
			 (max-path-from network-copy i j))))))

(define (min-path-from network i j)
  (let ([min-path-network (make-min-network network i j)])
    (get-max-path min-path-network)))

(define (make-min-path-network-from network i j)
  0)

(define (get-min-path max-path-network)
  (array-fold
   (lambda (point acc)
     (if (> (vertex-dist point) (vertex-dist acc)) point acc))
   (array-ref max-path-network 0 0)
   max-path-network))

(define default-array
  (list->array 2
	       (list
		(list -2 5 3 2)
		(list 9 -6 5 1)
		(list 3 2 7 3)
		(list -1 8 -4 8))))


