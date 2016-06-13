;; Cubic permutations
(use-modules (euler utils))

(define (find-n-cubic-permutation n)
  (define cubic-perms '())
  (define (search-loop i)
    (let* ((cube (expt i 3))
	   (key (number->key cube)))
      (add-perm key cube)
      (if (>= (get-perm-count key) n)
	  (get-min-perm-val key)
	  (search-loop (1+ i)))))
  (define (add-perm key val)
    (set! cubic-perms
      (assoc-set! cubic-perms
		  key
		  (list (set-min-perm-val key val) (1+ (get-perm-count key))))))
  (define (get-perm-count key)
    (let ((entry? (assoc-ref cubic-perms key)))
      (if entry? (cadr entry?) 0)))
  (define (get-min-perm-val key)
    (car (assoc-ref cubic-perms key)))
  (define (set-min-perm-val key val)
    (let ((entry? (assoc-ref cubic-perms key)))
      (if entry? (car entry?) val)))
  (search-loop 1))

(define (number->key n)
  (string-concatenate (map number->string (sort (number->digits n) <))))
