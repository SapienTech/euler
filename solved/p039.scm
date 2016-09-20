;; Integer right triangles
(define-module (euler solved p039))

(use-modules (srfi srfi-43)
             (euler utils)
             (wak foof-loop))

;;; Returns (number-of-triangles @perimeter) pair
(define* (solve #:optional (max-perimeter 1001))
  "For which perimeter ≤ 1000, is the number of solutions maximised?"
  (let* ((right-triangles (right-triangles-under max-perimeter))
         (grouped-triangles (group-triangles-by-perimeter right-triangles
                                                          max-perimeter)))
    (find-largest-triangle-group grouped-triangles)))

(define (right-triangles-under perimeter)
  (loop continue ((a 1) (b 1) (triangles '()))
    (let ((c (sqrt (+ (* a a) (* b b)))))
      (cond
       ((> b (ceiling (/ perimeter 2)))
        triangles)
       ((> (+ a b c) perimeter)
        (continue (=> a 1)
                  (=> b (1+ b))))
       ((> a b)
        (continue (=> a 1)
                  (=> b (1+ b))))
       ((not (= c (floor c)))
        (continue (=> a (1+ a))))
       (else (continue (=> a (1+ a))
                       (=> triangles (cons (list a b c)
                                           triangles))))))))

;;; returns vector where index is perimeter, and value is list
;;; of triangles with that perimeter.
(define (group-triangles-by-perimeter right-triangles max-perimeter)
  (categorize right-triangles triangle-perimeter max-perimeter))

(define (triangle-perimeter triangle)
  (apply + triangle))

(define (find-largest-triangle-group triangle-groups)
  (loop ((for group perimeter
              (in-vector triangle-groups))
         (with curr-group (cons 0 0)
               (cons (group-size group) perimeter))
         (with largest-group (cons 0 0)
               (take-larger-group largest-group
                                  curr-group)))
        => largest-group))

(define (group-size group)
  (length group))

(define (take-larger-group g1 g2)
  (if (>= (car g1) (car g2))
      g1 g2))
