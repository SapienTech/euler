;; Integer right triangles
(define-module (euler solved p039))

(use-modules (srfi srfi-43)
             (euler utils)
             (wak foof-loop))

(define* (solve #:optional (max-perimeter 1001))
  (let* ((right-triangles (right-triangles-under max-perimeter))
         (sorted-triangles
          (group-triangles-by-perimeter right-triangles max-perimeter))
         (triangle-count@perimeters (vector-map-no-idx length sorted-triangles)))
    (loop ((for triangle-count perimeter
                (in-vector triangle-count@perimeters))
           (with triangle-count@perimeter (cons 0 0)
                 (cons triangle-count perimeter))
           (with max-count@perimeter (cons 0 0)
                 (take-larger max-count@perimeter
                              triangle-count@perimeter)))
          => max-count@perimeter)))

(define (right-triangles-under max-perimeter)
  (loop continue ((a 1) (b 1) (triangles '()))
    (let ((c (sqrt (+ (* a a) (* b b)))))
      (cond
       ((> b (ceiling (/ max-perimeter 2)))
        triangles)
       ((> (+ a b c) max-perimeter)
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

(define (take-larger pair1 pair2)
  (if (>= (car pair1) (car pair2))
      pair1 pair2))
