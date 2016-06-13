;; Sudoku

(use-modules (sudoku solver)
	     (sudoku init)
	     (sudoku square)
	     (rnrs io ports)
	     (euler utils))


(define (sudoku-sum)
  (define board-generator (make-board-generator))
  (let loop ((acc 0))
    (let ((board (board-generator)))
      (if (not board) acc
	  (loop (+ acc
		   (get-top-left-number
		    (solver board))))))))

 ;; Not sure if the unless thing is a good idea...
(define (make-board-generator)
  (let ((file (open-input-file "../input/p096.txt")))
    (lambda ()
      (cond
       [(port-closed? file) #f]
       [(eof-object? (get-line file)) (close-input-port file) #f]
       [else 
	(let ((board-string ""))
	  (do ((i 0 (1+ i)))
	      ((>= i 9))
	    (set! board-string (string-append board-string
					      (get-line file))))
	  (init-board board-string))]))))

(define (get-top-left-number solved-board)
  (digits->number
   (map get-value
	(map (lambda (col)
	       (array-ref solved-board 0 col))
	     (iota 3)))))
