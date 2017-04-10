(module (euler solved p031))
(use (argyle))

(def coins '(200 100 50 20 10 5 2 1))

(def coin-sums (amount :o ((coin . stack) :or coins))
  (if (nil? stack) 1
      (loop rec ((amount amount))
        (if (< amount 0) 0
            (+ (rec (- amount coin))
               (coin-sums amount stack))))))
;;; TODO: add memoization alg
