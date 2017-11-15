(ns (euler solved p031))
(use (argyle))

(def coins '(200 100 50 20 10 5 2 1))

;;; This is how we want to write it
(def coin-sums1
  (fn-case
   ((amount (coin)) 1)
   ((amount :o ((coin . rest) :or coins))
    (loop rec ((amount amount))
          (+ (rec (- amount coin))
             (coin-sums amount rest))))))

(def coin-sums2 (amount :o ((coin . rest) :or coins))
  (if (nil? rest) 1
      (loop rec ((amount amount))
        (if (neg? amount) 0
            (+ (rec (- amount coin))
               (coin-sums2 amount rest))))))
