;; Double-base palindromes
(module (euler solved p036))

(use (arguile) (euler utils))

(def double-base-palendromes (:o (limit :or (^ 10 6)))
  (loop ((for i (up-from 1 (to limit)))
         (where palendromes '()
                (if (and (palendromic? (str i))
                         (palendromic? (number->binary i)))
                    (cons i palendromes)
                    palendromes)))
        => palendromes))
