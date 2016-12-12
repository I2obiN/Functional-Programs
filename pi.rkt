;;; R5RS
;;; Iterative-recursive pi procedure, probably needs a lot of work
(define (pi x y count)
  (if (= (modulo count 2) 0)
  (- y (/ 1 x)
  (+ y 1)
  (+ x 2)
  (+ count 1))
      ((+ y (/ 1 x))
      (+ y 1)
      (+ x 2)
      (+ count 1)))
  (pi x y count))
