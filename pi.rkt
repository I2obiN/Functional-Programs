;;; R5RS
;;; Iterative-recursive pi procedure, probably needs a lot of work
(define (pi-get n)
  (pi 0 1 n 0))

(define (pi sum a n count)
  ;;; if n == 0, 0
  (if (= n 0) 0)
  ;;; if count % 2 == 1, + ... else -, if count == n, sum
  (cond ((< count n)
  (cond ((= (modulo count 2) 1)
  (pi(+ sum (pi-calc (+ 2 a))) (+ a 2) n (+ count 1)))
  (pi(- sum (pi-calc (+ 2 a))) (+ a 2) n (+ count 1))))))

(define (pi-calc a)
  (/ 1.0 a))
  
  ;;; linear-recursive
  (define (pi-sum a b)
    (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))
      
      ;;; taken from SICP
