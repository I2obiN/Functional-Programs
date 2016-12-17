;;; R5RS
;;; Iterative-recursive pi procedure, probably needs a lot of work
;;; (pi-get 1000)

(define (pi-get n)
  (pi-iter 0 1 n 0))

(define (pi-iter sum a n count)
  (if (< count n)
  (if (= (modulo count 2) 0) (pi-iter (+ sum (pi-calc a)) (+ a 2) n (+ count 1))
    (pi-iter (- sum (pi-calc a)) (+ a 2) n (+ count 1)))) 
  (display (* 4 sum)) (newline))

(define (pi-calc a)
  (/ 1.0 a))

;;; Bench
(define start 0)
(define end 0)
(set! start (current-milliseconds))
(pi-get 400000)
(set! end (current-milliseconds))
(display (/ (- end start) 1000.00))
(write "seconds")
