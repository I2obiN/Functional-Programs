;;; R5RS
;;; Iterative-recursive pi procedure, probably needs a lot of work

(define (pi-get n)
  (pi 1 1 n 0))

(define (pi sum a n count)
  (if (= n 0) 0)
  (cond ((< count n)
         (cond ((= (modulo count 2) 1)
                (pi (+ sum (pi-calc (+ 2 a))) (+ a 2) n (+ count 1)))
               ((= (modulo count 2) 0)
                (pi (- sum (pi-calc (+ 2 a))) (+ a 2) n (+ count 1))))
  (display (* 4 sum)) (newline))))

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
