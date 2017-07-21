(define (tail-rec-test n)
 (define (loop c n)
  (if (> c n)
   n
   (loop (+ c 1) n)))
 (loop 1 n))
(tail-rec-test 100000000)
