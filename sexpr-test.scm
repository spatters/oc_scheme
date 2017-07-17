1
2
(+ 1 2)
(+ (+ 3 4) (+ 3 5))
(+ 2 4 5)
(- 1 2 3)
#t
#f
(and #t #f)
(and #f #t)
(and #t #t)
(and #f #f)
(or #t #f)
(or #f #t)
(or #t #t)
(or #f #f)
(equal? 1 1)
(equal? 1 2)
(if #t 1 0)
(if #f 1 0)
(> 1 2)
(if (> 1 2) (+ 3 4) (- 0 1))
(define x 4)
(+ x 5) 
(if x 0 1)
(define x #f)
(define (sq i) (* i i))
sq
(sq 3)
(lambda (x y) (+ x y))
((lambda (x y) (+ x y)) 4 5) 
(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))

