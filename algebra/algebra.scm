(define (does-commute op)
	(lambda (x y) (= (op x y) (op y x))))

(define (does-associate op)
	(lambda (x y z) (= (op (op a b) c) (op a (op b c)))))

(define (do-distribute op1 op2)
	(lambda (x y z) (= (op1 z (op2 x y)) (op2 (op1 z x) (op1 z y)))))

(define (get-two num)
	(- (* 2 (+ 3 num)) num 4 num))

(define (are-neighbors r1 r2)
	(= 1 (abs (- (* (numerator r1) (denominator r2)) 
                 (* (numerator r2) (denominator r1))))))
