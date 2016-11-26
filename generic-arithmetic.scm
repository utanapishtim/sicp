
;;; generic arithmetic operators
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;; scheme arithmetic
(define (install-scheme-number-package)
	(define package-key '(scheme-number scheme-number))

	(define (tag x) (attach-tag 'scheme-number x))

	(put 'add package-key (lambda (x y) (tag (+ x y))))

	(put 'sub package-key (lambda (x y) (tag (- x y))))

	(put 'mul package-key (lambda (x y) (tag (* x y))))

	(put 'div package-key (lambda (x y) (tag (/ x y))))

	(put 'make 'scheme-number (lambda (x) (tag x)))
	
	'done)

(define (make-scheme-number n)
	((get 'make 'scheme-number) n))


;;; rational arithmetic
(define (intall-rational-package)
	;;; internal procedures
	(define (numer x) (car x))
	
	(define (denom x) (cdr x))
	
	(define (make-rat n d)
	  (let ((g (gcd n d)))
	    (cons (/ n g) (/ d g))))

	(define (add-rat x y)
	  (make-rat (+ (* (numer x) (denom y))
	               (* (numer y) (denom x)))
	            (* (denom x) (denom y))))

	(define (sub-rat x y)
	  (make-rat (- (* (numer x) (denom y))
	               (* (numer y) (denom x)))
	            (* (denom x) (denom y))))

	(define (mul-rat x y)
	  (make-rat (* (numer x) (numer y))
	            (* (denom x) (denom y))))

	(define (div-rat x y)
	  (make-rat (* (numer x) (denom y))
	            (* (denom x) (numer y))))

	;;; external interface
	(define package-key '(rational rational));

	(define (tag x) (attach-tag 'rational x))

	(put 'add package-key (lambda (x y) (tag (add-rat x y))))

	(put 'sub package-key (lambda (x y) (tag (sub-rat x y))))

	(put 'mul package-key (lambda (x y) (tag (mul-rat x y))))

	(put 'div package-key (lambda (x y) (tag (div-rat x y))))

	(put 'make 'rational (lambda (n d)  (tag (make-rat n d))))
	
	'done)

	(define (make-rational n d) ((get 'make 'rational) n d))

	
;;; complex arithmetic
(define (intall-complex-package)
	;;; imported procedures from rectangular and polar packages
	(define (make-from-real-imag x y)
	  ((get 'make-from-real-imag 'rectangular) x y))

	(define (make-from-mag-ang r a)
	  ((get 'make-from-mag-ang 'polar) r a))

	;;; internal procedures
	(define (add-complex z1 z2)
	  (make-from-real-imag (+ (real-part z1) (real-part z2))
	                       (+ (imag-part z1) (imag-part z2))))

	(define (sub-complex z1 z2)
	  (make-from-real-imag (- (real-part z1) (real-part z2))
	                       (+ (imag-part z1) (imag-part z2))))

	(define (mul-complex z1 z2)
	  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
	                     (+ (angle z1) (angle z2))))

	(define (div-complex z1 z2)
	  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
	                     (- (angle z1) (angle z2))))

	;;; interface to system
	(define package-key '(complex complex))

	(define (tag z) (attach-tag 'complex z))

	(put 'real-part '(complex) real-part)

	(put 'imag-part '(complex) imag-part)

	(put 'magnitude '(complex) magnitude)

	(put 'angle '(complex) angle)

	(put 'add package-key (lambda (z1 z2) (tag (add-complex z1 z2))))

	(put 'sub package-key (lambda (z1 z2) (tag (sub-complex z1 z2))))

	(put 'mul package-key (lambda (z1 z2) (tag (mul-complex z1 z2))))

	(put 'div package-key (lambda (z1 z2) (tag (div-complex z1 z2))))

	(put 'make-from-real-imag 'complex 
	  (lambda (x y) (tag (make-from-real-imag x y))))

	(put 'make-from-mag-ang 'complex
	  (lambda (r a) (tag (make-from-mag-ang r a))))
	
	'done)

	(define (make-complex-from-real-imag x y)
	  ((get 'make-from-real-imag 'complex) x y))

	(define (make-complex-from-mag-ang r a)
	  ((get 'make-from-mag-ang 'complex) r a))


;;; coercion
(define (scheme-number->complex n)
	(make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number-> complex)


;;; genric operations + coercion
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
	  (let ((proc (get op type-tags)))
	    (if proc
	      (apply proc (map contents args))
	      (if (= (length args) 2)
	        (let ((type1 (car type-tags))
	              (type2 (cadr type-tags))
	              (a1 (car args))
	              (a2 (cadr args)))
	          (let ((t1->t2 (get-coercion type1 type2))
	                (t2->t1 (get-coercion type2 type1)))
	            (cond (t1->t2
	                   (apply-generic op (t1->t2 a1) a2))
	                  (t2->t1
	                   (apply-generic op a1 (t2->t1 a2)))
	                  (else
	                    (error "No method for these types"
	                           (list op type-tags))))))
	      (error "No method for these types"
	             (list op type-tags)))))))
