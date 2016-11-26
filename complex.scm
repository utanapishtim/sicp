(define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
	                     (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
	                     (- (imag-part z1) (imag-part z2))))


(define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
	                   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
	                   (- (angle z1) (angle z2))))

;;; Ben Bitdiddle's Representation
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z) (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
	(cons (* r (cos a)) (* r (sin a))))


;;; Alyssa P. Hacker's Representation
(define (real-part z)
	(* (magnitude z) (cos (angle z))))

(define (imag-part z)
	(* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
	(cons (sqrt (+ (square x) (square y)))
	      (atan x y)))

(define (make-from-mag-ang r a) (cons r a))


;;; type tags
(define (attach-tag type-tag contents)
	(cons type-tag contents))

(define (type-tag datum)
	(if (pair? datum)
	    (car datum)
	    (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
	(if (pair? datum)
	    (cdr datum)
	    (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
	(eq? (type-tag z) 'rectangular))

(define (polar? z)
	(eq? (type-tag z) 'polar))


;;; Ben Revised
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
	(sqrt (+ (square (real-part-rectangular z))
	         (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
	(atan (imag-part-rectangular z)
	      (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
	(attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
	(attach-tag 'rectangular
	            (cons (* r (cos a)) (* r (sin a)))))


;;; Alyssa Revised
(define (real-part-polar z)
	(* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
	(* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
	(attach-tag 'polar
	            (cons (sqrt (+ (square x) (square y)))
	                  (atan y x))))

(define (make-from-mag-ang-polar r a)
	(attach-tag 'polar (cons r a)))


;;; generic ops
(define (real-part z)
	(cond ((rectangular? z)
	       (real-part-rectangular (contents z)))
	      ((polar? z)
	       (real-part-polar (contents z)))
	      (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
	(cond ((rectangular? z)
	       (imag-part-rectangular (contents z)))
	      ((polar? z)
	       (imag-part-polar (contents z)))
	      (else (error "Unkown type -- IMAG-PART" z))))

(define (magnitude z)
	(cond ((rectangular? z)
	       (magnitude-rectangular (contents z)))
	      ((polar? z)
	       (magnitude-polar (contents z)))
	      (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
	(cond ((rectangular? z)
	       (angle-rectangular (contents z)))
	      ((polar? z)
	       (angle-polar (contents z)))
	      (else (error "Unkown type -- ANGLE" z))))

(define (make-from-real-imag x y)
	(make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
	(make-from-mag-ang-polar r a))


;;; data-directed programming
;;; (put <op> <type> <item>)
;;; (get <op> <type>)

;;; Ben
(define (install-rectangular-package)
	;; internal procedures
	(define (real-part z) (car z))
	
	(define (imag-part z) (cdr z))

	(define (make-from-real-imag x y) (cons xy))
	
	(define (magnitude z)
	  (sqrt (+ (square (real-part z))
	           (square (imag-part z)))))

	(define (angle z)
	  (atan (imag-part z) (real-part z)))

	(define (make-from-mag-ang r a)
	  (cons (* r (cos a)) (* r (sin a))))

	;; external interface
	(define (tag x) (attach-tag 'rectangular x))

	(put 'real-part '(rectangular) real-part)
	
	(put 'imag-part '(rectangular) imag-part)

	(put 'magnitude '(rectangular) magnitude)

	(put 'angle '(rectangular) angle)

	(put 'make-from-real-imag 'rectangular
	  (lambda (x y) (tag (make-from-real-imag x y))))

	(put 'make-from-mag-ang 'rectangular
	  (lambda (r a) (tag (make-from-mag-ang r a))))

	'done)

;;; Alyssa
(define (install-polar-package)
	;; internal procedures
	(define (magnitude z) (car z))

	(define (angle z) (cdr z))

	(define (make-from-mag-ang r a) (cons r a))

	(define (real-part z)
	  (* (magnitude z) (cos (angle z))))

	(define (imag-part z)
	  (* (magnitude z) (sin (angle z))))

	(define (make-from-real-imag x y)
	  (cons (sqrt (+ (square x) (square y)))
	        (atan y x)))

	;;; external interface
	(define (tag x) (attach-tag 'polar x))
	
	(put 'real-part '(polar) real-part)

	(put 'imag-part '(polar) imag-part)

	(put 'magnitude '(polar) magnitude)

	(put 'angle '(polar) angle)

	(put 'make-from-real-imag 'polar
	  (lambda (x y) (tag (make-from-real-imag x y))))

	(put 'make-from-mag-ang 'polar
	  (lambda (r a) (tag (make-from-mag-ang r a))))

	'done)


;;; using packages
(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
	  (let ((proc (get op type-tags)))
	    (if proc
	      (apply proc (map contents args))
	      (error
	        "No method for these types -- APPLY-GENERIC"
	        (list op type-tags))))))

;;; generic selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;; generic constructors
(define (make-from-real-imag x y)
	((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
	((get 'make-from-mag-ang 'polar) r a))


;;; message passing
(define (mp-make-from-real-imag x y)
	(define (dispatch op)
	  (cond ((eq? op 'real-part) x)
	        ((eq? op 'imag-part) y)
	        ((eq? op 'magnitude)
	         (sqrt (+ (square x) (square y))))
	        ((eq? op 'angle) (atan y x))
	        (else
	          (error "Unkown op -- MAKE-FROM-REAL-IMAG" op))))
	dispatch)

(define (mp-apply-generic op arg) (arg op))


