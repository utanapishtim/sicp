(define (make-serializer)
    ;; create mutex
	(let ((mutex (make-mutex)))
      ;; fn -> fn -> fn
	  (lambda (p)
        ;; a fn that takes some args
	    (define (serialized-p . args)
          ;; aquires the mutex
	      (mutex 'acquire)
          ;; applys the fn to those args
	      (let ((val (apply p args)))
            ;; releases the mutex
	        (mutex 'release)
            ;; returns the result of the above fn application
	        val))
	    serialized-p)))

(define (make-mutex)
	(let ((cell (list false)))
	  (define (the-mutex m)
	    (cond ((eq? m 'acquire)
	           (if (test-and-set! cell)
	               (the-mutex 'acquire))) ; retry
              ((eq? m 'release) (clear! cell))))
	  the-mutex))

(define (clear! cell)
	(set-car! cell false))

(define (test-and-set! cell)
	(if (car cell)
	    true
	    (begin (set-car! cell true)
	           false)))
