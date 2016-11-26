(define (make-table)
	(list '*table*))

(define (insert! key value table)
	(let ((record (assoc key (cdr table))))
	  (if record
	    (set-cdr! record value)
	    (set-cdr! table
	              (cons (cons key value) (cdr table)))))
	'ok)

(define (lookup key table)
	(let ((record (assoc key (cdr table))))
	  (if record
	    (cdr record)
	    false)))

(define (assoc key records)
	(cond ((null? records) false)
	      ((equal? key (caar records)) (car records))
	      (else (assoc key (cdr records)))))


(define (lookup-nested key-1 key-2 table)
	(let ((subtable (assoc key-1 (cdr table))))
	  (if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
	          (cdr record)
	          false))
	    false)))

(define (insert-nested! key-1 key-2 value table)
	(let ((subtable (assoc key-1 (cdr table))))
	  (if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
	        (set-cdr! record value)
	        (set-cdr! subtable
	                  (cons (cons key-2 value)
	                        (cdr subtable)))))
	    (set-cdr! table
	              (cons (list key-1
	                          (cons key-2 value))
	                    (cdr table)))))
	'ok)

(define (lookup-N table . keys)
	(cond ((null? keys) false)
          ((= (length keys) 1)
	       (let ((record (assoc (car keys) (cdr table))))
	         (if record
	           (cdr record)
	           false)))
	      (else (lookup-N (assoc (car keys) (cdr table)) (cdr keys)))))

(define (insert-N! table value . keys)
	(cond ((null? keys)
	       (error "no keys for insert -- INSERT-N" keys))
	      ((= (length keys) 1)
	       (let ((record (assoc (car keys) (cdr table))))
	         (if record
	           (set-cdr! record value)
	           (set-cdr! table
	                     (cons (cons (car keys) value) (cdr table))))))
	      (else 
            (let ((subtable (assoc (car keys) (cdr table))))
	          (if subtable
	            (insert-N! subtable value (cdr keys))
	            (error "no such key in table -- INSERT-N" (list (car keys) subtable)))))))




(define (make-table)
	(let ((local-table (list '*table*)))
	  (define (lookup key-1 key-2)
	    (let ((subtable (assoc key-1 (cdr local-table))))
	      (if subtable
	        (let ((record (assoc key-2 (cdr subtable))))
	          (if record
	            (cdr record)
	            false))
	        false)))
	  (define (insert! key-1 key-2 value)
	    (let ((subtable (assoc key-1 (cdr local-table))))
	      (if subtable
	        (let ((record (assoc key-2 (cdr subtable))))
	          (if record
	            (set-cdr! record value)
	            (set-cdr! subtable
	                      (cons (cons key-2 value)
	                            (cdr subtable)))))
	        (set-cdr! local-table
	                  (cons (list key-1
	                              (cons key-2 value))
	                        (cdr local-table)))))
	  'ok)
	(define (dispatch m)
	  (cond ((eq? m 'lookup-proc) lookup)
	        ((eq? m 'insert-proc!) insert!)
	        (else (error "Unkown operation -- TABLE" m))))
	dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
