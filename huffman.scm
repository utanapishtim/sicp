(define (make-leaf symbol weight)
	(list 'leaf symbol weight))

(define (leaf? object)
	(eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list left
          right
          (append (symbols left) (symbols right))
	      (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
	    (list (symbol-leaf tree))
	    (caddr tree)))

(define (weight tree)
	(if (leaf? tree)
	    (weight-leaf tree)
	    (cadddr tree)))

(define (decode bits tree)
	(define (decode-iter bits current-branch)
	  (if (null? bits)
	    '()
        (let ((next-branch
	          (choose-branch (car bits) current-branch)))
		  (if (leaf? next-branch)
	          (cons (symbol-leaf next-branch)
	                (decode-iter (cdr bits) tree))
	          (decode-iter (cdr bits) next-branch)))))
	(decode-iter bits tree))

(define (choose-branch bit tree)
	(cond ((= bit 0) (left-branch tree))
	      ((= bit 1) (right-branch tree))
	      (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
	      ((< (weight x) (weight (car set))) (cons x set))
	      (else (cons (car set)
	                  (adjoin-set x (cdr set))))))


(define (make-leaf-set pairs)
	(if (null? pairs)
	  '()
	  (let ((pair (car pairs)))
	    (adjoin-set (make-leaf (car pair)   ; symbol
	                           (cadr pair)) ; frequency
	                (make-leaf-set (cdr pairs))))))

(define sample-tree
	(make-code-tree (make-leaf 'A 4)
	                (make-code-tree (make-leaf 'B 2)
	                                (make-code-tree (make-leaf 'D 1)
	                                                (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
	(if (null? message)
	  '()
	  (append (encode-symbol (car message) tree)
	          (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
	(define (encoder symbol tree history)
	  (cond ((null? tree) '())
	        ((leaf? tree) 
	          (if (eq? symbol (symbol-leaf tree))
                history
                '()))
            (else (append (encoder symbol (left-branch tree) (append history (list 0)))
	                      (encoder symbol (right-branch tree) (append history (list 1)))))))
	(define encoding (encoder symbol tree '()))
	(if (null? encoding)
	  (error "no such symbol in huffman tree -- NO SUCH SYMBOL" symbol)
	  encoding))

(define (generate-huffman-tree pairs)
	(car (successive-merge (make-leaf-set pairs))))

(define (successive-merge pairs)
	(cond ((null? pairs) '())
	      ((= (length pairs) 1) pairs)
	      (else
	        (let ((p1 (car pairs)) (p2 (cadr pairs)))
	          (if (> (weight p1) (weight p2))
	            (successive-merge (cons p1 (successive-merge (cdr pairs))))
	            (successive-merge (cons (make-code-tree p1 p2) (cddr pairs))))))))

(define test-pairs (list (list 'A 2) (list 'NA 16) (list 'BOOM 1) (list 'GET 2) (list 'YIP 9) (list 'SHA 3) (list 'JOB 2) (list 'WAH 1)))
