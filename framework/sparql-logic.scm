;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solving and Simplifying
(define (replace-variables exp substitutions)
  (let rec ((exp exp))
    (if (null? exp) '()
	(let ((e (car exp))
	      (rest (rec (cdr exp))))
	  (if (pair? e)
	      (cons (rec e) rest)
	      (cons (or (alist-ref e substitutions) e) rest))))))
                             
(define (simplify-values vars vals bindings)
  (let ((vars (if (pair? vars) vars (list vars)))
	(vals (if (pair? (car vals)) vals (map list vals))))
    (call/cc
     (lambda (out)
       (let loop ((vars vars) (new-vars '()) 
		  (vals vals) (new-vals (make-list (length vals) '())))
	 (cond ((null? vars)
		(if (null? new-vars) '()
		    `(VALUES ,(reverse new-vars) ,@(map reverse new-vals))))
	       ((pair? vars)
		(cond ((sparql-variable? (car vars))
		       (loop (cdr vars) 
			     (cons (car vars) new-vars)
			     (map cdr vals) 
			     (map cons (map car vals) new-vals)))
		      (else 
		       (let ((matches (map (cut rdf-equal? <> (car vars)) (map car vals))))
			 (if (null? (filter values matches))
			     (out #f)
			     (loop (cdr vars) new-vars
				   (filter values (map (lambda (match? val) (and match? val)) 
						       matches vals))
				   (filter values (map (lambda (match? val) (and match? val)) 
						       matches new-vals))))))))
	       (else (cond ((sparql-variable? vars) `(VALUES ,vars ,@vals))
			   (else
			    (if (null? (filter not (map (cut rdf-equal? <> vars) vals)))
				'()
				(out #f)))))))))))

(define (validate-filter statement bindings)
  (let ((check (validate-constraint (second statement) bindings)))
    (case check
      ((#t) (values '() bindings))
      ((?) (values (list statement) bindings))
      ((#f)
       (values (list #f) bindings))
      (else (abort (format "Validate filter error: ~A" statement))))))

(define (validate-constraint constraint bindings)
  (match constraint
    ((`= a b) (unify a b bindings))
    ((`!= a b) (disunify a b bindings))
    ((`IN a bs) (constraint-member a bs bindings))
    ((`|NOT IN| a b)  (constraint-not-member a b bindings))
    (else '?)))

(define (unify a b bindings)
  (if (or (sparql-variable? a) (sparql-variable? b))
      '?
      (rdf-equal? a b)))

(define (disunify a b bindings)
  (if (or (sparql-variable? a) (sparql-variable? b))
      '?
      (not (rdf-equal? a b))))

(define (constraint-member a bs bindings)
  (let ((possible? (lambda (x) (or (sparql-variable? x) (rdf-equal? a x)))))
    (cond ((sparql-variable? a) '?)
          ((not (null? (filter possible? bs))) #t)
	  ((null? (filter sparql-variable? bs)) #f)
	  (else '?))))

(define (constraint-not-member a b bindings)
  (cond ((sparql-variable? a) '?)
	((member a b) #f)
	((null? (filter sparql-variable? b)) #t)
	(else '?)))
