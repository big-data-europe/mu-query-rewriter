;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instantiation
;; for INSERT and INSERT DATA
(define (instantiate where-block triples)
  (if (nulll? where-block) where-block
      (let ((where-block (expand-graphs where-block)))
        (rewrite (list where-block) '() (instantiation-rules triples)))))

;; Matche two triples, and if successful return a binding list.
;; ex: (match-triple '(?s a dct:Agent) '(<a> a dct:Agent) => '((?s . <a>))
;;     (match-triple '(?s a dct:Agent) '(<a> <b> <c>)) =>  #f
;;     (match-triple '(?s a dct:Agent) '(?s a dct:Agent)) => '()
(define (match-triple triple1 triple2)
  (let loop ((triple1 triple1) (triple2 triple2) (match-binding '()))
    (cond ((null? triple1) match-binding)
          ((sparql-variable? (car triple1))
           (if (equal? (car triple1) (car triple2)) ; no self-substitutions
               (loop (cdr triple1) (cdr triple2) match-binding)
               (loop (cdr triple1) (cdr triple2) 
                     (cons `(,(car triple1) . ,(car triple2)) match-binding))))
          ((rdf-equal? (car triple1) (car triple2))
           (loop (cdr triple1) (cdr triple2) match-binding))
          (else #f))))

(define (match-triples triple triples #!optional success?)
  (filter values
          (map (lambda (triple2) (match-triple triple triple2))
               triples)))

(define (instantiation-rules triples)
  `(((@SubSelect) . ,rw/subselect)
    (,annotation? . ,rw/copy)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (match block
            ((`GRAPH graph triple)
             (let ((match-bindings (match-triples triple triples)))
               (if (null? match-bindings)
                   (values (list block) bindings)
                   (let ((match-bindings (filter pair? match-bindings)))
                     (if (null? match-bindings) (values '() bindings)
                         (values `((UNION (,block)
                                          ,@(map (lambda (bindings) 
                                                  (let ((vars (map car bindings))
                                                        (vals (map cdr bindings)))
                                                    (if (null? (filter sparql-variable? (flatten vals)))
                                                        `((VALUES ,vars ,vals))
                                                        (map (lambda (var val)
                                                               `(BIND (AS ,var ,val)))
                                                             vars vals))))
                                                match-bindings)))
                                 bindings)))))))))
    ((UNION)
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
            (let ((new-blocks (filter values rw)))
              (case (length new-blocks)
                ((0)  (values (list new-blocks) new-bindings))
                ((1)  (values `((OPTIONAL ,@(car new-blocks))) new-bindings))
                (else (values `((UNION ,@new-blocks)) new-bindings)))))))
    (,quads-block? 
     . ,(lambda (block bindings)
	  (let-values (((rw new-bindings) (rewrite (list (cdr block)) bindings)))
            (fail-or-null rw new-bindings
              `((,(car block) ,@(join rw)))))))
    ((FILTER) ; **
     . ,(lambda (block bindings)
    	  (match block
    	    ((`FILTER (`|NOT EXISTS| . quads)) ; what about EXISTS?
    	     (let ((not-triples (expand-triples quads)))
	       (let ((trs
		      (remove null?
		       (map (lambda (ntriple)
                              (if (null? (match-triples ntriple triples))
                                  ntriple '()))
			    not-triples))))
		 (values (if (null? trs) (list #f)
			     `((FILTER (|NOT EXISTS| ,@trs))))
			 bindings))))
    	    (else 
	     (values (list block) bindings)))))
    ((VALUES BIND) . ,rw/copy)
    (,symbol? . ,rw/copy)
    (,nulll? . ,rw/remove)
    (,list? . ,rw/list)))
