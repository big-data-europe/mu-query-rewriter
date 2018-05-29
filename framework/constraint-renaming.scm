;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings, Substitutions and Renamings
(define constraint-where (make-parameter '()))
(define matched-triple (make-parameter '()))
(define constraint-substitutions (make-parameter '()))
(define dependency-substitutions (make-parameter '()))
(define dependencies  (make-parameter '()))
(define renaming-dependencies (make-parameter '()))
(define context-graph (make-parameter '()))

(define (current-substitution var bindings)
  (let ((substitutions (get-binding/default 'constraint-substitutions bindings '())))
    (a->rdf:type (alist-ref var substitutions))))

(define (current-substitution-recursive block bindings)
  (let rec ((exp block))
    (cond ((null? exp) '())
	  ((pair? exp) 
           (cons (rec (car exp))
                 (rec (cdr exp))))
	  ((sparql-variable? exp)
           (current-substitution exp bindings))
	  (else exp))))

(define (dependency-substitution var bindings)
  (let ((substitutions (dependency-substitutions)))
    (a->rdf:type (alist-ref var substitutions))))

(define (substitution-or-value var bindings)
  (if (sparql-variable? var)
      (current-substitution var bindings)
      var))

(define (deps var bindings)
  (let ((dependencies (renaming-dependencies)))
    (filter (lambda (v) (member v (or (alist-ref var dependencies) '())))
	    (matched-triple))))

(define (keys var bindings)
  (map (cut dependency-substitution <> bindings) (deps var bindings)))

(define (renaming var bindings)
  (let ((renamings
         (get-binding/default* (keys var bindings) (deps var bindings) bindings '())))
    (alist-ref var renamings)))

(define (update-renaming var substitution bindings)
  (fold-binding* substitution
                 (keys var bindings)
                 (deps var bindings)
                 (cut alist-update var <> <>)
                 '()
                 bindings))

(define (project-substitutions vars substitutions)
  (filter (lambda (substitution)
            (member (car substitution) vars))
          substitutions))

(define unique-variable-substitutions
   (lambda (uvs)
             (map (lambda (var)
                    `(,var . ,(gensym var)))
                  uvs)))

;; could we (re-)use match-triple here?
(define (triple-match? triple-a triple-b)
  (let ((check (lambda (u v) (or (sparql-variable? u) (sparql-variable? v) (rdf-equal? u v)))))
    (null? (remove values (map check triple-a triple-b)))))

(define (make-initial-substitutions triple-a triple-b)
  (append (map cons triple-a triple-b)
          (unique-variable-substitutions (*unique-variables*))))

(define (update-renamings new-bindings)
  (let ((substitutions (get-binding 'constraint-substitutions new-bindings)))
    (fold (lambda (substitution bindings)
	    (update-renaming (car substitution) (cdr substitution) bindings))
	  (delete-binding 'constraint-substitutions new-bindings)
	  substitutions)))

(define (project-substitution-bindings vars bindings)
  (update-binding 
   'constraint-substitutions 
   (project-substitutions vars (get-binding 'constraint-substitutions bindings))
   bindings))

(define (merge-substitution-bindings vars old-bindings new-bindings)
  (update-binding 'constraint-substitutions
		  (append
		   (project-substitutions
		    vars (get-binding 'constraint-substitutions new-bindings))
		   (get-binding 'constraint-substitutions old-bindings))
		  new-bindings))

(define (substituted-subselect-vars vars bindings)
  (filter sparql-variable?
	  (map (lambda (var)
		 (if (equal? var '*) var
		     (current-substitution var bindings)))
	       vars)))

(define (new-substitutions exp bindings)
  (let-values (((renamed-exp substitutions)
		(let loop ((exp exp) (renamed-exp '()) (substitutions '()))
		  (if (null? exp) 
		      (values renamed-exp substitutions)
		      (let ((var (car exp)))
			(cond ((and (pair? var) (not (list? var))) ; cons pair
                               (loop (cdr exp)
                                     (append renamed-exp (list var))
                                     substitutions))
                              ((pair? var)
			       (let-values (((sub subs) (loop var '() substitutions)))
				 (loop (cdr exp) 
				       (append renamed-exp (list sub))
				       subs)))
			      ((not (sparql-variable? var))
			       (loop (cdr exp) 
				     (append renamed-exp (list var))
				     substitutions))
			      ((current-substitution var bindings)
			       => (lambda (v) 
				    (loop (cdr exp) 
					  (append renamed-exp (list v))
					  substitutions)))
			      ((renaming var bindings)
			       => (lambda (v)
				    (loop (cdr exp)
					  (append renamed-exp (list v))
					  (cons `(,var . ,v) substitutions))))
			      (else (let ((new-var (gensym var)))
				      (loop (cdr exp)
					    (append renamed-exp (list new-var))
					    (cons `(,var . ,new-var) substitutions))))))))))
    (values renamed-exp
	    (update-binding 'constraint-substitutions
			    (append substitutions
				    (get-binding 'constraint-substitutions bindings))
			    bindings))))

(define (apply-constraint-rules constrained-triple)
  `(((@QueryUnit) 
     . ,(lambda (block bindings)
          (rewrite (cdr block) bindings)))
    (,annotation? . ,rw/copy)
    ((@Query)
     . ,(lambda (block bindings)
          (parameterize ((constraint-where (list (get-child 'WHERE (cdr block)))))
                        (rewrite (cdr block) bindings))))
    ((CONSTRUCT) 
     . ,(lambda (block bindings)
          (let-values (((triples _) (parameterize ((flatten-graphs? #t)) (expand-triples (cdr block)))))
            (if (= (length triples) 1)
                (let-values (((rw new-bindings) (rewrite triples bindings)))
                  (let* ((constraints (filter (compose not fail?) rw))
                         (constraint (if (equal? (length constraints) 1)
                                         (car constraints)
                                         (error-condition "Invalid constraint rewriting (multiple constraints)" rw))))
                    (if (where?)
                        (values `((*REWRITTEN* ,@constraint)) 
                                new-bindings)
                        (values
                         `((*REWRITTEN* ,constrained-triple))
                         (fold-binding constraint
                                       (if (insert?) 'insert-constraints 'delete-constraints)
                                       append-unique '() 
                                       new-bindings)))))
                (error-condition "Invalid constraint: multiple triples in CONSTRUCT block" triples)))))
    ((@Dataset) . ,rw/copy)
    (,triple? 
     . ,(lambda (triple bindings)
          (if (triple-match? triple constrained-triple)
              (let ((initial-substitutions (make-initial-substitutions triple constrained-triple)))
                (parameterize ((matched-triple triple)
                               (dependency-substitutions initial-substitutions))
                              (let-values (((rw new-bindings)
                                            (rewrite (constraint-where)
                                                     (update-binding 'constraint-substitutions
                                                                     initial-substitutions bindings)
                                                     rename-constraint-rules)))
                                (let ((cleaned-bindings #f))
                                  (values (list (get-child-body 'WHERE rw))
                                          new-bindings)))))
              (values '() bindings))))
    (,list? . ,rw/remove)))

(define rename-constraint-triple
  (lambda (triple bindings)
    (let ((graph (context-graph)))
      (let-values (((renamed-quad new-bindings) (new-substitutions (cons graph triple) bindings)))
        (values             
         (if (and (use-temp?) (update?) (where?))
             `((UNION (,(cdr renamed-quad)) ((GRAPH ,(*temp-graph*) ,(cdr renamed-quad)))))
             (list (cdr renamed-quad)))
         new-bindings)))))

(define rename-constraint-rules
  `((,symbol? . ,rw/copy)
    ((CONSTRUCT) . ,rw/remove)
    (,annotation? 
     . ,(lambda (block bindings)
          (match block
            ((`@Annotation label key) (values (list block) bindings))
            ((`@Annotation label key var)
             (let-values (((rw new-bindings) (new-substitutions (list var) bindings)))
               (values `((@Annotation ,label ,key ,@rw)) new-bindings))))))
    ((@SubSelect)
     . ,(lambda (block bindings) 
          (match block
	    ((`@SubSelect (label . vars) . rest)
             (let ((subselect-vars (extract-subselect-vars vars)))
             (let-values (((rw new-bindings)
			   (rewrite (get-child-body 'WHERE rest)
                                    (project-substitution-bindings subselect-vars bindings))))
               (let ((merged-bindings (merge-substitution-bindings subselect-vars bindings new-bindings)))
                 (fail-or-null rw merged-bindings
                   `((@SubSelect 
                      (,label ,@(substituted-subselect-vars vars merged-bindings))
                      ,@(replace-child-body 'WHERE rw rest)))))))))))
    ((WHERE)
     . ,(lambda (block bindings)
	  (parameterize ((renaming-dependencies (constraint-dependencies block bindings)))
	    (let-values (((rw new-bindings)
			  (rewrite (cdr block)
				   (update-binding
				    'dependencies (constraint-dependencies block bindings) 
				    bindings))))
	      (let ((new-bindings (update-renamings new-bindings)))
	      (if (fail? rw)
		  (values `((WHERE #f)) new-bindings)
                  (values `((WHERE ,@rw)) new-bindings)))))))

    ((UNION) . ,rw/union)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (match block
            ((`GRAPH graph . rest)
             (parameterize ((context-graph graph))
               (let-values (((rw new-bindings) (rewrite (cddr block) bindings)))
                 (fail-or-null rw new-bindings
                   `((GRAPH ,(substitution-or-value graph new-bindings)
                                    ,@rw)))))))))
    (,triple? . ,rename-constraint-triple)
    ((VALUES)
     . ,(lambda (block bindings)
	  (let-values (((rw new-bindings) (new-substitutions block bindings)))
	    (match rw
	      ((`VALUES vars . vals)
               (let ((vals (simplify-values vars vals bindings)))
                 (if (null? vals) (values '() new-bindings)
                     (values (list vals)
                             new-bindings))))))))
    ((FILTER) 
     . ,(lambda (block bindings)
	  (let-values (((rw new-bindings) (new-substitutions block bindings)))
	    (validate-filter rw new-bindings))))
    ((BIND) 
     . ,(lambda (block bindings)
	  (let-values (((rw new-bindings) (new-substitutions block bindings)))
	    (match rw
	      ((`BIND (`AS exp var))
	       (if (sparql-variable? var)
		   (values (list rw) new-bindings)
                   (error-condition "Invalid constrained BIND form" rw)))))))
    (,quads-block? . ,rw/quads)
    (,list? . ,rw/list)))

