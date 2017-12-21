;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming dependencies
(define (constraint-dependencies constraint-where-block bindings)
  (get-dependencies 
   (list constraint-where-block) 
   (map car (take (dependency-substitutions) 3))))

(define (get-dependencies query bound-vars)
  (rewrite query '() (dependency-rules bound-vars)))
   
(define (minimal-dependency-paths source? sink? dependencies)
  (let-values (((sources nodes) (partition (compose source? car) dependencies)))
    (let loop ((paths (join (map (match-lambda ((head . rest) (map (lambda (r) (list r head)) rest)))
                                 sources)))
               (finished-paths '()))
      (if (null? paths) finished-paths
          (let* ((path (car paths))
                 (head (car path))
                 (neighbors (remove (lambda (n) (member n path)) ;; remove loops
                                     (delete head (or (alist-ref head nodes) '())))))
            (if (or (sink? head)
                    (null? neighbors))
                (loop (cdr paths) (cons path finished-paths))
                (loop (append (cdr paths) 
                              (filter values
                                      (map (lambda (n) 
                                             (and (not (source? n)) (cons n path)))
                                           neighbors)))
                      finished-paths)))))))
                                  
(define (concat-dependency-paths paths)
  (let loop ((paths paths) (dependencies '()))
    (if (null? paths)
        (map (lambda (pair) 
               (cons (car pair) (delete-duplicates (cdr pair))))
             dependencies)
        (match (reverse (car paths))
          ((head . rest)
           (loop (cdr paths)
                 (fold (lambda (var deps)
                         (alist-update var (cons head (or (alist-ref var deps) '())) deps))
                       dependencies
                       rest)))))))

(define constraint-graph (make-parameter #f))

;; variables from FILTER, VALUES and BIND 
(define restricted-variables (make-parameter '()))

(define (deps-updater vars)
  (lambda (var deps) 
    (alist-update
     var (delete-duplicates
          (append (delete var vars)
                  (or (alist-ref var deps) '())))
     deps)))

(define (triple-dependency-vars triple matched-triple)
  (let ((vars* (filter sparql-variable? triple)))
    (if (and (equal? vars* matched-triple)
             (null? (lset-intersection equal? (restricted-variables) vars*)))
        vars*
        (cons (constraint-graph) vars*))))

(define (dependency-rules matched-triple)
  `((,triple? 
     . ,(lambda (triple bindings)
          (let ((vars (triple-dependency-vars triple matched-triple)))
            (values (list triple)
                    (update-binding 'dependency-paths
                                    (fold (deps-updater vars) 
                                          (or (get-binding 'dependency-paths bindings) '())
                                          vars)
                                    bindings)))))
    (,annotation? . ,rw/copy)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (match block
            ((`GRAPH graph . rest)
             (parameterize ((constraint-graph graph))
               (rewrite rest (cons-binding graph 'constraint-graphs bindings)))))))
     ((UNION) . ,rw/union)
     ((OPTIONAL) . ,rw/quads)
     ((VALUES)
      . ,(lambda (block bindings)
           (match block
             ((`VALUES vars . vals)
              (if (symbol? vars)
                  (values '() bindings)
                  (values '()
                          (update-binding
                           'dependency-paths
                           (fold (deps-updater vars)
                                 (or (get-binding 'dependency-paths bindings) '())
                                 (filter sparql-variable? vars))
                           bindings)))))))
     ((BIND)
      . ,(lambda (block bindings)
	   (match block
	     ((`BIND (`AS exp var))
	      (values '()
		      (fold-binding var 'dependency-paths
				    (lambda (var paths-list)
				      (fold (lambda (new-var paths)
					      (alist-update-proc new-var
                                                (lambda (path)
                                                  (cons var (or path '())))
                                                paths))
					    paths-list
					    (filter sparql-variable? (flatten exp))))
				    '()
				    bindings))))))
     ((@SubSelect)
      . ,(lambda (block bindings)
           (match block
             ((`@SubSelect ((or `SELECT `|SELECT DISTINCT| `|SELECT REDUCED|) . vars) . rest)
              (rewrite (get-child-body 'WHERE rest) bindings)))))
     ((WHERE)
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
            (let* ((paths (get-binding 'dependency-paths new-bindings))
                   (source? (lambda (var) (member var matched-triple)))
                   (sink? (lambda (x) (member x (get-binding/default 'constraint-graphs new-bindings '())))))
              (values
               (concat-dependency-paths (minimal-dependency-paths source? sink? paths))
               bindings)))))
     ((FILTER) . ,rw/copy)
     (,list? 
      . ,(lambda (block bindings)
           (let ((vars (filter sparql-variable?
                               (flatten (filter (block-member '(FILTER BIND VALUES)) block)))))
             (parameterize ((restricted-variables (append vars (restricted-variables))))
               (rw/list block bindings)))))))

(define (block-member heads)
  (lambda (exp)
    (and (pair? exp) 
         (member (car exp) heads))))
