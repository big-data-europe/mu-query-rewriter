;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expanding triples and quads
(define flatten-graphs? (make-parameter #f))

(define (expand-triples block #!optional (bindings '()) mapp bindingsp)
  (rewrite block bindings (expand-triples-rules mapp bindingsp)))

(define (recursive-expand-triples block #!optional (bindings '()) mapp bindingsp)
  (rewrite block bindings (recursive-expand-triples-rules mapp bindingsp)))

(define (expand-triple-rule #!optional mapp bindingsp)
  (let ((mapp (or mapp values))
        (bindingsp (or bindingsp (lambda (triples bindings) bindings))))
    (lambda (triple bindings)
      (let ((triples (map mapp (expand-triple triple))))
        (values triples (bindingsp triples bindings))))))

(define (expand-triples-rules #!optional mapp bindingsp)
  `(((@QueryUnit @UpdateUnit @Query @Update CONSTRUCT WHERE
      DELETE INSERT |DELETE WHERE| |INSERT DATA|) . ,rw/quads)
    (( @Prologue @Dataset @Using) . ,rw/copy)
    ((FILTER BIND MINUS OPTIONAL UNION VALUES
      @SubSelect |GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,select? . ,rw/copy)
    ((UNION) . ,rw/union)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cddr block) bindings)))
	  (if (flatten-graphs?)
              (values rw new-bindings)
	      (values `((GRAPH ,(second block) ,@rw))
		      (cons-binding (second block) 'named-graphs new-bindings))))))
    (,annotation? . ,rw/copy)
    (,triple? . ,(expand-triple-rule mapp bindingsp))
    (,list? . ,rw/list)
    (,symbol? . ,rw/copy)))

(define (recursive-expand-triples-rules #!optional mapp bindingsp)
  `(((@QueryUnit @UpdateUnit @Query @Update CONSTRUCT WHERE
      DELETE INSERT |DELETE WHERE| |INSERT DATA|) . ,rw/quads)
    ((@Prologue @Dataset @Using) . ,rw/copy)
    (,symbol? . ,rw/copy)
    (,annotation? . ,rw/copy)
    ((MINUS OPTIONAL) . ,rw/quads)
    ((UNION) . ,rw/union)
    (,select? . ,rw/copy)
    ((@SubSelect) . ,rw/subselect)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cddr block) bindings)))
            (if (flatten-graphs?)
                (values rw new-bindings)
                (values `((GRAPH ,(second block) ,@rw)) new-bindings)))))
    (,triple? . ,(expand-triple-rule mapp bindingsp))
    ((VALUES FILTER BIND |GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,list? . ,rw/list)))

;; be careful with optionals etc:
;; (expand-graphs '((GRAPH <G>  (OPTIONAL (?s ?p ?o)) (?s a <Truck>)))))
;;             => '((OPTIONAL (GRAPH <G> (?s ?p ?o))) (GRAPH <G> (?s a <Truck>)))
(define (expand-graphs statements #!optional (bindings '()))
  (rewrite statements bindings expand-graphs-rules))

(define eg-graph (make-parameter #f))

(define expand-graphs-rules
  `(((@SubSelect) . ,rw/subselect)
    (,select? . ,rw/copy)
    ((OPTIONAL WHERE) . ,rw/quads)
    ((UNION) . ,rw/union)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (match block
            ((`GRAPH graph . quads)
             (parameterize ((eg-graph graph))
               (rewrite quads bindings))))))
    (,triple? 
     . ,(lambda (triple bindings)
          (values `((GRAPH ,(eg-graph) ,triple))
                  bindings)))
    (,annotation? 
     . ,(lambda (annotation bindings)
          (values `((GRAPH ,(eg-graph) ,annotation))
                  bindings)))
    ((VALUES FILTER BIND MINUS |GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,list? . ,rw/list)
    (,symbol? . ,rw/copy)))

(define (replace-triple block old-triple new-triple)
  (rewrite block '() (replace-triple-rules old-triple new-triple)))

(define (replace-triple-rules old-triple new-triple)
  `(((@QueryUnit @UpdateUnit @Query @Update CONSTRUCT WHERE
      DELETE INSERT |DELETE WHERE| |INSERT DATA| *REWRITTEN*) . ,rw/quads)
    ((@Prologue @Dataset @Using) . ,rw/copy)
    (,symbol? . ,rw/copy)
    (,annotation? . ,rw/copy)
    ((MINUS OPTIONAL) . ,rw/quads)
    ((UNION) . ,rw/union)
    (,select? . ,rw/copy)
    ((@SubSelect) . ,rw/subselect)
    ((GRAPH) 
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cddr block) bindings)))
            (values `((GRAPH ,(second block) ,@rw)) new-bindings))))
    (,triple? 
     . ,(lambda (triple bindings)
          (if (equal? triple old-triple) 
              (values (list new-triple) bindings)
              (values (list triple) bindings))))
    ((VALUES FILTER BIND |GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,list? . ,rw/list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Graphs
(define (get-all-graphs query)
  (let ((query (if (procedure? query) (query) query)))
    (let-values (((rewritten-query bindings) (rewrite-query query extract-graphs-rules)))
      (let ((query-string (write-sparql rewritten-query)))
        (let-values (((vars iris) (partition sparql-variable? (get-binding/default 'all-graphs bindings '()))))
          (if (null? vars) iris
              (delete-duplicates
               (append iris
                       (map cdr (join (sparql-select query-string from-graph: #f)))))))))))

(define extract-graphs-rules
  `(((@QueryUnit @UpdateUnit) . ,rw/quads)
    (,annotation? . ,rw/copy)
    ((@Query @Update) 
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
            (let ((graphs (delete-duplicates
                           (filter sparql-variable?
                                   (get-binding/default 'all-graphs new-bindings '())))))
            (values `((@Query
                       ,@(insert-child-before 'WHERE `(|SELECT DISTINCT| ,@graphs) rw)))
                    new-bindings)))))
    ((WHERE)
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rw/quads block bindings)))
            (values rw new-bindings))))
    ((INSERT DELETE)
     . ,(lambda (block bindings)
          (let-values (((_ new-bindings) (rw/quads block bindings)))
            (values '() new-bindings))))
    ((@Prologue) . ,rw/copy)
    ((@Dataset @Using) 
     . ,(lambda (block bindings)
          (let ((graphs (map second (cdr block))))
            (values (list block) (fold-binding graphs 'all-graphs append '() bindings)))))
    (,select? . ,rw/remove)
    ((@SubSelect)
     . ,(lambda (block bindings)
          (match block
            ((@SubSelect (label . vars) . rest)
	     (let* ((dataset (get-child-body '@Dataset rest))
		    (graphs (and dataset (map second dataset))))
               (let-values (((rw new-bindings) (rewrite (get-child-body 'WHERE rest) bindings)))
		 (values
		  `((@SubSelect (,label ,@vars) ,@(replace-child-body 'WHERE rw rest)))
		  (if graphs
		      (fold-binding graphs 'all-graphs append '() new-bindings)
		      new-bindings))))))))
    ((CONSTRUCT SELECT) . ,rw/remove)
    ((GRAPH) . ,(lambda (block bindings)
                  (match block
                    ((`GRAPH graph . quads)
                     (let-values (((rw new-bindings) 
                                   (rewrite quads (cons-binding graph 'all-graphs bindings))))
                       (values `((GRAPH ,graph ,@rw)) new-bindings))))))

    ((UNION) 
     . ,(rw/lambda (block)
          (with-rewrite ((rw (rewrite (cdr block))))
	    `((UNION ,@rw)))))
    ((VALUES) ;; order-dependent
     . ,(lambda (block bindings)
          (let ((graphs (get-binding/default 'all-graphs bindings '())))
            (match block
              ((`VALUES vars . vals)
               (if (pair? vars)
                   (values (list block) bindings)
                   (if (member vars graphs)
                       (values (list block)
                               (update-binding 'all-graphs 
                                               (append vals (delete vars graphs))
                                               bindings))
                       (values (list block) bindings))))))))
    (,quads-block? . ,rw/quads)
    (,triple? . ,rw/copy)
    ((VALUES FILTER BIND) . ,rw/copy)
    ((|GROUP BY| OFFSET LIMIT) . ,rw/copy)
    (,list? . ,rw/list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find Graphs for Triples
(define (find-triple-graphs triple block)
  (delete-duplicates (rewrite block '() (find-triple-graphs-rules triple))))

(define (find-triple-graphs-rules triple)
  `(((GRAPH)
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite (cddr block))))
            (values
             (if (nulll? rw) '()
                 (append (list (second block))
                         (filter symbol? rw)))
             bindings))))
    ((@SubSelect)
     . ,(lambda (block bindings)
          (rewrite (cddr block))))
    ((OPTIONAL WHERE UNION)
     . ,(lambda (block bindings)
          (rewrite (cdr block))))
    ((OPTIONAL) . ,rw/quads)
    (,triple? 
     . ,(lambda (block bindings)
          (values
           (if (fail? (map rdf-equal? block triple)) '()
               (list #t))
           bindings)))
    ((FILTER VALUES BIND) . ,rw/remove)
    (,annotation? . ,rw/remove)
    (,list?
     . ,(lambda (block bindings)
          (rewrite block)))))

(define (get-triple-graphs triple bindings)
  (let ((vars (filter sparql-variable? triple))
        (triple (map a->rdf:type triple)))
    (cdr-or 
     (assoc triple (get-binding/default* vars 'graphs bindings '()))
     '())))

(define (update-triple-graphs new-graphs triple bindings)
  (let ((vars (filter sparql-variable? triple))
        (triple (map a->rdf:type triple)))
    (fold-binding* triple
                   vars
                   'graphs
                   (lambda (triple graphs-list)
                     (alist-update-proc triple 
					(lambda (graphs)
					  (if graphs (append new-graphs graphs) new-graphs))
                                    graphs-list))
                   '()
                   bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get Vars
(define (get-vars block)
  (delete-duplicates (rewrite block '() get-vars-rules)))

(define get-vars-rules
  `((,triple?
     . ,(lambda (block bindings)
          (values (filter sparql-variable? (flatten block)) bindings)))
    ((GRAPH) 
     . ,(lambda (block bindings)
          (match block
                 ((`GRAPH graph . rest)
                  (let-values (((rw b) (rewrite rest bindings)))
                    (if (sparql-variable? graph)
                        (values (cons graph rw) bindings)
                        (values rw bindings)))))))
    ((UNION)
     . ,(lambda (block bindings)
          (values (delete-duplicates
                   (join
                    (map rewrite (cdr block))))
                  bindings)))
    (,quads-block? 
     . ,(lambda (block bindings)
          (rewrite (cdr block))))
    ((BIND)
     . ,(lambda (block bindings)
          (match block
                 ((`BIND (`AS exp var))
                  (values (list var) bindings)))))
    ((@SubSelect)
     . ,(lambda (block bindings)
          (match block
                 ((`@SubSelect (_ . vars) . rest)
                  (if (equal? vars '(*))
                      (rewrite rest '() get-vars-rules)
                      (values (extract-subselect-vars vars) bindings))))))
    ((FILTER VALUES) . ,rw/remove)
    (,list? 
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite block)))
            (values (delete-duplicates rw) new-bindings))))))
