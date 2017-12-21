;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temp constraints
(define *temp-graph* (config-param "TEMP_GRAPH" '<http://mu.semte.ch/rewriter/temp> read-uri))

(define *use-temp?* (config-param "USE_TEMP" #f))

(define *insert-into-temp?* (make-parameter #f))

;; $query is broken; use header for now, and fix $query
(define (use-temp?)
  (cond ((header 'use-temp-graph) ; (($query) 'use-temp-graph))
         => (lambda (h) (equal? h "true")))
        (else (*use-temp?*))))

(define temp-rules
  `(((@QueryUnit @Query) . ,rw/continue)
    ((@Prologue @Dataset) . ,rw/copy)
    (,annotation? . ,rw/copy)
    ((CONSTRUCT) 
     . ,(lambda (block bindings)
          (values (list block) (update-binding 'triple (second block) bindings))))
    ((WHERE)
     . ,(lambda (block bindings)
          (let ((triple (get-binding 'triple bindings)))
            (values `((WHERE
                       (UNION ,(cdr block)
                              ((GRAPH ?temp ,triple)
                               (VALUES (?temp) (,(*temp-graph*)))))))
                    bindings))))))

(define (sync-temp-query)
  `(@UpdateUnit
    (@Update
     (@Prologue)
     (DELETE
      (GRAPH ,(*temp-graph*) (?s ?p ?o)))
     (INSERT
      (?s ?p ?o))
     (WHERE
      (GRAPH ,(*temp-graph*) (?s ?p ?o))))))

(define (sync-temp)
  (parameterize ((*rewrite-graph-statements?* #f)
                 (*use-temp?* #t))
                (sparql-update
                 (write-sparql
                  (rewrite-query (sync-temp-query) (main-transformation-rules))))))
