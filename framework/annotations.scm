;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations
(define (annotation? exp)
  (and (pair? exp)
       (equal? (car exp) '@Annotation)))

(define (values? exp)
  (and (pair? exp) (equal? (car exp) '*values*)))

(define (get-annotations query bindings)
  (let ((rw (delete-duplicates (rewrite-query query (get-annotations-rules))))
        (functional-property-substitutions (get-binding/default 'functional-property-substitutions bindings '())))
    (let-values (((vals annotations) (partition values? rw)))
      (if (null? functional-property-substitutions) annotations
          (map (lambda (annotation)
                 (match annotation
                        ((key var)
                         (let ((vals (alist-ref var functional-property-substitutions)))
                           (if vals
                               `(,key ,var ,(cons var vals))
                               annotation)))
                        (else annotation)))
               annotations)))))

(define (update-annotation-values annotation valss)
  (match annotation
    ((key var)
     (let ((vals (alist-ref var valss)))
       (if vals
           `(,key ,var ,vals)
           annotation)))
    (else annotation)))

(define (annotations-queries annotations query)
  (if (*calculate-annotations?*)
      (let-values (((pairs singles) (partition pair? annotations)))
        (let* ((pairs (remove values? pairs))
               (vars (filter sparql-variable?  (delete-duplicates (map second pairs)))))
          (if (or (null? pairs) (null? vars))
              (values #f #f)
              (let ((aqueries (rewrite-query query (query-annotations-rules vars))))
                (values aqueries pairs)))))
      (values #f #f)))

(define (query-annotations aquery annotations-pairs)
  (and aquery
       (join
        (map (lambda (row)
               (filter pair?
                       (map (lambda (annotation binding)
                              (match binding
                                ((var . val)
                                 (if (and (equal? (->string var)
                                                  (substring (->string (second annotation)) 1))
                                          (or (null? (cddr annotation))
                                              (member val (third annotation))))
                                     (list (first annotation) val)
                                     '()))))
                            annotations-pairs row)))
             (sparql-select (if (string? aquery)
                                aquery
                                (write-sparql aquery)))))))

(define (get-annotations-rules)
  `(((@QueryUnit @UpdateUnit)
     . ,(lambda (block bindings)
          (let-values (((rw b) (rewrite (cdr block) bindings)))
            (values (list rw) b))))
    ((@Query @Update)
     . ,(lambda (block bindings)
          (rewrite (list (cdr block)) bindings)))
    ((WHERE OPTIONAL)
     . ,(lambda (block bindings)
          (rewrite (cdr block) bindings)))
    (,annotation? 
     . ,(lambda (block bindings)
          (values
           (match block
             ((`@Annotation `access key)  (list key))
             ((`@Annotation `access key var) (list (list key var)))
             (else (error (format "Invalid annotation: ~A" block))))
           bindings)))
    (,select? . ,rw/remove)
    ((@SubSelect)
     . ,(lambda (block bindings)
          (match block
            ((@SubSelect (label . vars) . rest)
             (rewrite rest bindings)))))
    ((GRAPH)
     . ,(lambda (block bindings)
          (rewrite (cddr block) bindings)))
    ((UNION) 
     . ,(lambda (block bindings)
          (let-values (((rw b) (rewrite (cdr block))))
            (let ((vals (apply merge-alists (map second (filter values? rw))))
                  (quads (filter (compose not values?) rw)))
            (values (append quads `((*values* ,vals)))
                    b)))))
    (,triple? . ,rw/remove)
    ((|GROUP BY| ORDER LIMIT) . ,rw/remove)
    ((VALUES)
     . ,(lambda (block bindings)
          (match block
            ((`VALUES vars . vals)
             (values
              (if (pair? vars)
                  `((*values*
                    ,(apply
                      merge-alists
                      (map (lambda (vallst)
                             (map (lambda (var val)
                                    (list var
                                          (expand-namespace val (append (*namespaces*) (query-namespaces)))))
                                  vars vallst)) 
                           vals))))
                  `((*values*
                     ,(map (lambda (val)
                             `(,vars ,(expand-namespace val)))
                           vals))))
              bindings)))))
    ((@Prologue @Dataset @Using CONSTRUCT SELECT FILTER BIND |GROUP BY| OFFSET LIMIT INSERT DELETE) 
     . ,rw/remove)
    (,list?
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rewrite block bindings)))
            (let-values (((vals annotations) (partition values? rw)))
              (let ((merged-vals (apply merge-alists (map second vals))))
              (values (append (map (lambda (a)
                                     (update-annotation-values a merged-vals))
                                   annotations)
                              `((*values* ,merged-vals)))
                      new-bindings))))))))

(define (query-annotations-rules vars)
  `(((@UpdateUnit @QueryUnit) 
     . ,(lambda (block bindings)
          (let-values (((rw _) (rewrite (cdr block) bindings)))
            (values (list 
                     (map (lambda (query) `(@QueryUnit ,query)) 
                          rw))
                    bindings))))
    ((@Update @Query) 
     . ,(lambda (block bindings)
          (let-values (((rw _) (rewrite (cdr block) bindings)))
            (values `((@Query
                       ,@(insert-child-before 'WHERE
                                              `(SELECT DISTINCT ,@vars)
                                              rw)))
                    bindings))))
    ((@Using) 
     . ,(lambda (block bindings)
          (let-values (((rw _) (rewrite (cdr block) bindings)))
            (values `((@Dataset ,@rw)) bindings))))
    ((@Dataset @Prologue WHERE) . ,rw/copy)
    ((INSERT DELETE) . ,rw/remove)
    ((LIMIT OFFSET |GROUP BY|) . ,rw/remove)
    (,select? . ,rw/remove)))

