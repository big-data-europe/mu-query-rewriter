;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query ordering and cleanup

;; Virtuoso bug (?) when VALUES statement is not after all statements
;; whose variables it binds. Maybe not a bug at all. To think about.
(define (reorder block)
  (let loop ((statements block) (new-statements '())
             (values-statements '()) (annotations '()))
    (cond ((null? statements) (delete-duplicates
                               (append 
                                (reverse annotations)
                                (reverse new-statements)
                                (reverse values-statements))))
          ((equal? (caar statements) 'VALUES)
           (loop (cdr statements) new-statements 
                 (cons (car statements) values-statements) annotations))
          ((annotation? (car statements))
           (loop (cdr statements) new-statements
                 values-statements (cons (car statements) annotations)))
          (else
           (loop (cdr statements) (cons (car statements) new-statements) 
                 values-statements annotations)))))

(define (graph-equal? graph statement)
  (and (equal? (car statement) 'GRAPH)
       (equal? (cadr statement) graph)))

(define (group-graph-statements statements)
  (let loop ((statements statements))
    (if (nulll? statements) '()
        (match (car statements)
          ((`GRAPH graph . rest)
            (let-values (((same-graphs others) (partition (cut graph-equal? graph <>) (cdr statements))))
              (cons `(GRAPH ,graph ,@(delete-duplicates (append rest (join (map cddr same-graphs)))))
                    (loop others))))
          (statement (cons statement (loop (cdr statements))))))))

(define clean (compose delete-duplicates group-graph-statements reorder))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optimizing duplicates 
;; two ways -- should be harmonized
(define level-quads (make-parameter '()))

(define (rewritten? block) (equal? (car block) '*REWRITTEN*))

(define (new-level-quads new-quads)
  (append 
   (level-quads)
   (join (map cdr (filter rewritten? new-quads)))))

(define (optimize-duplicates block)
  (lset-difference equal?
                   (clean (filter pair? block))
                   (level-quads)))

(define collect-level-quads-rules
  `(((GRAPH)
     . ,(lambda (block bindings)
          (match block
           ((`GRAPH graph . rest)
            (values
             (map (lambda (triple)
                    (cons graph triple))
                  (filter triple? rest)) ; not quite right; what about GRAPH ?g1 { GRAPH ?g2 { s p o } } ?
             bindings)))))
    ((VALUES BIND FILTER) . ,rw/copy)
    (,list? . ,rw/remove)))

(define (collect-level-quads block)
  (rewrite block '() collect-level-quads-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functional Properties (and other optimizations)
(define (apply-optimizations block)
  (if (nulll? block) (values '() '())
      (let-values (((rw new-bindings) (rewrite (list block) '() (optimization-rules))))
        (if (equal? rw '(#f))
            (error (format "Invalid query block (optimizations):~%~A" (write-sparql block)))
             (values (clean (map (cut filter quads? <>) (filter quads? rw)))
                    (get-binding/default 'functional-property-substitutions new-bindings '())  )))))
;; (get-binding/default 'queried-functional-properties new-bindings '()))))))

(define *functional-property-cache* (make-hash-table))

(define *queried-properties-cache* (make-hash-table))

(define *transient-functional-property-cache* (make-parameter #f))

(define *transient-queried-properties-cache* (make-parameter #f))

(define (functional-property? p)
  (or (rdf-member p (*functional-properties*))
      (rdf-member p (*transient-functional-properties*))))

(define (queried-property? p)
  (or (rdf-member p (*queried-properties*))
      (rdf-member p (*transient-queried-properties*))))

(define (query-functional-property s p o)
  (if (rdf-member p (*functional-properties*))
      (query-functional-property* s p o *functional-property-cache*)
      (query-functional-property* s p o (*transient-functional-property-cache*))))

(define (query-functional-property* s p o cache)
  (hit-hashed-cache cache (list s p)
    (let ((results (sparql-select-unique "SELECT ~A  WHERE { ~A ~A ~A }" o s p o)))
      (and results (alist-ref (sparql-variable-name o) results)))))

(define (query-properties s p)
  (if (rdf-member p (*queried-properties*))
      (query-properties* s p *queried-properties-cache*)
      (query-properties* s p (*transient-functional-properties-cache*))))

(define (query-properties* s p cache)
  (hit-hashed-cache cache (list s p)
    (let ((results (sparql-select "SELECT ?o  WHERE { ~A ~A ?o }" s p)))
      (if results (map (cut alist-ref 'o <>) results)
          '()))))

(define (check-queried-property s p o)
  (rdf-member o (query-properties s p)))

(define fprops (make-parameter '((props) (subs))))

(define (replace-fprop elt)
  (if (sparql-variable? elt)
      (let ((subs (get-store 'subs)))
        (or (alist-ref elt subs) elt))
        elt))

(define (collect-fprops* block rec?)
    (let loop ((quads (filter quads? block))
               (props (append (get-returned-store/default 'props block '()) (get-store/default 'props '()))) ; ?
               (subs (append (get-returned-store/default 'subs block '()) (get-store/default 'subs '()))))
    (cond ((null? quads) `((props . ,props) (subs . ,subs)))
          ((triple? (car quads))
	   (match (car quads)
	     ((s p o) 
	      (if (functional-property? p)
		  (let ((current-value (cdr-when (assoc `(,s ,(a->rdf:type p)) props))))
		    (cond ((not current-value)
			   (loop (cdr quads) (cons `((,s ,(a->rdf:type p)) . ,o) props) subs))
			  ((and (sparql-variable? current-value)
				(not (sparql-variable? o)))
			   (loop (cdr quads) (cons `((,s ,(a->rdf:type p)) . ,o) props) 
				 (assoc-update current-value o subs)))
			  ((rdf-equal? o current-value) (loop (cdr quads) props subs))
			  ((sparql-variable? o)
			   (loop (cdr quads) props 
				 (assoc-update o current-value subs)))
			  (else #f)))
		  (loop (cdr quads) props subs)))))
          ((and (not rec?) (graph? (car quads)))
           (let ((inner (collect-fprops* (cddr (car quads)) #t)))
                 (and inner (loop (cdr quads)
                              (append (alist-ref 'props inner) props)
                              (append (alist-ref 'subs inner) subs)))))
          (else (loop (cdr quads) props subs)))))

(define (collect-fprops block #!optional rec?)
  (let ((collected-fprops  (collect-fprops* block rec?)))
    (and collected-fprops 
         ;; (append collected-fprops (store)))))
         (merge-alists collected-fprops (store)))))

(define this-level-quads (make-parameter '()))

(define last-level-quads (make-parameter '()))

(define (optimize-list block bindings)
  (let ((saved-llq (last-level-quads)) ; a bit... awkward
        (saved-tlq (this-level-quads))
        (key (gensym)))
    ;; (log-message "ol 0 (~A): ~A ~%~%" key block)
  (parameterize ((store (collect-fprops block))
                 (last-level-quads (this-level-quads))
                 (this-level-quads (append (last-level-quads)  (collect-level-quads block))))
     (if (store)
        (let-values (((rw new-bindings) (rw/list (filter quads? block) bindings)))
          ;; (log-message "ol 1 (~A): ~A ~%~%" key rw)
          (fail-or-null/low rw new-bindings
            (if (equal? (map (cut filter quads? <>) rw)
                        (list (filter quads? block)))
                (values rw new-bindings)
                (let ((rw (append-stores (join rw) values)))
                  (parameterize ((store (collect-fprops rw))
                                 (this-level-quads saved-tlq)
                                 (last-level-quads saved-llq))
                                (let-values (((rw2 nb2) (rewrite (list rw) new-bindings)))
                                  ;; (log-message "ol 2 (~A): ~A ~% key rw2)
                                  (fail-or-null rw2 nb2
                                    (list (append-stores (join rw2) clean)))))))))
        (values '(#f) bindings)))))

(define optimizations-graph (make-parameter #f))

(define (optimization-rules)
  `(((@UpdateUnit @QueryUnit @Update @Query) . ,rw/continue)
    ((@Prologue @Dataset GROUP |GROUP BY| LIMIT ORDER |ORDER BY| OFFSET) . ,rw/copy)
    (,select? . ,rw/copy)
    (,annotation? 
     . ,(lambda (block bindings)
          (match block
            ((`@Annotation `access key var) 
             (values `((@Annotation access ,key ,(replace-fprop var))) bindings))
            (else (values (list block) bindings)))))
    ((@SubSelect)
     . ,(lambda (block bindings)
          (let-values (((rw new-bindings) (rw/subselect block bindings)))
            (values rw new-bindings))))
    ((*store*) . ,rw/copy)
    ((VALUES) 
     . ,(lambda (block bindings)
          (if (member block (last-level-quads))
              (values '() bindings)
              (match block
                     ((`VALUES vars . vals)
                      (let ((simplified (simplify-values (map replace-fprop vars) vals bindings)))
                      (values
                       (if (null? simplified) '() (list simplified))
                       bindings)))))))
    ((FILTER) 
     . ,(lambda (block bindings)
          (if (member block (last-level-quads))
              (values '() bindings)
              (let ((subs (get-store 'subs))) ; (alist-ref 'subs (fprops))))
                (validate-filter (replace-variables block subs) bindings)))))
    ((BIND) 
     . ,(lambda (block bindings)
          (if (member block (last-level-quads))
              (values '() bindings)
              (values (list block) bindings)))) ; enough?
    ((GRAPH)
     . ,(lambda (block bindings)
          (parameterize ((optimizations-graph (second block)))
                        (let-values (((rw new-bindings) (rewrite (cddr block) bindings)))
                          (if (fail? rw) (values '(#f) new-bindings)
                              (let ((rw (delete-duplicates rw)))
                                (if (nulll? rw) (values '() new-bindings)
                                    (values (append-stores rw 
                                                           (lambda (quads)
                                                             `((GRAPH ,(second block) ,@quads))))
                                            new-bindings))))))))
    ((UNION)
     . ,(lambda (block bindings)
          (letrec ((union/1 (lambda (blocks)
                              (if (= (length blocks) 1) (car blocks) `((UNION ,@blocks))))))
            (let-values (((rw new-bindings)
                          (rewrite-union-fold (cdr block) bindings optimize-list 
                                        (lambda (b bindings)
                                          (fold-binding (get-binding/default 'functional-property-substitutions b '())
                                                        'functional-property-substitutions merge-alists
                                                        '() bindings)))))
              (if (member '() rw)
                  (let ((rw (remove null? rw)))
                    (if (fail? rw) (values '() new-bindings)
                        (values (intersect-stores rw (lambda (blocks) `((OPTIONAL ,@(union/1 blocks)))))
                                new-bindings)))
                  (let ((rw (remove not rw)))
                    (if (nulll? rw) (values '(#f) new-bindings)
                        (values (intersect-stores rw union/1) new-bindings))))))))
    (,quads-block? 
     . ,(lambda (block bindings)
          (match block
            ((`OPTIONAL (`GRAPH graph . rest))
             (values `((GRAPH ,graph (OPTIONAL ,@rest))) bindings))
            (else
             (let-values (((rw new-bindings) (optimize-list (cdr block) bindings)))
               (fail-or-null rw new-bindings
                             `((,(car block) ,@(delete-duplicates (filter quads? (join rw)))))))))))
    (,triple? 
     . ,(lambda (triple bindings)
          (if (member (cons (optimizations-graph) triple) (last-level-quads))
              (values '() bindings)
              (match (map replace-fprop triple)
                ((s p o)
                 (cond ((and (*query-functional-properties?*)
                             (not (sparql-variable? s))
                             (functional-property? p)
                             (sparql-variable? o))
                        (let ((o* (query-functional-property s p o)))
                          (if o*
                              (values (update-store 'subs `((,o . ,o*))
                                       (update-store 'props `(((,s ,p) . ,o*))
                                        `((,s ,p ,o*))))
                                      (fold-binding `((,o ,o*)) 'functional-property-substitutions 
                                                    merge-alists '() bindings))
                              (values `((,s ,p ,o)) bindings))))
                       ((and (not (sparql-variable? s))
                             (not (sparql-variable? o))
                             (queried-property? p))
                        (if (check-queried-property s p o)
                            (values `((,s ,p ,o)) bindings)
                            (values '(#f) bindings)))
                       (else (values `((,s ,p ,o)) bindings))))))))
                     
    (,list? . ,optimize-list)
    (,symbol? . ,rw/copy)))



