;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Renaming
(define *headers-replacements*
  (make-parameter
   `(("<SESSION>" mu-session-id uri))))

(define make-replacement-headers-pairs
  (match-lambda
   ((template key type)
    (let ((escape (case type
                    ((uri) sparql-escape-uri)
                    ((string) sparql-escape-ur)
                    (else values)))
          (val (header key)))
      (and val
           `(,template . ,(escape (header key))))))))

(define (replace-headers constraint-string)
  (string-translate* constraint-string 
                     (filter values
                             (map make-replacement-headers-pairs (*headers-replacements*)))))

(define (parse-constraint constraint)
  (let ((constraint
         (if (pair? constraint)
             constraint
             (parse-query constraint))))
    (car (recursive-expand-triples (list constraint) '() replace-a))))

(define (define-constraint key constraint)
  (case key
    ((read) (define-constraint* *read-constraint* constraint))
    ((write) (define-constraint* *write-constraint* constraint))
    ((read/write) 
     (begin
       (define-constraint* *read-constraint* constraint)
       (define-constraint* *write-constraint* constraint)))))

(define (define-constraint* C constraint)
  (if (procedure? constraint)
      (C (lambda () (parse-constraint (replace-headers (constraint)))))
      (C (lambda () (parse-constraint (replace-headers constraint))))))

; (@QueryUnit (@Query (@Prologue) (CONSTRUCT (?s ((?p ?o)))) (@Dataset) (WHERE (?s ((?p ?o))))))
(define optimize-constraint-rules
  `(((@QueryUnit @Query @Update
                 DELETE INSERT |DELETE WHERE| |INSERT DATA|) . ,rw/quads)
    ((CONSTRUCT @Prologue @Dataset @Using) . ,rw/copy)
    ((WHERE)
     . ,(lambda (block bindings)
          (let-values (((rw fpsubs fpqueries) (apply-optimizations (cdr block))))
            (values `((WHERE ,@(join rw)))
                    (update-binding 'functional-property-substitutions 
                                    fpsubs
                                    bindings)))))))

(define (optimize-constraint-headers) '(mu-session-id mu-call-id))

(define cache-key-headers (make-parameter #f))

(define (make-cache-key-headers)
  (map 
   (lambda (h) (or (header h) (gensym)))
   (optimize-constraint-headers)))

(define (optimize-constraint** C headers)
  (timed-let (format "Rewriting Constraint")
   (let-values (((a b)
    (parameterize ((*constraint-prologues* (constraint-prologues))
                   (*namespaces* (append (*namespaces*) (constraint-prefixes)))
                   (*transient-functional-property-cache* (make-hash-table))
                   (*transient-queried-properties-cache* (make-hash-table)))
      (rewrite-query C optimize-constraint-rules))))
     (values a b))))

(define optimize-constraint* (memoize optimize-constraint**))

(define (optimize-constraint C)
  (optimize-constraint* C (cache-key-headers)))
 
(define (get-constraint C)
  (optimize-constraint (if (procedure? C) (C) C)))
  ;;(if (procedure? C) (C) C))

(define (apply-constraint triple bindings C)
  (parameterize ((flatten-graphs? #f)
                 (cache-key-headers (make-cache-key-headers)))
                (let* ((C* (get-constraint C)))
                  (match triple
                         ((a (`^ b) c)
                          (rewrite (list C*) bindings (apply-constraint-rules (list c b a))))
                         ((a ((or `! `? `* `+) b) c)
                          (let-values (((rw new-bindings)
                                        (rewrite (list C*) 
                                                 bindings 
                                                 (apply-constraint-rules (list a b c)))))
                            (values (replace-triple rw  `(,a ,b ,c) triple)
                                    new-bindings)))
                         ((a b c)
                          (rewrite (list C*) bindings (apply-constraint-rules triple)))))))

(define (apply-read-constraint triple bindings)
  (apply-constraint triple bindings (*read-constraint*)))

(define (apply-write-constraint triple bindings)
  (apply-constraint triple bindings (*write-constraint*)))

(define (read-constraint) (get-constraint (*read-constraint*)))

(define (write-constraint) (get-constraint (*write-constraint*)))
