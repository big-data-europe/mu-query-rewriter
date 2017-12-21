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
      (C (lambda () (parse-constraint (constraint))))
      (C (parse-constraint constraint))))

(define (apply-constraint triple bindings C)
  (parameterize ((flatten-graphs? #f))
                (let* ((C* (if (procedure? C) (C) C)))
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
