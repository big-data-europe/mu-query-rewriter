;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;; prefix/namespace problems!
(define (construct-intermediate-graph-rules graph)
  `(((@QueryUnit @Query)  . ,rw/quads)
    ((CONSTRUCT)
     . ,(lambda (block bindings)
          (values `((INSERT
                     (GRAPH ,graph ,@(cdr block))))
                  bindings)))
    (,values . ,rw/copy)))
    
;; could be abstracted & combined with select-query-rules
(define (replace-dataset-rules graph)
  `(((@QueryUnit @Query) . ,rw/quads)
    ((GRAPH)
     . ,(rw/lambda (block) (cddr block)))
    ((@Prologue @SubSelect WHERE FILTER BIND |ORDER| |ORDER BY| |LIMIT|) . ,rw/copy)
    ((@Dataset) 
     . ,(rw/lambda (block)
          (parameterize ((*default-graph* graph)) ; not used
            `((@Dataset ,@(make-dataset 'FROM (list graph) #f))))))
    (,select? . ,rw/copy)
    (,triple? . ,rw/copy)
    (,list? . ,rw/list)))

(define (test query #!optional (cleanup? #t))
  (let ((rewritten-query (rewrite-query query (main-transformation-rules)))
        (intermediate-graph 
         (expand-namespace
          (symbol-append '|rewriter:| (gensym 'graph)))))
    (log-message "Creating intermediate graph: ~A " intermediate-graph)
    (print
     (sparql-update
      (write-sparql 
       (rewrite-query 
        (if (procedure? (*read-constraint*)) ((*read-constraint*)) (*read-constraint*))
        (construct-intermediate-graph-rules intermediate-graph)))))

    (parameterize ((*query-unpacker* sparql-bindings))
      (let ((r1 (sparql-select (write-sparql rewritten-query)))
            (r2 (sparql-select (write-sparql (rewrite-query query (replace-dataset-rules intermediate-graph))))))
        (log-message "~%==Expected Results==~%~A" r2)
        (log-message "~%==Actual Results==~%~A" r1)
        (when cleanup?
          (sparql-update (format "DELETE WHERE { GRAPH ~A { ?s ?p ?o } }" intermediate-graph)))
        (values r1 r2)))))

(define (test-call _)
  (let* (($$query (request-vars source: 'query-string))
         (body (read-request-body))
         ($$body (let ((parsed-body (form-urldecode body)))
                   (lambda (key)
                     (and parsed-body (alist-ref key parsed-body)))))
         (query-string (or ($$query 'query) ($$body 'query) body))
         (cleanup? (not (string-ci=? "false" (or ($$query 'cleanup) "true"))))
         (query (parse "test" query-string)))
    (let-values (((actual expected) (test query cleanup?)))
      `((expected . ,(list->vector expected))
        (actual . ,(list->vector actual))
        (equal . ,(equal? expected actual))))))

(define-rest-call 'POST '("test") test-call)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sandbox
(define (query-time-annotations annotations)
  (map (lambda (a)
         (match a
                ((key var vals)
                 `(,key ,(string->symbol (string-join (map symbol->string vals)))))
                (else a)))
       (remove values? annotations)))

(define (sandbox-call _)
  (let* ((body (read-request-body))
         ($$body (let ((parsed-body (form-urldecode body)))
                   (lambda (key)
                     (and parsed-body (alist-ref key parsed-body)))))
         (query-string ($$body 'query))
         (session-id (conc "\"" ($$body 'session-id) "\""))
         (read-constraint-string ($$body 'readconstraint))
	 (write-constraint-string ($$body 'writeconstraint))
	 (read-constraint (parse-constraint (replace-headers read-constraint-string)))
	 (write-constraint (parse-constraint (replace-headers write-constraint-string)))
         (query (parse "sandbox" query-string))
	 (fprops (map string->symbol
		      (string-split (or ($$body 'fprops) "") ", ")))
	 (qprops (map string->symbol
		      (string-split (or ($$body 'qprops) "") ", ")))  
	 (transient-fprops (map string->symbol
		      (string-split (or ($$body 'trfprops) "") ", ")))
	 (transient-qprops (map string->symbol
		      (string-split (or ($$body 'trqprops) "") ", ")))  
         (query-fprops? (equal? "true" ($$body 'query-fprops)))
         (unique-vars (map string->symbol
                              (string-split (or ($$body 'uvs) "") ", "))))
    (parameterize ((*write-constraint* write-constraint)
    		   (*read-constraint* read-constraint)
		   (*functional-properties* fprops)
                   (*transient-functional-properties* transient-fprops)
                   (*queried-properties* qprops)
                   (*transient-queried-properties* transient-qprops)
                   (*unique-variables* unique-vars)
                   (*query-functional-properties?* query-fprops?))
      (let-values (((rewritten-query bindings) (apply-constraints query)))
        (let* ((annotations (get-annotations rewritten-query bindings))
               (qt-annotations (and annotations (query-time-annotations annotations))))
               ;; (aquery (and annotations (annotations-query annotations rewritten-query)))
          (let-values (((aqueries annotations-pairs) (if annotations
                                                       (annotations-queries annotations rewritten-query)
                                                       (values #f #f))))
            (let* ((annotations-query-strings (and aqueries (map write-sparql aqueries)))
                   (queried-annotations (and annotations-query-strings
                                             (join (map (lambda (q)
                                                          (query-annotations q annotations-pairs))
                                                        annotations-query-strings))))
                   ;;(queried-annotations (and aquery (query-annotations aquery annotations-pairs)))
                  ;; (queried-annotations (and aquery (query-annotations aquery)))
                  (functional-property-substitutions (get-binding/default 'functional-property-substitutions bindings '())))
        (log-message "~%===Annotations===~%~A~%" annotations)
        (log-message "~%===Queried Annotations===~%~A~%"
                     (and queried-annotations (format-queried-annotations queried-annotations)))
        `((rewrittenQuery . ,(format (write-sparql rewritten-query)))
          (annotations . ,(format-annotations qt-annotations))
          (queriedAnnotations . ,(and queried-annotations (format-queried-annotations queried-annotations)))))))))))

(define (apply-call _)
  (let* ((body (read-request-body))
         ($$body (let ((parsed-body (form-urldecode body)))
                   (lambda (key)
                     (and parsed-body (alist-ref key parsed-body)))))
	 (read-constraint-string ($$body 'readconstraint))
	 (write-constraint-string ($$body 'writeconstraint))
	 (fprops (map string->symbol
		      (string-split (or ($$body 'fprops) "") ", ")))
	 (qprops (map string->symbol
		      (string-split (or ($$body 'qprops) "") ", ")))
	 (transient-fprops (map string->symbol
		      (string-split (or ($$body 'trfprops) "") ", ")))
	 (transient-qprops (map string->symbol
		      (string-split (or ($$body 'trqprops) "") ", ")))
         (query-fprops? (equal? "true" ($$body 'query-fprops)))
         (unique-vars (map string->symbol
                              (string-split (or ($$body 'uvs) "") ", "))))

    (log-message "~%Redefining read and write constraints~%")

    ;; brute-force redefining
    (set!
      *write-constraint*
      (make-parameter
       (parse-constraint
        write-constraint-string)))

    (set!
      *read-constraint*
      (make-parameter 
       (parse-constraint
        read-constraint-string)))

    (set! *functional-properties* (make-parameter fprops))
    (set! *queried-properties* (make-parameter qprops))
    (set! *transient-functional-properties* (make-parameter transient-fprops))
    (set! *transient-queried-properties* (make-parameter transient-qprops))
    (set! *unique-variables* (make-parameter unique-vars))
    (set! *query-functional-properties?* (make-parameter query-fprops?))
    ;; (set! apply-constraints-with-form-cache* (rememoize apply-constraints-with-form-cache))
    ;; (set! *query-forms* (make-hash-table))
    `((success .  "true"))))

(define (format-queried-annotations queried-annotations)
  (list->vector
   (map (lambda (annotation)
          (match annotation
            ((key val)
             `((key . ,(symbol->string key))
               (var . ,(write-uri val))))
            (key `((key . ,(symbol->string key))))))
        queried-annotations)))

(define (format-annotations annotations)
  (list->vector
   (map (lambda (annotation)
          (match annotation
            ((`*values* rest) `((key . ,(format "~A" rest)))) ; fudging
            ((key var) `((key . ,(symbol->string key))
                         (var . ,(symbol->string var))))
            (key `((key . ,(symbol->string key))))))
        annotations)))

(define-rest-call 'POST '("sandbox") sandbox-call)

(define-rest-call 'POST '("apply") apply-call)

(define-rest-call 'GET '("auth")
  (lambda (_)
    (let* ((session (header 'mu-session-id))
           (results (sparql-select-unique 
                     "SELECT ?user WHERE { <~A> <http://mu.semte.ch/vocabularies/authorization/account> ?user }" 
                     session))
           (user (and results (alist-ref 'user results))))
      (if user
          `((user . ,(write-uri user)))
          `((user . #f))))))

(define-rest-call 'POST '("auth")
  (lambda (_)
    (let* ((body (read-request-body))
           ($$body (let ((parsed-body (form-urldecode body)))
                     (lambda (key)
                       (and parsed-body (alist-ref key parsed-body)))))
           (user ($$body 'user))
           (session-id (header 'mu-session-id)))
      (sparql-update "DELETE WHERE { GRAPH <http://mu.semte.ch/authorization> { ?s ?p ?o } }")
      (sparql-update "INSERT DATA { GRAPH <http://mu.semte.ch/authorization> { <~A> <http://mu.semte.ch/vocabularies/authorization/account> <~A> } }" 
                     session-id user))))

(define-rest-call 'POST '("proxy")
  (lambda (_)
    (let ((query (read-request-body)))
      (log-message "~%==Proxying Query==~%~A~%" (add-prefixes query))
      (proxy-query "proxy"
                   (add-prefixes query)
		   (if (update-query? (parse "test" query))
		       (*sparql-update-endpoint*)
		       (*sparql-endpoint*))))))

(define-rest-call 'DELETE '("clear")
  (lambda  (_)
      (let ((graphs 
             (delete-duplicates
              (append 
               (get-all-graphs ((*read-constraint*)))
               (get-all-graphs ((*write-constraint*)))))))
        (sparql-update 
         (write-sparql
          `(@UpdateUnit
            (@Update
             (@Prologue ,@(constraint-prologues))
             (DELETE
              (GRAPH ?g (?s ?p ?o)))
             (WHERE
              (GRAPH ?g (?s ?p ?o))
              (VALUES ?g ,@graphs)))))))
          
       ;; (conc "DELETE { "
       ;;       " GRAPH ?g { ?s ?p ?o } "
       ;;       "} "
       ;;       "WHERE { "
       ;;       " GRAPH ?g { ?s ?p ?o } "
       ;;       " VALUES ?g { "
       ;;       (string-join (map (cut format "~%  ~A" <>) graphs))
       ;;       " } "
       ;;       "}"))
      `((success .  "true"))))

(define (load-plugin name)
  (load (make-pathname (*plugin-dir*) name ".scm")))

(define (save-plugin name read-constraint-string write-constraint-string 
                     fprops transient-fprops qprops transient-qprops unique-vars query-fprops?)
  (let* ((replace (lambda (str) 
                   (irregex-replace/all "^[ \n]+" (irregex-replace/all "[\"]" str "\\\"") "")))
         (read-constraint-string (replace read-constraint-string))
         (write-constraint-string (and write-constraint-string (replace write-constraint-string)))
         (fprops (or fprops (*functional-properties*)))
         (transient-fprops (or transient-fprops (*transient-functional-properties*)))
         (unique-vars  (map symbol->string (or unique-vars (*unique-variables*)))))

    (with-output-to-file (make-pathname (*plugin-dir*) name "scm")
      (lambda ()
        (format #t "(*functional-properties* '~A)~%~%" fprops)
        (format #t "(*transient-functional-properties* '~A)~%~%" transient-fprops)
        (format #t "(*query-functional-properties?* ~A)~%~%" query-fprops?)
        (format #t "(*queried-properties* '~A)~%~%" qprops)
        (format #t "(*transient-queried-properties* '~A)~%~%" transient-qprops)
        (format #t "(*unique-variables* '~A)~%~%" unique-vars)
        (format #t "(*headers-replacements* '((\"<SESSION>\" mu-session-id uri)))~%~%")

        (format #t (conc "(define-constraint  ~%"
                         (if write-constraint-string
                             "  'read \" ~%"
                             "  'read/write \" ~%")
                         read-constraint-string
                         "\")~%~%"))
        
        (when write-constraint-string
              (format #t (conc "(define-constraint  ~%"
                               "  'write \" ~%"
                               write-constraint-string
                               "  \")~%~%")))
        ))))

(define-rest-call 'POST '("plugin" name)
  (rest-call (name)
    (let* ((body (read-request-body))
           ($$body (let ((parsed-body (form-urldecode body)))
                     (lambda (key)
                       (and parsed-body (alist-ref key parsed-body)))))

           (session-id (conc "\"" ($$body 'session-id) "\""))
           (read-constraint-string ($$body 'readconstraint))
           (write-constraint-string ($$body 'writeconstraint))
           (fprops (map string->symbol
                        (string-split (or ($$body 'fprops) "") ", ")))
           (qprops (map string->symbol
                        (string-split (or ($$body 'qprops) "") ", ")))
           (transient-fprops (map string->symbol
                        (string-split (or ($$body 'trfprops) "") ", ")))
           (transient-qprops (map string->symbol
                        (string-split (or ($$body 'trqprops) "") ", ")))
           (query-fprops? (equal? "true" ($$body 'query-fprops)))
           (unique-vars (map string->symbol
                             (string-split (or ($$body 'uvs) "") ", "))))
      (save-plugin name read-constraint-string write-constraint-string 
                   fprops transient-fprops qprops transient-qprops unique-vars query-fprops?)
      `((success . "true")))))

(define-rest-call 'GET '("plugin")
  (lambda (_)
      `((plugins . ,(list->vector
                     (sort
                      (map pathname-file (glob (make-pathname (*plugin-dir*) "[a-zA-Z0-9]*.scm")))
                      string<=))))))

(define-rest-call 'GET '("plugin" name)
  (rest-call (name)
    (load-plugin name)
    (parameterize ((*write-annotations?* #t)
                   (*headers-replacements* '()))
      `((readConstraint . ,(write-sparql (call-if (*read-constraint*))))
        (writeConstraint . ,(write-sparql (call-if (*write-constraint*))))
        (functionalProperties . ,(list->vector (map ->string (*functional-properties*))))
        (queriedProperties . ,(list->vector (map ->string (*queried-properties*))))
        (transientFunctionalProperties . ,(list->vector (map ->string (*transient-functional-properties*))))
        (transientQueriedProperties . ,(list->vector (map ->string (*transient-queried-properties*))))
        (queryFunctionalProperties . ,(*query-functional-properties?*))
        (uniqueVariables . ,(list->vector (map ->string (*unique-variables*))))))))

(define-rest-call 'GET '("users")
  (lambda (_)
    (list->vector
     (map (lambda (user)
            `((uri . ,(write-uri (alist-ref 'user user)))
              (role . ,(write-uri (alist-ref 'role user)))
              (roleTitle . ,(alist-ref 'roleTitle user))
              (name . ,(rdf->json (alist-ref 'name user)))))
          (sparql-select
           (conc
            "PREFIX muauth: <http://mu.semte.ch/vocabularies/authorization/>"
            "PREFIX dct: <http://purl.org/dc/terms/>"
            "SELECT DISTINCT ?user ?role ?roleTitle ?name "
            "WHERE { "
            " ?user muauth:hasRole ?role "
            " OPTIONAL { ?role dct:title ?roleTitle }"
            " OPTIONAL { "
            "  { ?user dct:title ?name } "
            "  UNION { ?user foaf:name ?name } "
            " } "
           "}" ))))))

;; (define (serve-file path)
;;   (log-message "~%Serving ~A~%" path)
;;   (call-with-input-file path
;;     (lambda (port)
;;       (read-string #f port))))

;; (define (sandbox filename)
;;   (let ((filename (if (equal? filename "") "index.html" filename)))
;;     (if (feature? 'docker) 
;;         (make-pathname "/app/sandbox/" filename)
;;         (make-pathname "./sandbox/" filename))))

;; (define-rest-call 'GET '("sandbox") (lambda (_) (serve-file (sandbox "index.html"))))

;; ;; a way to do this directly in mu-chicken-support?
;; (define-rest-call 'GET '("sandbox" file)
;;   (rest-call
;;    (file)
;;    (serve-file (sandbox file))))

;; (define-rest-call 'GET '("sandbox" dir file)
;;   (rest-call
;;    (dir file)
;;    (serve-file (sandbox (string-join (list dir file) "/")))))

;; (define-rest-call 'GET '("sandbox" dir dir2 file)
;;   (rest-call
;;    (dir dir2 file)
;;    (serve-file (sandbox (string-join (list dir dir2 file) "/")))))
