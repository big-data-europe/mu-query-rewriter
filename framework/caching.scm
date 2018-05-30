(use srfi-69 srfi-18 abnf lexgen)
(require-extension mailbox)

(define (debug-message str #!rest args)
  (when (*debug-logging?*)
    (apply format (current-error-port) str args)))

(define (error-message str #!rest args)
  (apply format (current-error-port) str args))

(define *query-forms* (make-hash-table)) 

(define *cache-mailbox* (make-mailbox))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick Splitting of Prologue/Body by Parsing
(define ws*
  (repetition
   (alternatives char-list/wsp char-list/crlf char-list/lf char-list/cr)))

(define PrefixDecl*
  (concatenation
   ws* (char-list/:s "PREFIX") ws* PNAME_NS ws* IRIREF ws*))

(define BaseDecl*
  (concatenation
   ws* (char-list/:s "BASE") ws* IRIREF ws*))

(define Prologue*
  (repetition
   (alternatives BaseDecl* PrefixDecl*)))

(define split-query-prefix
  (memoize
   (lambda (q)
     (match-let (((prefix body) 
                  (map list->string
                       (lex Prologue* error q))))
       (values prefix body)))))

(define get-query-prefix 
  (memoize
   (lambda (q)
     (let-values (((prefix body) (split-query-prefix q)))
       prefix))))

(define get-query-body 
  (memoize
   (lambda (q)
     (let-values (((prefix body) (split-query-prefix q)))
       body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REST
(define (regex-escape-string str)
  (string-translate* str '(("\\" . "\\\\")
                           ("$" . "\\$")
                           ("." . "\\.")
                           ("|" . "\\|")
                           ("+" . "\\|")
                           ("(" . "\\(")
                           (")" . "\\)")
                           ("[" . "\\[")
                           ("{" . "\\{")
                           ("*" . "\\*")
                           ("?" . "\\?")
                           ("^" . "\\^")
                           )))

(define uri-pat "<[^> ]+>")
(define uri-regex (irregex uri-pat))
(define str-pat "\\\"[^\"]+\\\"")
(define str-regex (irregex str-pat))
(define form-regex (irregex (format "(~A)|(~A)" uri-pat str-pat)))

(define (match-to-replacement-keys pattern query from-index match substr key* key)
  (conc pattern
        (substring query from-index (irregex-match-start-index match))
        (if (string=? key substr) substr
            (conc "<" key ">"))))

(define (match-to-regex pattern query from-index match substr key* key)
  (conc pattern
        (regex-escape-string (substring query from-index (irregex-match-start-index match)))
        (if key*
            (conc "\\k<" key ">")
            (conc "(?<" key ">"
                  (let ((c (substring substr 0 1)))
                    (cond ((equal? c "<") uri-pat)
                          ((equal? c "\"") str-pat)))
                  ")"))))                 

(define (split-matches query-body form? matches)
  (irregex-fold form-regex
                (lambda (from-index form-match seed)
                  (match seed
                    ((pattern matches end-index n)
                     (let* ((substr (irregex-match-substring form-match))
                            (key* (cdr-when (assoc substr matches)))
                            (key (or key*
                                     (if form? substr
                                         (conc "uri" (number->string n))))))
                       (list ((if form?
                                  match-to-replacement-keys
                                  match-to-regex)
                              pattern query-body from-index form-match substr key* key)
                             (if (or key* form?)
                                 matches 
                                 (alist-update substr key matches))
                             (irregex-match-end-index form-match)
                             (+ n 1))))))
                `("" ,matches #f 0) 
                query-body))

(define (make-query-pattern/form query #!optional form? (matches '()))
  (let ((query-body (get-query-body query)))
    (match (split-matches query-body form? matches)
     ((pattern matches end-index _)
      (if end-index
          (values (conc pattern
                        (if form?
                            (substring query-body end-index)
                            (regex-escape-string (substring query-body end-index))))
                  matches)
          (values (if form? query-body (regex-escape-string query-body))
                  '()))))))

(define (make-query-form query matches)
  (make-query-pattern/form query #t matches))

(define make-query-pattern
  (memoize 
   (lambda (query)
     (make-query-pattern/form query))))

(define constraint-key (make-parameter 'five))

(define (query-cache-key query)
  (let-values (((pattern matches) (make-query-pattern query)))
    (let ((ck (constraint-key)))
      (values
       (list (get-query-prefix query)
             pattern
             (car ck)
             (filter values
                     (map (match-lambda ((uri . key)
                                         (let ((sub-var (alist-ref (string->symbol uri) (second ck))))
                                           (and sub-var (list sub-var key)))))
                          matches)))
       matches))))

(define (populate-cached-form pattern form match query-string)
  (let ((matches (map (lambda (name-pair)
                        (let ((name (car name-pair))
                              (index (cdr name-pair)))
                                 (cons (conc "<" (symbol->string name) ">")
                                       (irregex-match-substring match index))))
                      (irregex-match-names match))))
    (string-translate* form matches)))

(define (query-matches-fprop-queries matches bindings)
  (let ((queries (get-binding 'functional-property-queries bindings)))
    (if queries
        (filter values
                (map (match-lambda ((uri . m)
                                    (let ((q (assoc (string->symbol uri) queries)))
                                      (and q (cons (string->symbol m) (cdr q))))))
                     matches))
        '())))

;; (define (query-form-lookup query-string #!optional bindings)
;;   (log-message "~%With bindings: ~A~%" bindings)
;;   (if (*cache-forms?*)
;;       (let-values (((key matches) (query-cache-key query-string)))
;;         (let ((fprop-queries (and bindings (query-matches-fprop-queries matches bindings)))
;;               (cached-forms (hash-table-ref/default *query-forms* key #f)))
;;           (if cached-forms
;;               (let ((cached-form (assoc fprop-queries cached-forms)))
;;                 (if cached-form
;;                     (let ((pattern-regex (first (cdr cached-form))))
;;                       (values (irregex-match pattern-regex (get-query-body query-string))
;;                               (cdr cached-form)))
;;                     (values #f #f)))
;;               (values #f #f))))
;;       (values #f #f)))

(define (query-form-lookup query-string #!optional bindings)
  (if (*cache-forms?*)
      (let-values (((key matches) (query-cache-key query-string)))
        (let ((cached-forms (hash-table-ref/default *query-forms* key #f)))
          (if cached-forms
              (if bindings
                  (let ((fprop-queries (and bindings (query-matches-fprop-queries matches bindings))))
                    (let ((cached-form (assoc fprop-queries cached-forms)))
                      (if cached-form
                          (let ((pattern-regex (first (cdr cached-form))))
                            (values (irregex-match pattern-regex (get-query-body query-string))
                                    (cdr cached-form)))
                          (values #f #f))))
                  (let loop ((cached-forms cached-forms))
                    (if (or (not cached-forms) (null? cached-forms)) (values #f #f)
                        (let* ((cached-form (car cached-forms))
                               (pattern-regex (first (cdr cached-form)))
                               (matches (irregex-match pattern-regex (get-query-body query-string))))
                          (if matches
                              (let ((fprops-match? (every values
                                                          (map (match-lambda ((slot . index)
                                                                              (let* ((uri (irregex-match-substring matches index))
                                                                                     (prop-query (alist-ref slot (car cached-form))))
                                                                              (match prop-query
                                                                                     ((p o) (check-functional-property uri p o))
                                                                                     (#f '())))))
                                                             (irregex-match-names matches)))))
                                (if fprops-match?
                                    (values matches (cdr cached-form))
                                    (loop (cdr cached-forms))))
                              (loop (cdr cached-forms)))))))
              (values #f #f))))
      (values #f #f)))

(define (populate-cached-forms query-string form-match cached-forms)
  (match cached-forms
    ((pattern form form-prefix annotations annotations-forms annotations-prefixes annotations-pairs
              deltas-forms deltas-prefixes bindings update? cached-logkey)
     (log-message "~%Using cached form [~A]: ~A~%" (logkey) cached-logkey)
     (values (replace-headers
              (conc form-prefix
                    (populate-cached-form pattern form form-match query-string)))
             annotations
             (and annotations-forms
                  (map (lambda (annotations-prefix annotations-form)
                         (replace-headers
                          (conc annotations-prefix
                                (populate-cached-form pattern annotations-form form-match query-string))))
                       annotations-prefixes annotations-forms))
             annotations-pairs
             (and deltas-forms 
                  (map (lambda (prefix-pair form-pair)
                         (map (lambda (prefix form)
                                (and form
                                     (conc prefix
                                           (populate-cached-form pattern form form-match query-string))))
                              prefix-pair form-pair))
                       deltas-prefixes deltas-forms))
             bindings
             update?))))

(define cache-save-daemon
  (make-thread
    (lambda ()
      (let loop ()
        (let ((thunk (mailbox-receive! *cache-mailbox*)))
          (handle-exceptions exn (begin
                                   (log-message "Error saving cache forms [~A]~%" (logkey))
                                   (print-exception exn))
            (thunk)))
        (loop)))))

(thread-start! cache-save-daemon)

(define (enqueue-cache-action! thunk)
  (mailbox-send! *cache-mailbox* thunk))

(define (make-cached-forms query-string rewritten-query
                           annotations annotations-query-strings annotations-pairs
                           deltas-query-strings bindings update? key)
  (let-values (((pattern form-bindings) (make-query-pattern query-string)))
    (let* ((make-form (lambda (q) (and q (make-query-form q form-bindings)))))
(log-message "~%Caching deltas: ~%~A~%"  (map (lambda (pair)
                                               (map (lambda (q) (and q (make-form q)))
                                                    pair))
                                             deltas-query-strings))
      (list (irregex pattern)
            (make-form rewritten-query)
            (get-query-prefix rewritten-query)
            annotations
            (and annotations-query-strings (map make-form annotations-query-strings))
            (and annotations-query-strings (map get-query-prefix annotations-query-strings))
            annotations-pairs
            (and deltas-query-strings (map (lambda (pair)
                                               (map (lambda (q) (and q (make-form q)))
                                                    pair))
                                             deltas-query-strings))
            (and deltas-query-strings (map (lambda (pair)
                                            (map (lambda (q) (and q (get-query-prefix q)))
                                                 pair))
                                          deltas-query-strings))
                                          
            bindings update? key))))

(define (query-form-save! query-string rewritten-query annotations annotations-query-strings annotations-pairs
                          deltas-query-strings bindings update? logkey)
  (let-values (((form-key matches) (query-cache-key query-string)))
    (let ((fprop-queries (query-matches-fprop-queries matches bindings))
          (table (hash-table-ref/default *query-forms* form-key '())))
      (hash-table-set! *query-forms*
                       form-key
                       (cons (cons fprop-queries
                                   (make-cached-forms query-string rewritten-query 
                                                      annotations annotations-query-strings annotations-pairs
                                                      deltas-query-strings bindings update? logkey))
                             table)))))

(define (enqueue-save-cache-form  query-string rewritten-query-string
                                  annotations annotations-query-strings annotations-pairs
                                  deltas-query-strings bindings update?)
  (let ((key (logkey)) (*rc* (*read-constraint*)) (*wc* (*write-constraint*))
        (ck (constraint-key)) (ckh (make-cache-key-headers)))
    (debug-message "Saving cache form [~A]...~%" (logkey))
    (enqueue-cache-action!
    (lambda ()
       (parameterize ((logkey key)
                      (*read-constraint* *rc*)
                      (*write-constraint* *wc*)
                      (constraint-key ck)
                      (cache-key-headers ckh))
                     (let-values (((form-match _) (query-form-lookup query-string bindings)))
                       (if form-match
                           (begin (debug-message "Already cached [~A]~%" key)
                                  #f)
                           (timed "Generate Cache Form"
                                  (query-form-save! query-string 
                                                    rewritten-query-string
                                                    annotations
                                                    annotations-query-strings annotations-pairs
                                                    deltas-query-strings
                                                    bindings update? key)))))))))

(define (apply-constraints-with-form-cache query-string)
  (parameterize ((cache-key-headers (make-cache-key-headers)))
                (apply-constraints-with-form-cache* query-string)))

(define (make-constraint-key select-query?)
  (if select-query?
      (let-values (((constraint bindings) (read-constraint)))
        (list constraint
              (map (compose (cut apply cons <>) reverse)
                   (delete-duplicates (get-binding 'functional-property-substitutions bindings)))))
      (let-values (((r-constraint r-bindings) (read-constraint))
                   ((w-constraint w-bindings) (write-constraint)))
        (list (list r-constraint w-constraint) 
              (map (compose (cut apply cons <>) reverse)
                   (delete-duplicates
                    (append (get-binding 'functional-property-substitutions r-bindings)
                            (get-binding 'functional-property-substitutions w-bindings))))))))

(define (apply-constraints-with-form-cache* query-string)
  (timed-let "Lookup"
   (parameterize ((constraint-key (make-constraint-key (irregex-search (irregex "select" 'i) (get-query-body query-string)))))
    (let-values (((form-match cached-forms) (query-form-lookup query-string)))
     (if form-match
         (populate-cached-forms query-string form-match cached-forms)
         (let ((query (parse-query query-string)))
           (timed-let "Rewrite" 
              (let-values (((rewritten-query bindings) (apply-constraints query)))
                (let* ((update? (update-query? query))
                       (annotations (and (*calculate-annotations?*) 
                                         (handle-exceptions exn #f
                                                            (get-annotations rewritten-query bindings)))))
                  (let-values (((aqueries annotations-pairs) (if annotations
                                                               (annotations-queries annotations rewritten-query)
                                                               (values #f #f))))
                    (let* ((rewritten-query-string (write-sparql rewritten-query))
                           (annotations-query-strings (and aqueries (map write-sparql aqueries)))
                           (deltas-query-strings (and update? (*send-deltas?*) (make-deltas-queries rewritten-query))))

                      (log-message "~%==Rewritten Query== [~A]~%~A~%" (logkey) rewritten-query-string)

                      (when (*cache-forms?*)
                            (enqueue-save-cache-form query-string rewritten-query-string
                                                     annotations annotations-query-strings annotations-pairs
                                                     deltas-query-strings bindings update?))

                      (values rewritten-query-string
                              annotations
                              (and annotations-query-strings annotations-query-strings)
                              annotations-pairs
                              deltas-query-strings
                              bindings
                              update?
                              )))))))))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; memoization
;; (define keys (memoize keys*))

;;(define renaming (memoize renaming*))

(define-syntax memoize-save
  (syntax-rules ()
    ((_ proc)
     (begin
       (put! (quote proc) 'memoized proc)
       (memoize proc)))))

(define-syntax rememoize
  (syntax-rules ()
    ((_ proc)
     (memoize (get (quote proc) 'memoized)))))

(define-syntax unmemoize
  (syntax-rules ()
    ((_ proc)
     (or (get (quote proc) 'memoized)
         proc))))

(define unique-variable-substitutions (memoize-save unique-variable-substitutions))

(define parse-query (memoize-save parse-query))

(define rdf-equal? (memoize-save rdf-equal?)) ; needs to take namespaces as param as well

(define get-constraint-prefixes (memoize-save get-constraint-prefixes))

(define parse-constraint (memoize-save parse-constraint))

(define replace-headers (memoize-save replace-headers))

(define get-dependencies (memoize-save get-dependencies))

;; This breaks 
;;(define apply-constraints-with-form-cache* (memoize-save apply-constraints-with-form-cache*))

