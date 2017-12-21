(use srfi-69 srfi-18 abnf lexgen)
(require-extension mailbox)

(define (debug-message str #!rest args)
  (when (*debug-logging?*)
    (apply format (current-error-port) str args)))

(define *query-forms* (make-hash-table))

(define *cache-mailbox* (make-mailbox))

(define-syntax timed-let
  (syntax-rules ()
    ((_ label (let-exp (vars/vals ...) body ...))
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let-exp (vars/vals ...)
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (debug-message "~%[~A] ~A Time: ~Ams / ~Ams / ~Ams~%" (logkey) label (- ut2 ut1) (- st2 st1) (- t2 t1))
               body ...))))))))

(define-syntax timed
  (syntax-rules ()
    ((_ label body ...)
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let ((result body ...))
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (debug-message "~%[~A] ~A Time: ~Ams / ~Ams / ~Ams~%" (logkey) label (- ut2 ut1) (- st2 st1) (- t2 t1))
               result))))))))

(define-syntax timed-limit
  (syntax-rules ()
    ((_ limit label expression body ...)
     (let-values (((ut1 st1) (cpu-time)))
       (let ((t1 (current-milliseconds)))
         (let ((result body ...))
           (let-values (((ut2 st2) (cpu-time)))
             (let ((t2 (current-milliseconds)))
               (when (> (- ut2 ut1) limit)
                     (debug-message "~%[~A] Exceeded time limit for ~A: ~Ams / ~Ams / ~Ams~%~A~%~%"
                                    (logkey) label (- ut2 ut1) (- st2 st1) (- t2 t1)
                                    expression))
               result))))))))

(define-syntax try-safely
  (syntax-rules ()
    ((_ label exp body ...)
     (handle-exceptions exn 
                        (begin (log-message "~%[~A]  ==Error ~A==~%~A~%~%" 
                                         (logkey) label exp)
                               #f)
       body ...))))

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

(define (query-cache-key query)
  (list (get-query-prefix query)
        ;;  (get-binding 'queried-functional-properties bindings)) ...
        (make-query-pattern query)  
        (call-if (*read-constraint*))
        (call-if (*write-constraint*))))

(define (populate-cached-form pattern form match query-string)
  (let ((matches (map (lambda (name-pair)
                        (let ((name (car name-pair))
                              (index (cdr name-pair)))
                                 (cons (conc "<" (symbol->string name) ">")
                                       (irregex-match-substring match index))))
                      (irregex-match-names match))))
    (string-translate* form matches)))

(define (query-form-lookup query-string)
  (if (*cache-forms?*)
      (let ((cached-forms (hash-table-ref/default *query-forms* (query-cache-key query-string) #f)))
        (if cached-forms
            (let ((pattern-regex (first cached-forms)))
              (values (irregex-match pattern-regex (get-query-body query-string))
                      cached-forms))
            (values #f #f)))
      (values #f #f)))

(define (populate-cached-forms query-string form-match cached-forms)
  (match cached-forms
    ((pattern form form-prefix annotations annotations-forms annotations-prefixes annotations-pairs
              deltas-form deltas-prefix bindings update? cached-logkey)
     (log-message "~%[~A] Using cached form of ~A~%" (logkey) cached-logkey)
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
             (and deltas-form 
                  (replace-headers
                   (conc deltas-prefix
                         (populate-cached-form pattern deltas-form form-match query-string))))
             bindings
             update?))))

(define cache-save-daemon
  (make-thread
    (lambda ()
      (let loop ()
        (let ((thunk (mailbox-receive! *cache-mailbox*)))
          (handle-exceptions exn (begin
                                   (log-message "[~A] Error saving cache forms~%" (logkey))
                                   (print-exception exn))
            (thunk)))
        (loop)))))

(thread-start! cache-save-daemon)

(define (enqueue-cache-action! thunk)
  (mailbox-send! *cache-mailbox* thunk))

(define (make-cached-forms query-string rewritten-query
                           annotations annotations-query-strings annotations-pairs
                           deltas-query-string bindings update? key)
  (let-values (((pattern form-bindings) (make-query-pattern query-string)))
    (let* ((make-form (lambda (q) (and q (make-query-form q form-bindings)))))
      (list (irregex pattern)
            (make-form rewritten-query)
            (get-query-prefix rewritten-query)
            annotations
            (and annotations-query-strings (map make-form annotations-query-strings))
            (and annotations-query-strings (map get-query-prefix annotations-query-strings))
            annotations-pairs
            (and deltas-query-string (make-form deltas-query-string))
            (and deltas-query-string (get-query-prefix deltas-query-string))
            bindings update? key))))

(define (query-form-save! query-string rewritten-query annotations annotations-query-strings annotations-pairs
                          deltas-query-string bindings update? key)
      (hash-table-set! *query-forms*
                       (query-cache-key query-string)
                       ;; (filter (lambda (pair) (not (sparql-variable? (cdr pair))))
                       ;;         (get-binding 'functional-properties bindings)))
                       (make-cached-forms query-string rewritten-query 
                                          annotations annotations-query-strings annotations-pairs
                                          deltas-query-string bindings update? key)))

(define (enqueue-save-cache-form  query-string rewritten-query-string
                                  annotations annotations-query-strings annotations-pairs
                                  deltas-query-string bindings update?)
  (let ((key (logkey)) (rc (*read-constraint*)) (wc (*write-constraint*)))
    (debug-message "[~A] Saving cache form...~%" (logkey))
    (enqueue-cache-action!
     (lambda ()
       (parameterize ((logkey key)
                      (*read-constraint* rc)
                      (*write-constraint* wc))
                     (let-values (((form-match _) (query-form-lookup query-string)))
                       (if form-match
                           (begin (debug-message "[~A] Already cached~%" key)
                                  #f)
                           (timed "Generate Cache Form"
                                  (query-form-save! query-string 
                                                    rewritten-query-string
                                                    annotations
                                                    annotations-query-strings annotations-pairs
                                                    deltas-query-string
                                                    bindings update? key)))))))))

(define (apply-constraints-with-form-cache query-string)
 (apply-constraints-with-form-cache* query-string
                                     (call-if (*read-constraint*))
                                     (call-if (*write-constraint*))))

(define (apply-constraints-with-form-cache* query-string
                                           read-constraint
                                           write-constraint)
  (timed-let "Lookup"
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
                    (let* (;;(queried-annotations (and aquery (try-safely "Getting Queried Annotations" aquery 
                           (rewritten-query-string (write-sparql rewritten-query))
                           (annotations-query-strings (and aqueries (map write-sparql aqueries)))
                           (deltas-query (and (*send-deltas?*) (notify-deltas-query rewritten-query)))
                           (deltas-query-string (and deltas-query (write-sparql deltas-query))))

                      (log-message "~%[~A]  ==Rewritten Query==~%~A~%" (logkey) rewritten-query-string)

                      (when (*cache-forms?*)
                            (enqueue-save-cache-form query-string rewritten-query-string
                                                     annotations annotations-query-strings annotations-pairs
                                                     deltas-query-string bindings update?))

                      (values (replace-headers rewritten-query-string)
                              annotations
                              (and annotations-query-strings (map replace-headers annotations-query-strings))
                              annotations-pairs
                              (and deltas-query-string (replace-headers deltas-query-string))
                              bindings
                              update?
                              )))))))))))

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

(define apply-constraints-with-form-cache* (memoize-save apply-constraints-with-form-cache*))

