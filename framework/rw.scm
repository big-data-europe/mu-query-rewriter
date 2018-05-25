;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For RW library

;; (define rw/value
;;   (lambda (block bindings)
;;     (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
;;       (values rw new-bindings))))

(define (sparql-variable-name var)
  (string->symbol (substring (symbol->string var) 1)))

(define (rewrite-union-fold block bindings proc bindings-fold)
  (let loop ((block block)
             (rw '())
             (new-bindings '()))
    (if (null? block) (values rw new-bindings)
        (let-values (((r b) (proc (car block) bindings)))
          (let ((r (if (null? r) '(()) r)))
            (loop (cdr block)
                  (append rw r)
                  (bindings-fold b new-bindings)))))))

(define (extract-subselect-vars vars)
  (filter values
          (map (lambda (var)
                 (if (symbol? var) var
                     (match var
                       ((`AS _ v) v)
                       (else #f))))
               vars)))

(define (subselect-bindings vars bindings)
  (let* ((subselect-vars (extract-subselect-vars vars))
         (inner-bindings (if (or (equal? subselect-vars '(*))
                                 (equal? subselect-vars '*))
                             bindings)))
    (project-bindings subselect-vars bindings)))

(define (merge-subselect-bindings vars new-bindings bindings)
  (merge-bindings
   (project-bindings (extract-subselect-vars vars) new-bindings)
   bindings))

(define (rw/subselect block bindings)
  (match block
    ((`@SubSelect (label . vars) . rest)
     (let* ((subselect-vars (extract-subselect-vars vars))
            (inner-bindings (if (or (equal? subselect-vars '(*))
                                    (equal? subselect-vars '*))
                                bindings
                                (project-bindings subselect-vars bindings)))
            (vars (if (equal? vars '(*))
                      (get-vars rest)
                      vars)))
       (let-values (((rw new-bindings) (rewrite rest inner-bindings)))
         (values `((@SubSelect (,label ,@vars) ,@rw))
                 (merge-bindings 
                  (project-bindings subselect-vars new-bindings)
                  bindings)))))))

(define-syntax fail-or-null
  (syntax-rules ()
    ((_ rw new-bindings)
     (fail-or-null rw new-bindings rw new-bindings))
    ((_ rw new-bindings body)
     (fail-or-null rw new-bindings body new-bindings))
    ((_ rw new-bindings body bindings-exp)
     (cond ((nulll? rw) (values '() new-bindings))
           ((fail? rw) (values '(#f) new-bindings))
           (else (values body bindings-exp))))))

(define-syntax fail-or-null/low 
  (syntax-rules ()
    ((_ rw new-bindings body ...)
     (cond ((nulll? rw) (values '() new-bindings))
           ((fail? rw) (values '(#f) new-bindings))
           (else body ...)))))

(define (rw/list block bindings)
  (let-values (((rw new-bindings) (rewrite block bindings)))
    (fail-or-null rw new-bindings
      (list (filter pair? rw)))))

(define (rw/quads block bindings)
  (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
    (fail-or-null rw new-bindings
      `((,(car block) ,@(filter pair? rw))))))

(define (rw/union block bindings)
  (let-values (((rw new-bindings) (rewrite (cdr block) bindings)))
    (let ((new-blocks (filter values rw)))
      (case (length new-blocks)
	((0)  (values (list #f) new-bindings))
        ((1)  (values (car new-blocks) new-bindings))
	(else (values `((UNION ,@new-blocks)) new-bindings))))))

(define (atom-or-cdr a)
  (if (pair? a) (cdr a) a))

(define (not-node? head)
  (lambda (exp)
    (not (equal? (car exp) head))))

(define (get-child label block)
  (assoc label block))

(define (get-child-body label block)
  (alist-ref label block))

(define (replace-child-body label replacement block)
  (alist-update label replacement block))

(define (replace-child-body-if label replacement block)
  (if replacement
      (alist-update label replacement block)
      block))

(define (append-child-body/before label new-statements block)
  (replace-child-body label (append new-statements (or (get-child-body label block) '())) block))

(define (append-child-body/after label new-statements block)
  (replace-child-body label (append (or (get-child-body label block) '()) new-statements) block))

(define (insert-child-before head statement statements)
  (cond ((nulll? statements) '())
        ((equal? (caar statements) head)
         (cons statement statements))
        (else (cons (car statements)
                    (insert-child-before head statement (cdr statements))))))

(define (insert-child-after head statement statements)
  (cond ((nulll? statements) '())
        ((equal? (caar statements) head)
         (cons (car statements)
	       (cons statement (cdr statements))))
        (else (cons (car statements)
                    (insert-child-after head statement (cdr statements))))))

(define delete? (make-parameter #f))

(define insert? (make-parameter #f))

(define update? (make-parameter #f))

(define where? (make-parameter #f))

(define (insert-query? query)
  (or (get-child-body '|INSERT DATA| (cdr query))
      (get-child-body 'INSERT (cdr query))))

(define (update-query? query)
  (equal? (car query) '@UpdateUnit))

(define (select-query? query)
  (equal? (car query) '@QueryUnit))

(define (update-unit? unit)
  (alist-ref '@Update unit))

(define (fail? block)
  (not (null? (filter not block))))

(define (a->rdf:type b)
  (if (equal? b 'a) 'rdf:type b))

(define *constraint-prologues* (make-parameter '()))

(define (rdf-equal? a b)
  (if (and (symbol? a) (symbol? b))
      (equal? (expand-namespace (a->rdf:type a) (*namespaces*))
              (expand-namespace (a->rdf:type b) (*namespaces*)))
      (equal? a b)))

(define (rdf-member a bs)
  (let ((r (filter (cut rdf-equal? a <>) bs)))
    (and (not (null? r)) r)))

(define (literal-triple-equal? a b)
  (null? (filter not (map rdf-equal? a b))))

(define replace-a
  (match-lambda ((s p o) `(,s ,(a->rdf:type p) ,o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
(define (alist-update-proc key proc alist)
  (let loop ((alist alist) (cont values))
    (cond ((null? alist) (cont (list (cons key (proc #f)))))
          ((equal? (caar alist) key) 
           (cont `((,key . ,(proc (cdar alist))) 
                   ,@(cdr alist))))
          (else (loop (cdr alist) 
                      (lambda (rest)
                        (cont `(,(car alist) ,@rest))))))))

(define (assoc-update-proc key proc alist)
  (cond ((null? alist) (list (cons key (proc #f))))
        ((equal? key (caar alist))
         (cons (cons key (proc (cdar alist)))
               (cdr alist)))
        (else (cons (car alist)
                    (assoc-update-proc key proc (cdr alist))))))

(define append-unique (compose delete-duplicates append))

;; merges alists whose values must be lists
;; e.g., '((a . (1 2 3))) '((a . (4)))
(define (merge-alists #!rest alists)
  (let loop ((alists alists) (accum '()))
    (if (null? alists) accum
        (let inner ((alist (car alists)) (accum accum))
          (if (null? alist) (loop (cdr alists) accum)
              (let ((kv (car alist)))
                (inner (cdr alist)
                       (alist-update-proc (car kv) 
                                          (lambda (current)
                                            (append-unique (or current '()) (cdr kv)))
                                          accum))))))))

(define (key< . args)
  (apply string<? (map (compose symbol->string car) args)))

(define (intersect-sorted-alists a b)
  (let loop ((a a) (b b) (accum '()))
    (cond ((or (null? a) (null? b)) (reverse accum))
          ((key< (car b) (car a)) (loop a (cdr b) accum))
          ((key< (car a) (car b)) (loop (cdr a) b accum))
          (else (loop (cdr a) (cdr b) (cons `(,(caar a) ,@(lset-intersection equal? (cdar a) (cdar b)))
                                            accum))))))
    
;; intersection of alists, whose values must all be lists
;; '((a 4) (b 5)) '((a 3 4 5)) => '((a 4))
;; probably very inneficient, but here the lists will never be very long
(define (intersect-alists . alists)
  (if (null? alists) '()
      (fold intersect-sorted-alists
            (sort (car alists) key<)
            (map (cute sort <> key<) (cdr alists)))))
 
(define (error-condition message block)
  (abort
   (make-property-condition
    'exn
    'message (format (conc message ":~%~A") (write-sparql block)))))

(define (print-exception exn)
  (log-message "\t[~A]  Error: (~A) ~A~%~%\tArguments: ~A~%~%\tCall History:~%~%~A\t<---~%"
               (logkey)
               ((condition-property-accessor 'exn 'location) exn)
               ((condition-property-accessor 'exn 'message) exn)

               ((condition-property-accessor 'exn 'arguments) exn)
               (string-join
                (map (lambda (link)
                       (let ((a (first link))
                             (b (format "~A" (second link))))
                         (format "\t~A    ~A" 
                                 a (substring b 0 (min 80 (string-length b))))))
                     (map vector->list
                          ((condition-property-accessor 'exn 'call-chain) exn)))
                "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Axes
;;
;; (define (in-where?)
;;   ((parent-axis
;;     (lambda (context) 
;;       (let ((head (context-head context)))
;;         (and head (equal? (car head) 'WHERE)))))
;;    (*context*)))
;;
;; (define (get-context-graph)
;;   (let ((ancestor ((parent-axis (lambda (context) (equal? (car (context-head context)) 'GRAPH)))
;;                  (*context*))))
;;     (and ancestor
;;          (match (context-head ancestor)
;;            ((`GRAPH graph . rest) graph)
;;            (else #f)))))
;;
;; (define (update?)
;;   ((parent-axis
;;     (lambda (context) 
;;       (let ((head (context-head context)))
;;         (and head (equal? (car head) '@Update)))))
;;    (*context*)))
;;
;; (define (all-select-variables)
;;   (get-vars
;;    (cdr
;;     (context-head
;;      ((next-sibling-axis
;;        (lambda (context) 
;;          (let ((head (context-head context)))
;;            (and head (equal? (car head) 'WHERE)))))
;;       (*context*))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Namespaces
(define all-namespaces (make-parameter '()))

(define (get-constraint-prefixes read write)
  (append
   (query-prefixes (call-if read))
   (query-prefixes (call-if write))))

(define (constraint-prefixes)
  (get-constraint-prefixes (*read-constraint*) (*write-constraint*)))

(define (constraint-and-query-prefixes query)
  (delete-duplicates
   (append (constraint-prefixes)
           (query-prefixes query)
           (*namespaces*))))

(define (get-constraint-prologues* read write)
  (append-unique
   (all-prologues (cdr (call-if read)))
   (all-prologues (cdr (call-if write)))))

(define get-constraint-prologues (memoize get-constraint-prologues*))

(define (constraint-prologues)
  (get-constraint-prologues (*read-constraint*)  (*write-constraint*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Up-Down Store
(define store (make-parameter '()))

(define (store? exp)
  (and (pair? exp)
       (equal? (car exp) '*store*)))

(define quads? (compose not store?))

(define (get-store key) (alist-ref key (store)))

(define (get-store/default key default) (or (alist-ref key (store)) default))

(define (get-returned-store key block) (alist-ref key (or (alist-ref '*store* block) '())))

(define (get-returned-store/default key block default)
  (or (alist-ref key (or (alist-ref '*store* block) '()))
      default))

(define (append-stores rw #!optional (proc values))
  (let-values (((stores quads) (partition store? rw)))
    (let ((merged-stores (apply merge-alists (append (map cdr stores) (list (store))))))
      (cond ((nulll? quads) '())
            ((null? merged-stores) (list (proc quads)))
            (else (cons `(*store* . ,merged-stores)
                        (proc quads)))))))
  
(define (cdr/safe elt) (if (null? elt) '() (cdr elt)))

(define (car/safe elt) (if (null? elt) '() (car elt)))

(define (intersect-stores rw proc)
  ;; (let ((stores (map cdr (filter pair? (map car (filter pair? (map (cut filter store? <>) rw))))))
  (let ((stores (map cdr/safe (map car/safe (map (cut filter store? <>) rw))))
        (new-blocks (filter (compose not fail?)  (map (cut filter quads? <>) rw))))
    (if (= (length new-blocks) 0)
        (list #f)
        (cons `(*store* . ,(apply intersect-alists stores))
              (proc new-blocks)))))

(define (update-store key val rw)
  (alist-update '*store* 
                (alist-update key val  (or (alist-ref '*store* rw) '()))
                rw))

(define (nulll? block)
  (null? (remove store? (remove annotation? block))))

