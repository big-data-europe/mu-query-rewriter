(use files matchable intarweb spiffy spiffy-request-vars
     uri-common intarweb medea irregex srfi-13 http-client cjson
     memoize)

(import s-sparql mu-chicken-support)
(use s-sparql mu-chicken-support)

(require-extension sort-combinators)

;; (define-syntax include-file
;;   (syntax-rules ()
;;     ((_ path) (include path))))



(include "framework/settings.scm")

(include "framework/rw.scm") ; factor up to s-sparql

 (include "framework/sparql-logic.scm")

 (include "framework/annotations.scm")  

(include "framework/utility-transformations.scm")

(include "framework/temporary-graph.scm")

(include "framework/optimization.scm")

(include "framework/main-transformation.scm")

(include "framework/constraint-renaming.scm")

(include "framework/constraint-renaming-dependencies.scm")

(include "framework/constraint.scm")

(include "framework/instantiation.scm")

(include "framework/deltas.scm")

(include "framework/caching.scm")

(include "framework/call-specification.scm")

(include "framework/sandbox.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load
(log-message "~%==Query Rewriter Service==")

(when (*plugin*)
      (load (make-pathname (*plugin-dir*) (*plugin*) ".scm")))

(log-message "~%Proxying to SPARQL endpoint: ~A " (*sparql-endpoint*))
(log-message "~%and SPARQL update endpoint: ~A " (*sparql-update-endpoint*))

(*port* 8890)

(http-line-limit #f)
