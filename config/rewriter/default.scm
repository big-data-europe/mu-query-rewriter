(*functional-properties* '())

(define-constraint
  'read/write
  (conc "CONSTRUCT {"
            "  ?s ?p ?o."
            "}"
            "WHERE {"
            "  GRAPH <http://mu.semte.ch/application> {"
            "    ?s ?p ?o."
            "  }"
            "}"))
