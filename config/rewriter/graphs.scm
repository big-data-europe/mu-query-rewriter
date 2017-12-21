(*functional-properties* '(rdf:type))

(*query-functional-properties?* #t)

(define-constraint
  'read/write
  (conc "CONSTRUCT {"
            "  ?s ?p ?o."
            "}"
            "WHERE {"
            "  GRAPH ?graph {"
            "    ?s ?p ?o."
            "    ?s a ?type "
            "  }"
            "  VALUES (?graph ?type) { "
            "    (<carsGraph> <Car>) "
            "    (<peopleGraph> <http://xmlns.com/foaf/0.1/Person>) "
            "  }"
            "}"))
