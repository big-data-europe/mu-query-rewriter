(*functional-properties* '(rdf:type))

(*unique-variables* '(?user ?role))

(*query-functional-properties?* #t)

(define-constraint
  'write
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

(define-constraint
  'write
  (conc "PREFIX mu: <http://mu.semte.ch/vocabularies/core/> "
        "CONSTRUCT {"
        "  ?s ?p ?o."
        "}"
        "WHERE {"
        "  GRAPH <http://mu.semte.ch/authorization> { "
        "    <SESSION> mu:account ?user. "
        "    ?user <role> \"editor\". "
        "  } "
        "  GRAPH ?graph {"
        "    ?s ?p ?o."
        "    ?s a ?type "
        "  }"
        "  VALUES (?graph ?type) { "
        "    (<carsGraph> <Car>) "
        "    (<peopleGraph> <http://xmlns.com/foaf/0.1/Person>) "
        "  }"
        "}"))
