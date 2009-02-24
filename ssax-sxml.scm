(include "ssax-sxml-path.scm")

(load (string-append ssax-sxml-path "/ssax-sxml/libs/util"))
(load (string-append ssax-sxml-path "/ssax-sxml"))


(define (sxml:complement pred)
  (lambda(node)
    (case (pred node)
      ((#f ()) node)
      (else #f))))
