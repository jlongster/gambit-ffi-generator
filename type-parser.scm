(include "mparser-path.scm")

(load (string-append mparser-path "/streams.scm"))
(load (string-append mparser-path "/kernel.scm"))
(load (string-append mparser-path "/number.scm"))
(load (string-append mparser-path "/extras.scm"))


(define-parser (type)
  (<> (function)
      (const)
      (pointer)
      (array)
      (struct)
      (union)
      (enum)
      (unsigned)
      (base-type)))


(define-parser (parameter)
  (<> (type)
      (>> (char #\,)
          (type))))


(define-parser (parameters)
  (kleene parameter))


(define-parser (function)
  (>> (word "p.f(")
      (<- params (parameters))
      (word ").")
      (<- ret (type))
      (return (list 'function params ret))))


(define-parser (const)
  (>> (word "q(const).")
      (<- r0 (type))
      (return (list 'const r0))))
      
  
(define-parser (pointer)
  (>> (word "p.")
      (<- r0 (type))
      (return (list 'pointer r0))))


(define-parser (array)
  (>> (word "a(")
      (<- r0 (int))
      (word ").")
      (<- r1 (type))
      (return (list 'array r0 r1))))


(define-parser (tag t)
  (>> (word t)
      (char #\space)
      (<- r0 (identifier))
      (return (list (string->symbol t) r0))))


(define-parser (struct)
  (tag "struct"))


(define-parser (union)
  (tag "struct"))


(define-parser (enum)
  (tag "enum"))


(define-parser (unsigned)
  (>> (word "unsigned ")
      (<- r0 (identifier))
      (return (string->symbol (string-append "unsigned-" r0)))))


(define-parser (identifier-char)
  (<> (alpha)
      (digit)
      (char #\_)))


(define-parser (identifier+ r0)
  (<> (>> (<- r1 (identifier-char))
	  (identifier+ (string-append r0 (string r1))))
      (return r0)))


(define-parser (identifier)
  (>> (<- r0 (identifier-char))
      (identifier+ (string r0))))


(define-parser (base-type)
  (>> (<- r0 (identifier))
      (return (string->symbol r0))))


(define (parse-type str)
  (run (type) (string->stream str)))