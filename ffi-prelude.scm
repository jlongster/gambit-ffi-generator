(define-macro (define-constant name type)
  `(define ,name (c-lambda () ,type ,(string-append "___result = " (symbol->string name) ";"))))