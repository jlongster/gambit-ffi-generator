(include "streams#.scm")
(include "language#.scm")
(include "monad#.scm")
(include "kernel#.scm")

(include "expressions#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(include "number.scm")

(define-parser (sum)
  (>> (char #\+) (return +)))

(define-parser (dif)
  (>> (char #\-) (return -)))

(define-parser (mul)
  (>> (char #\*) (return *)))

(define-parser (div)
  (>> (char #\/) (return /)))

(define-parser (sqr)
  (>> (char #\^) (return (lambda (c) (* c c)))))

(define table
  (let(
       (prefix
        `((,dif 1)))
       
       (infix 
        `((,sum 1 left)
          (,dif 1 left)
          (,mul 2 left)
          (,div 2 left)))
       
       (postfix
        `((,sqr 3))))
  (make-operator-table
   prefix
   infix
   postfix)))

(define-parser (par)
  (>> (char #\()
      (<- e (expr table _term))
      (char #\))
      (return e)))

(define-parser (_term)
  (<> (num) (par)))

(define-parser (math-exp)
  (>> (<- r (expr table _term))
      (<> (eos) (fail "end expected"))
      (return r)))
                
(define (calc s)
  (run (math-exp) (string->stream s)))
