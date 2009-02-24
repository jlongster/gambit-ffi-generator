(##include "~~/lib/gambit#.scm")

(include "streams#.scm")
(include "language#.scm")
(include "kernel#.scm")

(include "extras#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (not safe)
         (block))

;; json to scheme transform

(define-parser (json)
  (>> (sp) (json-value)))

(define-parser (json-value)
  (<> (json-object)
      (json-array)
      (json-string)
      (json-number)
      (json-null)
      (json-undefined)
      (json-bool)))

(define-parser (json-null)
  (>> (word "null")
      (return '())))

(define-parser (json-undefined)
  (>> (word "undefined")
      (return '())))

(define-parser (json-bool)
  (<> (json-true)
      (json-false)))

(define-parser (json-string)
  (<> (double-quotes-string)
      (single-quotes-string)))

(define-parser (json-number)
  (<> (p-inf)
      (n-inf)
      (exponential)
      (signed-float)
      (signed-integer)))

(define-parser (json-array)
  (>> (char #\[) (sp)
      (<- ls (<> (>> (<- c (json-value))
                     (<- cs (kleene (parser () (>> (char #\,) (sp) (json-value)))))
                     (return (cons c cs)))
                 (return '())))
      (char #\])
      (return (apply vector ls))))

(define-parser (json-object)
  (>> (char #\{) (sp)
      (<- as (<> (>> (<- c (json-object-pair))
                     (<- cs (kleene
                             (parser ()
                                     (>> (char #\,)
                                         (sp)
                                         (json-object-pair)))))
                     (return (cons c cs)))
                 (return '())))
      (char #\})
      (return (list->table as))))

(define-parser (sp)
  (<> (>> (whitespace) (sp))
      (return #\space)))

(define-parser (json-true)
  (>> (word "true")
      (return #t)))

(define-parser (json-false)
  (>> (word "false")
      (return #f)))

(define-parser (double-quotes-string)
  (>> (char #\")
      (<- cs (more-double-quotes-string))
      (return (list->string cs))))

(define-parser (more-double-quotes-string)
  (<> (>> (char #\")
          (return '()))
      (>> (char #\\)
          (<- c (any))
          (<- cs (more-double-quotes-string))
          (return (cons c cs)))
      (>> (<- c (any))
          (<- cs (more-double-quotes-string))
          (return (cons c cs)))))

(define-parser (single-quotes-string)
  (>> (char #\')
      (<- cs (more-single-quotes-string))
      (return (list->string cs))))

(define-parser (more-single-quotes-string)
  (<> (>> (char #\')
          (return '()))
      (>> (char #\\)
          (<- c (any))
          (<- cs (more-single-quotes-string))
          (return (cons c cs)))
      (>> (<- c (any))
          (<- cs (more-single-quotes-string))
          (return (cons c cs)))))

(define-parser (p-inf)
  (>> (word "Infinity")
      (return +inf.0)))

(define-parser (n-inf)
  (>> (word "-Infinity")
      (return '-inf.0)))

(define-parser (exponential)
  (>> (<- b (<> (signed-float)
                (signed-integer)))
      (<> (char #\e)
          (char #\E))
      (<- e (signed-integer))
      (return (* b (expt 10 e)))))

(define-parser (signed-float)
  (signed float))

(define-parser (signed-integer)
  (signed integer))

(define-parser (signed w)
  (>> (<- s (sign))
      (<- i (w))
      (return (s i))))

(define-parser (sign)
  (<> (>> (char #\+) (return +))
      (>> (char #\-) (return -))
      (return +)))

(define-parser (float)
  (>> (<- i (<> (integer) (return 0)))
      (char #\.)
      (<- ds (kleene digit-number))
      (let(
           (f (fold-right (lambda (d i) (/ (+ i d) 10)) ds 0)))
        (return (+ i f)))))

(define-parser (integer)
  (>> (<- i (repeat 1 digit-number))
      (return (fold-left (lambda (i d) (+ d (* 10 i))) 0 i))))

(define (fold-left f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
        (fold (f i (car l)) (cdr l)))))

(define (fold-right f l i)
  (let fold ((l l) (i i))
    (if (null? l) i
        (f (car l) (fold (cdr l) i)))))

(define digit-number
  (let(
       (d0 (char->integer #\0)))
    (parser ()
            (>> (<- d (digit))
                (return (- (char->integer d) d0))))))

(define-parser (key)
  (>> (<- c (<> (alpha) (char #\_) (char #\$)))
      (<- cs (kleene
              (parser ()
                      (<> (alpha)
                          (digit)
                          (char #\_)
                          (char #\$)))))
      (return (list->string
               (cons c cs)))))

;; Could be the key only a string or a keyword symbol too?
(define-parser (json-object-pair)
  (>> (<- k (<> (json-string) (key)))
      (sp)
      (char #\:)
      (sp)
      (<- v (json-value))
      (sp)
      (return (cons k v))))

(define (json->scheme s)
  (run (json) (string->stream s)))

