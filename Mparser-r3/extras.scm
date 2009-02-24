(##namespace ("extras#"))

(##include "~~/lib/gambit#.scm")

(include "language#.scm")
(include "kernel#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define-parser (kleene p)
  (letrec(
          (kl (parser (es)
                      (<> (>> (<- e (p))
                              (kl (cons e es)))
                          (return (reverse es))))))
    (kl '())))

(define-parser (times n p)
  (letrec(
          (tm (parser (n)
                      (if (= n 0) (return '())
                          (>> (<- e (p))
                              (<- es (tm (- n 1)))
                              (return (cons e es)))))))
    (tm n)))
  
(define-parser (upto n p)
  (letrec(
          (ut (parser (j es)
                      (if (= j 0) (return (reverse es))
                          (<> (>> (<- e (p))
                                  (ut (- j 1) (cons e es)))
                              (return (reverse es)))))))
    (ut n '())))

(define-parser (repeat n p)
  (>> (<- h (times n p))
      (<- t (kleene p))
      (return (append h t))))

(define-parser (repeat-max n m p)
  (>> (<- h (times n p))
      (<- t (upto (- m n) p))
      (return (append h t))))

(define-parser (maybe p)
  (<> (p) (return '())))

(define-parser (word w)
  (let(
       (l (string-length w)))
    (letrec(
            (wd (parser (j)
                        (if (>= j l) (return w)
                            (>> (char (string-ref w j))
                                (wd (+ j 1)))))))
      (wd 0))))
        