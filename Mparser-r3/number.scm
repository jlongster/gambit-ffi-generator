;; File: number.scm
;; Author: Francesco Bracchi

(include "streams#.scm")
(include "language#.scm")
(include "monad#.scm")
(include "kernel#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define idigit
  (let(
       (i0 (char->integer #\0)))
    (parser ()
            (>> (<- c (digit))
                (return (- (char->integer c) i0))))))

(define-parser (int+ c)
  (<> (>> (<- c1 (idigit))
          (int+ (+ c1 (* 10 c))))
      (return c)))

(define-parser (int) 
  (>> (<- d (idigit))
      (int+ d)))

(define-parser (frac)
  (>> (char #\.)
      (<- d (idigit))
      (frac+ (/ 1 100) (/ d 10))))

(define-parser (frac+ m c0)
  (<> (>> (<- c (idigit))
          (frac+ (/ m 10) (+ (* m c) c0)))
      (return c0)))

(define-parser (sign)
  (<> (>> (char #\+) (return (lambda (n) n)))
      (>> (char #\-) (return (lambda (n) (- n))))
      (return (lambda (n) n))))
                      
(define-parser (pow10)
  (>> (<> (char #\e) (char #\E))
      (<- s (sign))
      (<- i (int))
      (return (s i))))

;; NUMBER : 1 1.1 1.1e-10 1.1e+10 +1 -1 1.1e10 ...
(define-parser (num)
  (>> (<- s (sign))
      (<- ip (int))
      (<- fp (<> (frac) (return 0)))
      (<- ex (<> (pow10) (return 0)))
      (return (s (* (+ ip fp) (expt 10 ex))))))

(define-parser (number)
  (>> (<- n (num))
      (eos)
      (return n)))

;; string->number
(define (string->number* s)
  (run (number) (string->stream s)))
