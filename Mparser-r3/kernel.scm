(##namespace ("kernel#"))

(##include "~~/lib/gambit#.scm")

(include "reflect#.scm")
(include "streams#.scm")
(include "language#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define-structure parser-exception reason)

(define-parser (char c0)
  (reflect (ts sc fl)
           (if (char=? (stream-car ts) c0)
               (sc c0 (stream-cdr ts) fl)
               (fl (make-parser-exception
                    (string-append
                     "not "
                     (list->string (list c0))))))))

(define-parser (digit)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (char-numeric? c)
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not a digit"))))))

(define-parser (upcase)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (and (char>=? c #\A) (char<=? c #\Z))
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not an upcase"))))))

(define-parser (locase)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (and (char>=? c #\a) (char<=? c #\z))
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not a locase"))))))

(define-parser (interval l u)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (and (char>=? c l) (char<=? c u))
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not in interval"))))))

(define-parser (alpha)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (char-alphabetic? c)
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not an alpha"))))))

(define-parser (whitespace)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (char-whitespace? c)
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not a whitespace"))))))

(define-parser (eos)
  (reflect (ts sc fl)
           (if (char=? (stream-car ts) #\nul)
               (sc #\nul (stream-cdr ts) fl)
               (fl (make-parser-exception "not end")))))

(define-parser (any)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (not (char=? c #\nul))
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "not any"))))))

(define-parser (test-token t?)
  (reflect (ts sc fl)
           (let(
                (c (stream-car ts)))
             (if (t? c)
                 (sc c (stream-cdr ts) fl)
                 (fl (make-parser-exception "test failed"))))))

(define-parser (fail r)
  (reflect (ts sc fl)
           (fl (make-parser-exception r))))

(define-parser (token-stream)
  (reflect (ts sc fl) (sc ts ts fl)))

(define-parser (continuation)
  (reflect (ts sc fl) (sc (lambda (v) (sc v ts fl)) ts fl)))

(define-parser (failure)
  (reflect (ts sc fl) (sc fl ts fl)))

