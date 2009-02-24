(##namespace ("streams#"))

(##include "~~/lib/gambit#.scm")
(include "streams#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe))

(define (stream-car s) (car (force s)))
(define (stream-cdr s) (cdr (force s)))

(define (port->stream p)
  (stream-cons
   (let(
        (c (read-char p)))
     (if (eof-object? c) #\nul c))
   (port->stream p)))

(define (string->stream s)
  (list->stream (string->list s)))

(define nul-stream (stream-cons #\nul nul-stream))

(define (list->stream l)
  (if (null? l) nul-stream
      (stream-cons (car l) (list->stream (cdr l)))))

(define (stream . l) (list->stream l))

(define (stream->list s)
  (if (char=? (stream-car s) #\nul) '()
      (cons (stream-car s) (stream->list (stream-cdr s)))))

(define (stream->string s)
  (list->string (stream->list s)))

(define (stream->port s p)
  (if (char=? #\nul (stream-car s)) '()
      (begin
        (display (stream-car s) p)
        (stream->port (stream-cdr s) p))))