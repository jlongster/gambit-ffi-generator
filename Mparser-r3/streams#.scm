(##namespace
 ("streams#"
  stream-cons
  stream-car
  stream-cdr
  port->stream
  string->stream
  nul-stream
  list->stream
  strewam
  stream->list
  stream->string
  stream->port))

(define-macro (stream-cons a d)
  `(delay (cons ,a ,d)))