(##namespace
 ("language#"
  run
  run-ndet
  parser-eval
  parser
  define-parser))

(include "reflect#.scm")
(include "monad#.scm")

(define-macro (run p s)
  (let(
       (vv (gensym 'vv))
       (ts (gensym 'ts))
       (fl (gensym 'fl)))
    `(with-args (,s (lambda (,vv ,ts ,fl) ,vv) raise)
                ,p)))

(define-macro (run-ndet p s)
  (let(
       (vv (gensym 'vv))
       (ts (gensym 'ts))
       (fl (gensym 'fl)))
    `(with-args (,s (lambda (,vv ,ts ,fl) (values ,vv ,fl)) raise)
                ,p)))


(define-macro (parser-eval e . x)
  (cond
   ((and (list? e) (eq? (car e) '>>) (= 2 (length e)))
    `(with-args ,x (parser-eval ,(cadr e))))
   
   ((and (list? e) (eq? (car e) '<>) (= 2 (length e)))
    `(with-args ,x (parser-eval ,(cadr e))))
   
   ((and (list? e) (eq? (car e) '<*>) (= 2 (length e)))
    `(with-args ,x (parser-eval ,(cadr e))))

   ((and (list? e) (eq? (car e) '>>))
    (let(
         (c (cadr e)))
      (if (and (list? c) (eq? (car c) '<-) (= (length c) 3))
          `(with-args ,x (bind (,(cadr c) (parser-eval ,(caddr c)))
                               (parser-eval (>> ,@(cddr e)))))
          (let(
               (__ (gensym '__)))
            `(with-args ,x (bind (,__ (parser-eval ,(cadr e)))
                                 (parser-eval (>> ,@(cddr e)))))))))

   ((and (list? e) (eq? (car e) '<>))
    `(with-args ,x (orelse (parser-eval ,(cadr e))
                           (parser-eval (<> ,@(cddr e))))))
   
   ((and (list? e) (eq? (car e) '<*>))
    `(with-args ,x (orelse* (parser-eval ,(cadr e))
                            (parser-eval (<*> ,@(cddr e))))))

   ((and (list? e) (eq? (car e) 'if))
    `(if ,(cadr e)
         (with-args ,x (parser-eval ,(caddr e)))
         (with-args ,x (parser-eval ,(cadddr e)))))
   ((and (list? e)
         (or (eq? (car e) 'let)
             (eq? (car e) 'let*)
             (eq? (car e) 'letrec)))
    `(,(car e) ,(cadr e) (with-args ,x (parser-eval ,(caddr e)))))

   (else
    `(with-args ,x ,e))))

(define-macro (parser f b)
  `(gamma ,f (parser-eval ,b)))


(define-macro (define-parser s b)
  `(define ,(car s) (parser ,(cdr s) ,b)))
