(##namespace
 ("reflect#"
  with-args
  reflect
  reify))

(define-macro (with-args x c)
  (append c x))

(define-macro (reflect v b . x)
  `(let ,(map list v x) ,b))

;; doesn't like references in quote, be aware of being macrologist
(define-macro (reify vm val . xs)
  `(with-args
    ,xs
    ,(let subst ((v (car vm))
                 (m (cadr vm))
                 (val val))
       (cond
        ((not (pair? val))
         val)
        
        ((eq? (car val) v)
         (append m (subst v m (cdr val))))
        
        (else
         (cons (subst v m (car val))
               (subst v m (cdr val))))))))



