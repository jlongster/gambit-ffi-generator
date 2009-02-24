(##namespace ("monad#"
              gamma
              return
              bind
              orelse
              orelse*))

(include "reflect#.scm")

(define-macro (gamma f b)
  (let(
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(lambda ,(append f (list ts sc fl))
       (with-args (,ts ,sc ,fl) ,b))))

(define-macro (return v . x)
  (let(
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl)))
    `(with-args ,x
                (reflect (,ts ,sc ,fl)
                         (,sc ,v ,ts ,fl)))))

(define-macro (bind p n ts sc fl)
  (let(
       (v (car p))
       (m (cadr p))
       (ts1 (gensym 'ts1))
       (fl1 (gensym 'fl1)))
    `(with-args
      (,ts
       (lambda (,v ,ts1 ,fl1)
         (with-args (,ts1 ,sc ,fl) ,n))
       ,fl)
      ,m)))

(define-macro (orelse* m n . x)
  (let(
       (_m (gensym '_m))
       (_n (gensym '_n))
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (rr (gensym 'rr)))
    `(with-args ,x
                (reify (,_m ,m)
                       (reify (,_n ,n)
                              (reflect (,ts ,sc ,fl)
                                       (,_m ,ts ,sc
                                            (lambda (,rr) (,_n ,ts ,sc ,fl)))))))))

(define-macro (orelse m n . x)
  (let(
       (_m (gensym '_m))
       (_n (gensym '_n))
       (ts (gensym 'ts))
       (sc (gensym 'sc))
       (fl (gensym 'fl))
       (vv (gensym 'vv))
       (rr (gensym 'rr)))
    `(with-args ,x
                (reify (,_m ,m)
                       (reify (,_n ,n)
                              (reflect (,ts ,sc ,fl)
                                       (,_m ,ts
                                            (lambda (,vv ,ts ,fl) (,sc ,vv ,ts ,fl))
                                            (lambda (,rr) (,_n ,ts ,sc ,fl)))))))))