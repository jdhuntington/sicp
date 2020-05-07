(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

(define (fp-iter base cap terms)
  (let ((t1 (car terms))
        (t2 (car (cdr terms)))
        (t3 (car (cdr (cdr terms)))))
    (display (list t1 t2 t3 terms))
    (cond ((= base cap) t1)
          (else
           (let ((next-term (+ t1 (* 3 t2) (* 3 t3))))
             (fp-iter
              (+ 1 base)
              cap
              (cons next-term terms)))))))


(define (fp n)
  (cond ((< n 3) n)
        (else (fp-iter 3 n '(4 2 1)))))




(define (thing x)
  (let ((y 2))
    (+ x y 1)))
