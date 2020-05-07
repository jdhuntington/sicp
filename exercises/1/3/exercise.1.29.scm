;; Exercise 1.29.
;;
;; Simpson's Rule is a more accurate method of numerical integration
;; than the method illustrated above. Using Simpson's Rule, the
;; integral of a function f between a and b is approximated as where h
;; = (b - a)/n, for some even integer n, and yk = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson's Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n = 100 and n =
;; 1000), and compare the results to those of the integral procedure
;; shown above.
(define (my-integral-mk-terms f a b n h offset acc)
  (if (< n offset)
      acc
      (let ((multiplier
             (cond
              ((= 0 offset) 1)
              ((= n offset) 1)
              ((= 1 (modulo offset 2)) 2)
              (else 4))))
        (let ((next-term (* multiplier (f (+ a (* offset h))))))
          (my-integral-mk-terms f a b n h (+ 1 offset) (cons next-term acc))))))

(define (my-integral f a b n)
  (let ((h (/ (- b a) n)))
    (let ((terms (my-integral-mk-terms f a b n h 0 '())))
      (/ (* h (apply + terms) 3)))))
