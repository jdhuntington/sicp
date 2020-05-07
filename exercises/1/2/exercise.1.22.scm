                                        ; code from the book

                                        ; section 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

                                        ; exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (let ((is-prime (prime? n)))
    (if is-prime
        (report-prime (- (runtime) start-time)))
    is-prime))                          ; modified to return prime value

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (find-n-primes-above start n)
  (if (> n 0)
      (if (timed-prime-test start)
          (find-n-primes-above (+ 1 start) (- n 1))
          (find-n-primes-above (+ 1 start) n))))

                                        ; result: EMACHINETOOFAST
