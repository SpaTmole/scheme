#lang racket
(define (x y z) (+ y z))

(define (A x y) ;Ackerman
  (cond ((= y 0 ) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds 0)) 0)
        (else (+ (cc amount (- kinds 1))
                 (cc (- amount (denum kinds)) kinds)))))

(define (denum kind)
  (cond ((= kind 1) 1)
        ((= kind 2) 5)
        ((= kind 3) 10)
        ((= kind 4) 25)
        ((= kind 5) 50)))
                     
(define (extfib-r n)
  (cond ((< n 3) n)
        (else (+ (extfib-r (- n 1)) (extfib-r (- n 2)) (extfib-r (- n 3))))))

(define (double proc)
  (lambda (x) (proc 
                (proc x))))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(define (compose f g)
    (lambda (x)(f (g x))))

(define (repeated f n)
  (define (iter-repeated f n)
    (cond ((= n 0) f)
          (else (lambda(x)((compose f (iter-repeated f (- n 1))) x))))
    )
  (iter-repeated f (- n 1))
  )