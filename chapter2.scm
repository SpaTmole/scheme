#lang racket
(define (equal? list1 list2)
  (cond ((and (eq? list1 '()) (eq? list2 '())) true)
        ((eq? (car list1) (car list2)) (equal? (cdr list1) (cdr list2)))
        (else false)))
                                     