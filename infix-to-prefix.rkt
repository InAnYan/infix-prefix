#lang racket

(module+ test
  (require rackunit))

(define-syntax define-infix->prefix-parser
  (syntax-rules ()
    [(_ name OPS ...)
     (define (name x)
       (match x
         [(list XS (... ...) 'OPS YS (... ...))
          (list 'OPS (name XS) (name YS))] ...
         [(list X)
          (name X)]
         [_ x]))]))

(provide define-infix->prefix-parser)


(module+ test
  (define-infix->prefix-parser arith
    + - * /)

  ; Dummy cases.
  (check-equal? (arith 1) 1)
  (check-equal? (arith 'x) 'x)
  (check-equal? (arith '(1)) '1)
  (check-equal? (arith '(x)) 'x)

  ; One operator.
  (check-equal? (arith '(x + y)) '(+ x y))
  (check-equal? (arith '(x * y)) '(* x y))

  ; Sequence of one operator.
  (check-equal? (arith '(x + y + z)) '(+ (+ x y) z))

  ; Grouping.
  (check-equal? (arith '(x + (y + z))) '(+ x (+ y z)))

  ; Different operators.
  (check-equal? (arith '(x + y * z)) '(+ x (* y z)))
  (check-equal? (arith '(x * y + z * w)) '(+ (* x y) (* z w))))
