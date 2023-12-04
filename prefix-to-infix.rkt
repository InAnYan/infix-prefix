#lang racket

(module+ test
  (require rackunit))

(struct op-info (precedence associativity))

(define-syntax-rule (make-ops-table ASSOCS ...)
  (make-hash (make-ops-assocs ASSOCS ...)))

(provide make-ops-table)

(define-syntax make-ops-assocs
  (syntax-rules ()
    [(make-ops-assocs)
     empty]
    [(make-ops-assocs (OP NUM ASSOC) XS ...)
     (cons (cons 'OP (op-info NUM 'ASSOC))
           (make-ops-assocs XS ...))]))

(provide make-ops-assocs)

(define default-arithmetic-ops
  (make-ops-table
   [+ 1 left-right]
   [- 1 left]
   [* 2 left-right]
   [/ 2 left]))

(define (prefix->infix expr [ops default-arithmetic-ops])
  (remove-infix-parens
   (prefix->infix-raw
    (many->binary expr)) ops))

(provide prefix->infix)

(define/match (many->binary expr)
  [((list op X))
   (list op (many->binary X))]
  [((list op A B))
   (list op
         (many->binary A)
         (many->binary B))]
  [((list op A B XS ...))
   (list op
         (many->binary A)
         (many->binary (append (list op B) XS)))]
  [(_) expr])

(define (prefix->infix-raw expr)
  (cond
    [(unary? expr)
     (unary->infix-raw expr)]
    [(binary? expr)
     (binary->infix-raw expr)]
    [else
     expr]))

(define (unary->infix-raw expr)
  (list (get-op expr) (prefix->infix-raw (second expr))))

(define (binary->infix-raw expr)
  (let ([op (get-op expr)]
        [left (prefix->infix-raw (second expr))]
        [right (prefix->infix-raw (third expr))])
    (list left op right)))

(define (remove-infix-parens expr ops)
  (cond
    [(unary? expr)
     (remove-unary-parens expr ops)]
    [(binary? expr)
     (remove-binary-parens expr ops)]
    [else
     expr]))

(define (remove-unary-parens expr ops)
  expr)

(define (remove-binary-parens expr ops)
  (let ([op (second expr)]
        [left (first expr)]
        [right (third expr)])
    (append (maybe-enclosed left op ops 'left)
            (list op)
            (maybe-enclosed right op ops 'right))))

(define (maybe-enclosed expr op ops associativity)
  (let ([conv (remove-infix-parens expr ops)]
        [expr-op (get-op-infix expr)])
    (if (or (unary? expr) (binary? expr))
        (if (compare-ops expr-op op ops)
            (list conv)
            (if (associativy-compatible? (get-associativity op ops) associativity)
                conv
                (list conv)))
        (list conv))))

(define (associativy-compatible? a b)
  (if (or (equal? a 'left-right)
          (equal? b 'left-right))
      #t
      (equal? a b)))

(define (compare-ops a b ops)
  (< (get-precedence a ops)
     (get-precedence b ops)))

(define (get-precedence op ops)
  (op-info-precedence (hash-ref ops op)))

(define (get-associativity op ops)
  (op-info-associativity (hash-ref ops op)))

(define (unary? expr)
  (and (list? expr) (= (length expr) 2)))

(define (binary? expr)
  (and (list? expr) (= (length expr) 3)))

(define (get-op expr)
  (first expr))

(define (mklist x)
  (if (list? x)
      x
      (list x)))

(define (get-op-infix expr)
  (cond
    [(unary? expr) (first expr)]
    [(binary? expr) (second expr)]
    [else #f]))

(module+ test
  ; Primitives:
  (check-equal? (prefix->infix 1) 1)
  (check-equal? (prefix->infix 'a) 'a)
  (check-equal? (prefix->infix "str") "str")

  ; Unary:
  (check-equal? (prefix->infix '(- 7)) '(- 7))
  (check-equal? (prefix->infix '(- (- 7))) '(- (- 7)))
  (check-equal? (prefix->infix '(- (+ 1 2))) '(- (1 + 2)))
  (check-equal? (prefix->infix '(- (- (+ 1 2)))) '(- (- (1 + 2))))

  ; Binary (several):
  (check-equal? (prefix->infix '(+ 1 2)) '(1 + 2))
  (check-equal? (prefix->infix '(+ 1 (+ 2 3))) '(1 + 2 + 3))

  ; Binary (same precedence):
  (check-equal? (prefix->infix '(+ 1 (+ 2 3))) '(1 + 2 + 3))

  ; ???
  (check-equal? (prefix->infix '(+ a (- b c))) '(a + b - c))
  (check-equal? (prefix->infix '(+ (- a b) c)) '(a - b + c))

  (check-equal? (prefix->infix '(- a (+ b c))) '(a - (b + c)))

  ; Binary + unary:
  (check-equal? (prefix->infix '(+ (- a) b)) '(- a + b))

  ; Binary (different precedence):
  (check-equal? (prefix->infix '(+ a (* b c))) '(a + b * c))
  (check-equal? (prefix->infix '(* a (+ b c))) '(a * (b + c)))

  ; Binary with more than two arguments:
  (check-equal? (prefix->infix '(+ 1 2 3)) '(1 + 2 + 3))
  (check-equal? (prefix->infix '(+ 1 2 (* a b c))) '(1 + 2 + a * b * c))

  ; Associativity:
  (check-equal? (prefix->infix '(+ a (+ b c))) '(a + b + c))
  (check-equal? (prefix->infix '(+ (+ a b) c)) '(a + b + c))
  (check-equal? (prefix->infix '(- a (- b c))) '(a - (b - c)))
  (check-equal? (prefix->infix '(- (- a b) c)) '(a - b - c)))
  
