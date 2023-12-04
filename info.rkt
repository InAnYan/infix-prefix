#lang info
(define collection "infix-prefix")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/infix-prefix.scrbl" ())))
(define pkg-desc "A library to convert prefix expressions into infix and vice versa. [Partially done]")
(define version "0.1")
(define pkg-authors '(InAnYan))
