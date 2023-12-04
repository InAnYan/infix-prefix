#lang scribble/manual

@require[@for-label[infix-prefix
                    racket/base]]

@title{infix-prefix: a library to convert prefix expressions to infix and vice versa}
@author{Ruslan Popov}

@defmodule[infix-prefix]

@section{Warnings}
@itemlist[@item{It is not heavily tested.}
          @item{Infix to prefix functionality does not count proper precedence and associativity.}
          @item{I'm not sure if unary operators are parsed correctly.}]

@section{Prefix to infix}
In order to convert a prefix expressions to infix you need to create a table of operators (by @racket[make-ops-table]).

Then you should call the @racket[prefix->infix] function with an expression to parse and your operators table. From the library, a default operators table is available for addition, subtraction, multiplication and division.

Example:
@codeblock{
 #lang racket

 (require infix-prefix)

 (define my-ops
 (make-ops-table
 [+ 1 left-right]
 [- 1 left]
 [* 2 left-right]
 [/ 2 left]))
   
 (println (prefix->infix '(+ x y (* z w)) my-ops)) ; '(x + y + z * w)
}

Inner implementation:

@itemlist[@item{Firstly, expression is converted to a binary tree. That means all forms like @tt{(+ 1 2 3)} is converted to @tt{(+ 1 (+ 2 3))}. WARNING: There is a bug, this works correctly for left associative expressions only.}
          @item{Then, binary expression are convert to prefix form. This form does not count associativity and precedence, instead it wraps all in parenthesis.}
          @item{Finally, groupings are removed according to operators table were they are unnecessary.}]

@section{Infix to prefix}
In order to convert an infix expression to prefix you need to define a special parser with @racket[define-infix->prefix-parser]. You should supply name and operators that you need to parse.

Then you can call that parser by name and supply an expression.

Example:
@codeblock{
 #lang racket

 (require infix-prefix)

 (define-infix->prefix-parser my-parser
 + - * /)

 (println (my-parser '(x + y * z))) ; '(+ x (* y z))
}

Inner implementation:

The @racket[define-infix->prefix-parser] form uses @racket[match]. It searches for operators inside an expression, splits expression into left and right part, and then calls itself recursively and makes a prefix expression.

@section{Functions}

@defproc[(prefix->infix [expr any/c] [ops hash?])
         any/c]{
 Converts a prefix expression into infix form by operators table.
 The operators table must be created via @racket[make-ops-table] form.
}

@defform[(make-ops-table clause ...)
         #:grammar
         [(clause (operator precedence associativity))]
         #:contracts ([operator symbol?]
                      [precedence integer?]
                      [associativity (or/c 'left 'right 'left-right)])]{
 Create an operators table. For more details refer to the provided grammar of clause.
}

@defform[(define-infix->prefix-parser name op ...)
         #:contracts ([name symbol?]
                      [op symbol?])]{
 Define an infix to prefix parser with name @racket[name]. The precedence of operators is encoded in the order of @racket[op].
}
