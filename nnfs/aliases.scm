(define-module (nnfs aliases)
  #:export (first rest second first-of-first first-of-second))

(define first car)
(define rest cdr)
(define second cadr)
(define first-of-first caar)
(define first-of-second caadr)
