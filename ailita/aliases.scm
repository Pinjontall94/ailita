(define-module (ailita aliases)
  #:export (rest first-of-first first-of-second))

(define rest cdr)
(define first-of-first caar)
(define first-of-second caadr)

(define (sum list)
  (apply + list))
