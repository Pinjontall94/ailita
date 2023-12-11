(define-module (nnfs cost)
  #:use-module (nnfs aliases)
  #:use-module (srfi srfi-1)
  #:export (cost))

(define (cost observed expected)
  """
  Takes two lists of values 0 to 1, computes the sum of the squared residuals.
  """
  (apply + (map (lambda (x)
                        (expt (- (first x) (second x)) 2))
                (zip observed expected))))
