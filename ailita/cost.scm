(define-module (ailita cost)
  #:use-module (ailita aliases)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (quadratic-cost))

(define (vector-sum v)
  (let [(v-len (vector-length v))]
    (do ((i 0 (+ i 1))
	 (sum 0 (+ sum (vector-ref v i))))
	((= i v-len) sum))))

(define (quadratic-cost observed expected)
  """
  Takes two lists of values 0 to 1, computes the sum of the squared residuals.
  """
  (sum (map (lambda (x)
	      (expt (- (first x) (second x)) 2))
	    (zip observed expected))))


;; (define (cross-entropy-cost observed expected))
