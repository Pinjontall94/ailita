(define-module (ailita)
  #:use-module (srfi srfi-1)
  #:use-module (ailita macros)
  #:use-module (ailita neuron)
  #:use-module (ailita seqs))

(display "ailita loaded!\n")

;; (format #t "~a\n" %load-path)

(define dbl-training-input '(0 1 2 3 4 5))
(define dbl-expected-output '(0 2 4 6 8 10))
(define dbl-training-set (zip dbl-training-input dbl-expected-output))

(define (mse a b)
  "Compute the mean squared error of two values."
  (let ((delta (- a b)))
    (* delta delta)))

(define (cost model input expected)
  "Compute the cost of a given model with respect to expected output."
  (let* [(w model)
	 (x input)
	 (exp expected)
	 ;; The actual output for the given model
	 (y (map (lambda (n) (* n w)) x))
	 (zipped-outputs (zip exp y))
	 ;; Run mse for each pair of expected and actual values
	 (pairwise-mse (map (lambda (p) (mse (first p) (second p)))
			    zipped-outputs))
	 ;; Sum all pairwise-mse to find the total cost
	 (result (fold + 0 pairwise-mse))]
    ;; Return the cost
    result))

;; (define pre-baked-random-weight (random 10))
(define pre-baked-random-weight 5)

;; if w = 5 => cost = 495
(define init-cost (cost pre-baked-random-weight dbl-training-input dbl-expected-output))

(define (dbl-cost w)
  (cost w dbl-training-input dbl-expected-output))

;; pick some tiny value to nudge w by
(define eps 1e-4)

(define (finite-difference f input-to-improve h)
  "We have differentiation at home. Differentiation at home:"
  (/ (- (f (+ input-to-improve h))
	(f input-to-improve))
     h))
