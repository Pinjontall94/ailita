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
  """Compute the mean squared error of two values."""
  (let ((delta (- a b)))
    (* delta delta)))

(define (cost model input expected)
  """Compute the cost of a given model with respect to expected output."""
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
    ;; Print/format the inputs, intermediate values, and result before returning
    ;; the result from the function
    (and (format #t "w (model): ~a\n" w)
	 (format #t "x (input): ~a\n" x)
	 (format #t "exp (expected output): ~a\n" exp)
	 (format #t "y (actual output): ~a\n" y)
	 (format #t "========================================\n")
	 (map (lambda (p)
		(format #t "expected: ~a\tactual: ~a\n"
			(first p) (second p)))
	      zipped-outputs)
	 (format #t "pairwise-mse: ~a\n" pairwise-mse)
	 (format #t "cost of the model:\n"))
    result))
