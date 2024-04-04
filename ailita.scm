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
  (define (ff model input)
    (map (lambda (n) (* n model)) input))
  (let* [(w model)
	 (x input)
	 (exp expected)
	 ;; The actual output for the given model
	 ;; (y (map (lambda (n) (* n w)) x))
	 (y (ff w x))
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

;; If w = 5 => cost = 495
(define init-cost (cost pre-baked-random-weight dbl-training-input dbl-expected-output))

(define (dbl-cost w)
  (cost w dbl-training-input dbl-expected-output))

;; Pick some tiny value to nudge w by
(define eps 1e-4)

;; We have derivatives at home. Derivatives at home:
(define (finite-difference f input-to-improve h)
  "Compute the approximate derivative of function at a point."
  (/ (- (f (+ input-to-improve h))
	(f input-to-improve))
     h))

;; Pick some small value to step as you walk down the gradient
(define step 1e-2)

(define (gradient-descent f starting-point step-size epsilon generations)
  (let loop [(start starting-point)
	     (count generations)]
    (if (= count 0)
	(begin
	  ;; exit condition
	  (format #t "============================================\n")
	  (format #t "start: ~f\tresult: ~f\n" start (f start)))
	(begin
	  (format #t "start: ~f\tresult: ~f\n" start (f start))
	  (loop (- (* step-size
		    (finite-difference f start epsilon)))
	      (- count 1))))))
