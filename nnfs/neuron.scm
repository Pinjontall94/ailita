(define-module (nnfs neuron)
  #:use-module (nnfs macros)
  #:use-module (nnfs aliases)
  #:use-module (nnfs seqs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (neural-network
	    make-neuron
	    sigmoid-neuron
	    sigmoid
	    derived-sigmoid
	    relu
	    derived-relu
	    softplus))

;; Activation Functions
(define (sigmoid x)
  (/ 1 (+ 1 (expt 2.71828 (* -1 x)))))

(define (derived-sigmoid x)
  (* (sigmoid x)
     (1 - (sigmoid x))))

(define (relu x)
  (if (< 0 x) x 0))

(define (derived-relu x)
  (if (< 0 x) 1 0))

(define (softplus x)
  (log (+ 1 (expt 2.71828 x))))


;; Neurons
(define (sum-vector v)
  (let loop [(i 0)
	     (acc 0)]
    (if (= i (vector-length v))
	acc
	(loop (+ i 1)
	      (+ acc (vector-ref v i))))))

(define (dot-product lst1 lst2)
  (sum (map * lst1 lst2)))

(define (dot-product-vector v1 v2)
  (sum-vector (vector-map (lambda (i x y) (* x y))
				 v1 v2)))

(define (make-neuron activation-func)
  (lambda (weights bias activations)
    (activation-func (+ bias
                      (dot-product weights activations)))))

(define (sigmoid-neuron weights bias activations)
  ((make-neuron sigmoid) weights bias activations))


;; Layers
(define (sigmoid-layer weights biases activations)
  (map (lambda (weights biases)
           (sigmoid-neuron weights
                           biases
                           activations))
         weights biases))


;; Feed-forward network
(define (neural-network weights biases activations)
  (fold-right (lambda (w b acc)
                (if (or (null? w) (null? b)) ;; Could add conditionals for different act funcs
                    acc
                    (sigmoid-layer w b acc)))
              activations
              weights biases))
