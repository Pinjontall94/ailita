(define-module (nnfs neuron)
  #:use-module (nnfs macros)
  #:use-module (nnfs seqs)
  #:use-module (srfi srfi-1)
  #:export (neural-network sigmoid-neuron derived-sigmoid relu sigmoid softplus))

;; Niceties
(define rest cdr)
(define first-of-first caar)
(define first-of-second caadr)


;; Activation Functions
(define (sigmoid x)
  (/ 1 (+ 1 (expt 2.71828 (* -1 x)))))

(define (derived-sigmoid x)
  (* (sigmoid x)
     (1 - (sigmoid x))))

(define (relu x)
  (if (< 0 x) x 0))

(define (softplus x)
  (log (+ 1 (expt 2.71828 x))))


;; Neurons
(define (dot-product v1 v2)
  (apply + (map * v1 v2)))

(define (make-neuron activation-func)
  (lambda (weights bias activations)
    (activation-func (+ bias
                      (dot-product weights activations)))))

(define (sigmoid-neuron weights bias activations)
  ((make-neuron sigmoid) weights bias activations))


;; Layers
(define (sigmoid-layer weights biases activations)
  (let [(per-neuron-weights-biases (zip weights biases))]
    (map (lambda (wb)
           (sigmoid-neuron (first wb)
                           (second wb)
                           activations))
         per-neuron-weights-biases)))


;; Feed-forward network
(define (neural-network weights biases activations)
  (fold-right (lambda (acc el)
                (if (null? el) ;; Could add conditionals for different act funcs
                    acc
                    (sigmoid-layer (first el)
                                   (second el)
                                   acc)))
              activations
              (zip weights biases)))
