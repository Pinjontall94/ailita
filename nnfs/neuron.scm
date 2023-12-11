(define-module (nnfs neuron)
  #:use-module (nnfs macros)
  #:use-module (nnfs aliases)
  #:use-module (srfi srfi-1)
  #:export (neuron sigmoid relu softplus))

(define first car)
(define rest cdr)
(define second cadr)
(define first-of-first caar)
(define first-of-second caadr)


(define (dot-product v1 v2)
  ;; Define a recursive iterative process on each vec's values
  (define (dot-product-iter acc el)
    ;; Standard scheme case analysis for recursive iteration
    (if (or (nil? (first el))
            (nil? (second el)))
        ;; Break condition
        acc
        ;; Pass new state into recursive function call
        (let ((vec1 (first el))
              (vec2 (second el))
              (scalar1 (first-of-first el))
              (scalar2 (first-of-second el)))
          ;; new acc = acc + (v1[0] * v2[0])
          ;; new el = v1[1:], v2[1:]
          (dot-product-iter (+ acc (* scalar1 scalar2))
                (list (rest vec1)
                      (rest vec2))))))

  ;; Case analysis for the dot-product
  (if (and (number? v1)
           (number? v2))
      ;; If v1 and v2 are simply numbers, just multiply and return product
      (* v1 v2)
      ;; Else, Accumulate by the iterative procedure dot-product-iter
      ;; Kick off the iterative procedure with 0 as acc, and el as (v1, v2)
      (dot-product-iter 0 (list v1 v2))))


(define (neuron activation-func weight-vec act-vec bias)
  (activation-func (+ bias
                      (dot-product weight-vec act-vec))))

(define (sigmoid x)
  (/ 1 (+ 1 (expt 2.71828 (* -1 x)))))

(define (relu x)
  (if (< 0 x) x 0))

(define (softplus x)
  (log (+ 1 (expt 2.71828 x))))
