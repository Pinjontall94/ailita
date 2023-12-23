(define-module (nnfs seqs)
  #:use-module (nnfs macros)
  #:use-module (srfi srfi-1)
  #:export (make-normalized-random-array
            make-random-weights-biases))

(define (repeat num times)
  (let loop [(num num)
             (acc '())
             (count 0)]
    (if (= count times)
        acc
        (loop num
              (cons num acc)
              (+ 1 count)))))


(define (repeat-vector num times)
  (->> (repeat num times)
       (list->vector)))


(define (repeat-array num cols rows)
  (-> num
      (repeat cols)
      (repeat rows)))


(define (repeat-vector-array num cols rows)
  (-> num
      (repeat-vector cols)
      (repeat-vector rows)))


(define (make-normalized-random-vector dims)
  (let [(zero-vector (repeat 0.0 dims))]
    (->> zero-vector
         (map (lambda (el)
                (/ (+ el (random 100))
                   100))))))


(define (make-normalized-random-matrix cols rows)
  (let [(zero-array (repeat-array 0.0 cols rows))]
    (->> zero-array
         (map (lambda (row)
                (map (lambda (el)
                       (/ (+ el (random 100))
                          100))
                     row))))))


(define (make-random-vector-list architecture-clist)
  (fold-right (lambda (el acc)
                (if (null? el)
                    acc
                    (cons (make-normalized-random-array el)
                          acc)))
              '()
              architecture-clist))
