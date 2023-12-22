(define-module (nnfs seqs)
  #:use-module (nnfs macros)
  #:export (make-normalized-random-array))

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


(define (make-normalized-random-array cols rows)
  (let [(zero-array (repeat-array 0.0 cols rows))
        (scale-factor 100)]
    (map (lambda (row)
           (map (lambda (element)
                  (/ (+ element (random scale-factor)
                     scale-factor)))
                row))
         zero-array)))
