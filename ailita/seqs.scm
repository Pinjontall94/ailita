(define-module (ailita seqs)
  #:use-module (ailita macros)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (repeat
	    repeat-array
	    repeat-vector
	    repeat-vector-native
	    make-random-vector-list
	    make-normalized-random-array
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

(define (repeat-vector-native num times)
  ;; values call determines construction of current and next val at index i
  (vector-unfold (lambda (i x) (values x x))
		 times
		 num))

(define (repeat-array num cols rows)
  (-> num
      (repeat cols)
      (repeat rows)))


(define (make-normalized-random-vector dims)
  (let [(zero-vector (repeat 0.0 dims))]
    (->> zero-vector
	 ;; Note that lambdas passed to vector-map need an extra index arg
         (vector-map (lambda (i el)
                (/ (+ el (random 100))
                   100))))))


(define (make-normalized-random-matrix cols rows)
  (let [(zero-array (repeat-array 0.0 cols rows))]
    (->> zero-array
         (vector-map (lambda (i row)
                (vector-map (lambda (i el)
                       (/ (+ el (random 100))
                          100))
                     row))))))


(define (make-random-vector-list architecture-list)
  (fold-right (lambda (el acc)
                (if (null? el)
                    acc
                    (cons (make-normalized-random-vector el)
                          acc)))
              '()
              architecture-list))
