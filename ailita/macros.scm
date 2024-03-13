(define-module (ailita macros)
    #:export (-> ->>))

(define-syntax ->>
  (syntax-rules ()
    ;; if e.g. (->> 3), simply return 3
    ((_ x) x)

    ;; if e.g. (->> 3 (+ 1 2)), ret (->> (+ 1 2 3))
    ((_ x (f args ...) rest ...)
     (->> (f args ... x) rest ...))

    ;; if e.g. (->> 3 square (+ 1)), ret (->> (+ 1 (square 3)))
    ((_ x f rest ...)
     (->> (f x) rest ...))))

(define-syntax ->
  (syntax-rules ()
    ;; if e.g. (-> 3), simply return 3
    ((_ x) x)

    ;; if e.g. (-> 3 (+ 1 2)), ret (-> (+ 3 1 2))
    ((_ x (f args ...) rest ...)
     (-> (f x args ...) rest ...))

    ;; if e.g. (-> 3 square (+ 1)), ret (-> (+ 1 (square 3)))
    ((_ x f rest ...)
     (-> (f x) rest ...))))
