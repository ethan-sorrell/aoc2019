#lang racket

(define (lines->map lines)
  (foldl append '()
       (for/list ([y (range (length lines))])
         (map cons
              (map (lambda (x) (cons x y)) (range (string-length (list-ref lines y))))
              (string->list (list-ref lines y))))))

(define (all-slopes width height)
  (filter (lambda (x) (= 1 (gcd (first x) (second x))))
          (cartesian-product (range (- width) (+ 1 width))
                             (range (- height) (+ 1 height)))))

(define (try-slope comet-map x y slope)
  (let loop ((start-x x)
             (start-y y))
    (let* ([next-x (+ start-x (second slope))]
           [next-y (+ start-y (first slope))]
           [val (hash-ref comet-map (cons next-x next-y) #f)])
      (cond
        [(equal? val #f) 0]
        [(equal? val #\#) 1]
        [(equal? val #\.) (loop next-x next-y)]))))

(define (check-position comet-map x y width height)
  (let loop ((acc '())
             (slopes (all-slopes width height)))
    (if (null? slopes) acc
        (loop (cons (try-slope comet-map x y (first slopes)) acc)
              (rest slopes)))))

(let* ([lines (file->lines "input.txt")]
       [width (string-length (list-ref lines 0))]
       [height (length lines)]
       [comet-map (lines->map lines)]
       [comet-map (make-immutable-hash comet-map)])
  (apply max
         (for*/list ([x (range width)]
                     [y (range height)]
                     #:when (equal? #\# (hash-ref comet-map (cons x y) #f)))
           (apply + (check-position comet-map x y width height)))))
