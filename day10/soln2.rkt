#lang racket

(define (lines->map lines)
  (foldl append '()
       (for/list ([y (range (length lines))])
         (map cons
              (map (lambda (x) (cons x y)) (range (string-length (list-ref lines y))))
              (string->list (list-ref lines y))))))

(define (slope-quadrant rise run)
  (cond
    [(and (>= rise 0) (>= run 0)) 1]
    [(and (>= 0 rise) (>= run 0)) 2]
    [(and (>= 0 rise) (>= 0 run)) 3]
    [(and (>= rise 0) (>= 0 run)) 4]
    [else (begin
            (println "Error!: Can't Determine Quadrant")
            (println rise)
            (println run))]))

(define (slope-less-than? a b)
  (let* ([a-rise (- (first a))]
         [a-run (second a)]
         [b-rise (- (first b))]
         [b-run (second b)]
         [a-quad (slope-quadrant a-rise a-run)]
         [b-quad (slope-quadrant b-rise b-run)])
    (cond
      [(< a-quad b-quad) #t]
      [(< b-quad a-quad) #f]
      [(or (= a-run 0) (= b-rise 0)) #t]
      [(or (= a-rise 0) (= b-run 0)) #f]
      [(> (/ a-rise a-run) (/ b-rise b-run)) #t]
      [(< (/ a-rise a-run) (/ b-rise b-run)) #f]
      [else #f])))

(define (all-slopes width height)
  (sort
   (filter (lambda (x) (= 1 (gcd (first x) (second x))))
           (cartesian-product (range (- width) (+ 1 width))
                              (range (- height) (+ 1 height))))
   slope-less-than?))

(define (shoot-laser comet-map x y slope)
  (let loop ((start-x x)
             (start-y y))
    (let* ([next-x (+ start-x (second slope))]
           [next-y (+ start-y (first slope))]
           [val (hash-ref comet-map (cons next-x next-y) #f)])
      (cond
        [(equal? val #f) #f]
        [(equal? val #\#) (cons next-x next-y)]
        [(equal? val #\.) (loop next-x next-y)]))))

(define (destroy-n comet-map x y width height n)
  (let loop ((destroyed 1)
             (laser-hit #f)
             (rem-map comet-map)
             (slopes (all-slopes width height)))
    (cond
      [(and laser-hit (= destroyed n)) laser-hit] ;; done
      [laser-hit (loop (add1 destroyed)             ;; hit
                       #f
                       (hash-set rem-map laser-hit #\.)
                       slopes)]
      [(null? slopes) (loop destroyed   ;; next rotation
                            #f
                            rem-map
                            (all-slopes width height))]
      [else (loop destroyed             ;; miss
                  (shoot-laser rem-map x y (first slopes))
                  rem-map
                  (rest slopes))])))

(let* ([lines (file->lines "input.txt")]
       [width (string-length (list-ref lines 0))]
       [height (length lines)]
       [comet-map (lines->map lines)]
       [comet-map (make-immutable-hash comet-map)])
  (destroy-n comet-map 11 11 width height 200))
