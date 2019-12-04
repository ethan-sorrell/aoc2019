#lang racket

(define directions
  (hash #\U #(0 1)
        #\D #(0 -1)
        #\R #(1 0)
        #\L #(-1 0)))

(define (hash-add-point point h)
  (hash-set h (vector-take point 2) (vector-ref point 2)))

(define (tokens->points tokens)
  (let loop ((remTokens tokens)
             (accPoints (hash))
             (startPoint #(0 0 0)))
    (if (null? remTokens) accPoints
        (let* ([token (car remTokens)]
               [dir (hash-ref directions (string-ref token 0))]
               [dirx (vector-ref dir 0)]
               [diry (vector-ref dir 1)]
               [startx (vector-ref startPoint 0)]
               [starty (vector-ref startPoint 1)]
               [startstep (vector-ref startPoint 2)]
               [norm (string->number (substring token 1))])
          (loop (cdr remTokens)
                (foldl hash-add-point accPoints
                        (for/list ([i (in-range 1 (add1 norm))])
                          (vector (+ startx (* i dirx))
                                  (+ starty (* i diry))
                                  (+ startstep i))))
                (vector (+ startx (* norm dirx))
                        (+ starty (* norm diry))
                        (+ startstep norm)))))))

(define (distance point)
  (let ([x (vector-ref point 0)]
        [y (vector-ref point 1)])
    (+ (abs x) (abs y))))

(define (delay point wire1 wire2)
  (+ (hash-ref wire1 point)
     (hash-ref wire2 point)))

(define (solve1)
  (let* ([lines (file->lines "input.txt")]
         [tokenlists (map (lambda (x) (string-split x ",")) lines)]
         [wire1tokens (list-ref tokenlists 0)]
         [wire2tokens (list-ref tokenlists 1)]
         [wire1points (tokens->points wire1tokens)]
         [wire2points (tokens->points wire2tokens)])
    (apply min
     (for/list ([point (in-hash-keys wire2points)]
           #:when (hash-has-key? wire1points point))
       (distance point)))))

(define (solve2)
  (let* ([lines (file->lines "input.txt")]
         [tokenlists (map (lambda (x) (string-split x ",")) lines)]
         [wire1tokens (list-ref tokenlists 0)]
         [wire2tokens (list-ref tokenlists 1)]
         [wire1points (tokens->points wire1tokens)]
         [wire2points (tokens->points wire2tokens)])
    (apply min
           (for/list ([point (in-hash-keys wire2points)]
                      #:when (hash-has-key? wire1points point))
             (delay point wire1points wire2points)))))
