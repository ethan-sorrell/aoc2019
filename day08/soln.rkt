#lang racket

(define (solve1)
  (let* ([file (file->string "input.txt")]
         [file (substring file 0 (- (string-length file) 1))]
         [layers (for/list [(i (range (quotient (string-length file) (* 25 6))))]
                   (substring file (* 25 6 i) (* 25 6 (+ 1 i))))]
         [zero-counts (map (lambda (x) (length (filter
                                                (lambda (y)
                                                  (equal? y #\0)) (string->list x))))
                           layers)]
         [layer (argmin (lambda (x) (length (filter (lambda (y) (equal? y #\0))
                                                    (string->list x))))
                        layers)])
    (* (length (filter (lambda (x) (equal? #\2 x)) (string->list layer)))
       (length (filter (lambda (x) (equal? #\1 x)) (string->list layer))))))

(define (layers->positions layers)
  (map list->string
       (for/list ([i (range (string-length (first layers)))])
         (map (lambda (x) (string-ref x i)) layers))))

(define (occlude layers)
  (findf (lambda (x) (< (char->integer x) 50)) (string->list layers)))

(define (solve2)
  (let* ([file (file->string "input.txt")]
         [file (substring file 0 (- (string-length file) 1))]
         [layers (for/list [(i (range (quotient (string-length file) (* 25 6))))]
                   (substring file (* 25 6 i) (* 25 6 (+ 1 i))))]
         [positions (layers->positions layers)]
         [top (list->string
               (map (lambda (x) (if (equal? x #\0) #\_ x))
                    (map occlude positions)))]
         [lines (for/list [(i (range (quotient (string-length top) 25)))]
                  (substring top (* 25 i) (* 25 (+ 1 i))))])
    (for ([line (in-list lines)])
      (println line))))

(solve1)
(solve2)
