#lang racket

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
  (*
   (length (filter (lambda (x) (equal? #\2 x)) (string->list layer)))
   (length (filter (lambda (x) (equal? #\1 x)) (string->list layer)))))
