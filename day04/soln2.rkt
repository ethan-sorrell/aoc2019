#lang racket

(define (splitDigits num)
  (let loop ((rem num)
               (acc '()))
    (if (= 0 rem) acc
        (loop (quotient rem 10) (cons (remainder rem 10) acc)))))

(define (validPassword pw)
  (let loop ((rem (splitDigits pw))
             (validRepeat #f)
             (inRepeat #f)
             (candidateRepeat #f))
    (cond [(null? (rest rem)) (or validRepeat candidateRepeat)]
          [(= (first rem) (second rem)) (loop (rest rem) validRepeat
                                              #t
                                              (xor #t inRepeat))]
          [(< (first rem) (second rem)) (loop (rest rem)
                                              (or validRepeat candidateRepeat)
                                              #f
                                              #f)]
          [else #f])))

(define (countPasswords min_range max_range)
  (length
   (for/list ([i (range (add1 min_range) max_range)]
         #:when (validPassword i))
     i)))

(define (solve2)
  (let* ([line (first (file->lines "input.txt"))]
         [vals (string-split line "-")]
         [min_range (string->number (first vals))]
         [max_range (string->number (second vals))])
    (println (countPasswords min_range max_range))))
