#lang racket

(define test-input 12345678)

(define (generate-pattern posn len)
  (define base-pattern (list 0 1 0 -1))
  (define multi-pattern
    (for*/list ([elt (in-list base-pattern)]
                [i (in-range (add1 posn))])
      elt))
  (define repeat-pattern
    (for/list ([i (in-range (add1 len))])
      (list-ref multi-pattern (remainder i (length multi-pattern)))))
  (drop repeat-pattern 1))

(define (signal->list signal)
  (let loop ((rem (quotient signal 10))
             (acc (list (remainder signal 10))))
    (if (= 0 rem)
        acc
        (loop (quotient rem 10) (cons (remainder rem 10) acc)))))

(define (output-element input-list posn)
  (define pattern (generate-pattern posn (length input-list)))
  (define dot-product
    (for/sum ([elt-in (in-list input-list)]
              [elt-pattern (in-list pattern)])
      (* elt-in elt-pattern)))
  (abs (remainder dot-product 10)))

(define (fft input-list)
  (define len (length input-list))
  (for/list ([i (in-range len)])
    (output-element2 input-list i)))

(define (output-element2 input-list posn)
  (define rep (add1 posn))
  (let loop ((rem input-list)
             (pattern -1)
             (acc 0))
    (when (> rep (length rem)) (set! rep (length rem)))
    (cond
      [(null? rem) (abs (remainder acc 10))]
      [(= pattern 0) (loop (drop rem rep) 1 acc)]
      [(= pattern 2) (loop (drop rem rep) 3 acc)]
      [(= pattern 1) (loop (drop rem rep) 2 (+ acc (apply + (take rem rep))))]
      [(= pattern 3) (loop (drop rem rep) 0 (- acc (apply + (take rem rep))))]
      [(= pattern -1) (loop (drop rem (- rep 1)) 1 acc)])))

(define (solve signal iters)
  (define input-list (signal->list signal))
  (define result
    (for/fold ([acc input-list])
              ([i (in-range iters)])
      (fft acc)))
  (println (apply string-append (map number->string (take result 8)))))

(define test-input2 03036732577212944063491565474664)

(define (solve2 signal iters)
  (define single-input-list (signal->list signal))
  (define input-list
    (for/fold ([acc '()])
              ([i (in-range 10000)])
      (append single-input-list acc)))
  (define result
    (for/fold ([acc input-list])
              ([i (in-range 1)])
      (fft acc)))
  (println (apply string-append (map number->string (take result 8)))))

(define input (file->string "input.txt"))
(define signal (string->number (string-trim input)))
;; (solve2 test-input2 1)
;; (time (solve signal 100))
