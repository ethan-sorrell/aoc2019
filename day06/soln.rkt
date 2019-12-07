#lang racket

(define (add-orbit phrase orbits)
  (let ([object (first phrase)]
        [satellite (second phrase)])
    (hash-update orbits object (lambda (x) (cons satellite x)) '())))

(define (parse-orbits tokenlists)
  (let loop ((rem tokenlists)
             (acc (hash)))
    (if (null? rem) acc
        (loop (rest rem) (add-orbit (first rem) acc)))))

(define (calculate-orbits orbits)
  (let loop ((curr-planet "COM")
             (upstream 0))
    (let ([satellites (hash-ref orbits curr-planet '())])
      (if (null? satellites) upstream
          (+ upstream
             (apply + (map (lambda (x) (loop x (add1 upstream))) satellites)))))))

(let* ([lines (file->lines "input.txt")]
       [tokenlists (map (lambda (x) (string-split x ")")) lines)]
       [orbits (parse-orbits tokenlists)])
  (calculate-orbits orbits)
  ;; orbits
  )
