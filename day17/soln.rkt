#lang racket

(require "intcode_computer.rkt")

(define mem (file->mem "input.txt"))

(define (read-camera)
  (let loop ((state (change-input (list 0 mem) 0))
             (camera-feed (set))
             (row 0)
             (col 0))
    (define new-state (run-instr state))
    (cond
      [(hash? new-state) camera-feed]
      [(= 3 (length new-state)) (loop new-state camera-feed row col)]
      [(= 10 (first new-state))         ;; newline
       (loop (rest new-state) camera-feed (add1 row) 0)]
      [(= 35 (first new-state))         ;; manifold
       (loop (rest new-state)
             (set-add camera-feed (cons row col))
             row
             (add1 col))]
      [else
       (loop (rest new-state) camera-feed row (add1 col))])))

(define (list-intersections camera-feed)
  (define (intersection? point)
    (match-define (cons row col) point)
    (andmap (lambda (x) (set-member? camera-feed x))
                  (list (cons row (add1 col))
                        (cons row (sub1 col))
                        (cons (add1 row) col)
                        (cons (sub1 row) col))))
  (for/list ([point (in-set camera-feed)]
             #:when (intersection? point))
    point))


(define (solve1)
  (define intersections (list-intersections (read-camera)))
  (for/sum ([point (in-list intersections)])
    (* (car point) (cdr point))))

(solve1)
