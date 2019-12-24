#lang racket

(require "intcode_computer.rkt")

(define (run-until-output state)
  (cond
    [(not (list? state)) #f]
    [(= 4 (length state)) state]
    [else (run-until-output (run-instr state))]))

(define (translate-direction char)
  (cond [(equal? char #\n) 1]
        [(equal? char #\s) 2]
        [(equal? char #\w) 3]
        [(equal? char #\e) 4]))

(define (update-board board direction)
  ;; FIXME
  (hash))

(define (decide-direction board direction)
  ;; FIXME
  #\n)

;; TODO: decide on a navigation strategy
(define (find-oxsys state [board (hash)] [direction #\w] [output 1])
  (cond
    [(= output 2) board]                ;; done
    [(= output 1)                       ;; we can continue
     (match-define (cons new-output new-state)
       (run-until-output (change-input state (translate-direction direction))))
     (find-oxsys new-state (update-board board direction) direction new-output)]
    [(= output 0)                       ;; we hit a wall
     (define new-direction (decide-direction board direction))
     (match-define (cons new-output new-state)
       (run-until-output (change-input state (translate-direction new-direction))))
     (find-oxsys state
                 (update-board board new-direction)
                 new-direction
                 new-output)]))

(define program
  (let ([file (file->string "input.txt")])
    (substring file 0 (- (string-length file) 1))))

(define mem
  (let* ([memlist (string-split program ",")]
         [memlist (map string->number memlist)]
         [mempairs (map cons (range (length memlist)) memlist)])
    (make-immutable-hash mempairs)))
