#lang racket

(require racket/hash)
(require "intcode_computer.rkt")

(define (run-until-output state)
  (cond
    [(not (list? state)) #f]
    [(= 4 (length state)) state]
    [else (run-until-output (run-instr state))]))

(define (translate-direction char)
  (case char
    [(#\n) 1]
    [(#\s) 2]
    [(#\w) 3]
    [(#\e) 4]))

(define (update-board board direction output)
  (define old-pos (hash-ref board "bot"))
  (define new-pos
    (case direction
      [(#\x) old-pos]
      [(#\n) (cons (car old-pos) (add1 (cdr old-pos)))]
      [(#\s) (cons (car old-pos) (sub1 (cdr old-pos)))]
      [(#\e) (cons (add1 (car old-pos)) (cdr old-pos))]
      [(#\w) (cons (sub1 (car old-pos)) (cdr old-pos))]))
  (case output
    [(1)                                ;; no obstacle
     (let ([new-board (hash-set board "bot" new-pos)])
       (hash-set new-board new-pos #\.))]
    [(0)                                ;; hit wall
     (hash-set board new-pos #\#)]
    [(2)                                ;; found ox-sys
     (let* ([new-board (hash-set board "bot" new-pos)]
            [new-board (hash-set new-board "goal" new-pos)])
       (hash-set new-board new-pos #\.))]))

(define (decide-direction direction obstacle?)
  (define dirs (list #\w #\n #\e #\s #\w #\n))
  (define index
    (case direction
      [(#\n) 1]
      [(#\e) 2]
      [(#\s) 3]
      [(#\w) 4]))
  (if obstacle?
      (list-ref dirs (add1 index))
      (list-ref dirs (sub1 index))))

(define (explore-area state
                      [board (hash "bot" (cons 0 -1))]
                      [direction #\n]
                      [output 1]
                      [goal #f])
  (cond
    [(and goal (equal? (cons 0 0) (hash-ref board "bot"))) board]
    [else
     (define new-direction (decide-direction direction (= output 1)))
     (match-define (cons new-output new-state)
       (run-until-output (change-input state (translate-direction new-direction))))
     (explore-area new-state
                   (update-board board direction output)
                   new-direction
                   new-output
                   (or goal (= output 2)))]))

(define (show-board board)
  (define new-board
    (let* ([goal-pos (hash-ref board "goal")]
           [new-board (hash-remove board "bot")]
           [new-board (hash-set (hash-remove new-board "goal") goal-pos #\O)])
      (hash-set new-board (cons 0 0) #\B)))
  (define x-min (apply min (map car (hash-keys new-board))))
  (define x-max (apply max (map car (hash-keys new-board))))
  (define y-min (apply min (map cdr (hash-keys new-board))))
  (define y-max (apply max (map cdr (hash-keys new-board))))
  (for ([row (range y-max (sub1 y-min) -1)])
    (println
     (list->string
      (for/list ([col (range x-min (add1 x-max))])
        (hash-ref new-board (cons col row) #\space))))))

(define (adjacent-nodes nodes)
  (for*/set ([node (in-list nodes)]
             [dir (in-list (list (cons 0 1) (cons 0 -1) (cons 1 0) (cons -1 0)))])
    (cons (+ (car node) (car dir)) (+ (cdr node) (cdr dir)))))

(define (dijkstra board)
  (define start (hash-ref board "goal"))
  (define (extend-graph frontier weights distance)
    (for/fold ([new-frontier (hash)])
              ([node (in-set (adjacent-nodes (hash-keys frontier)))])
              (if (or (hash-has-key? weights node)
                      (equal? #\# (hash-ref board node #\#))) new-frontier
                  (hash-set new-frontier node (add1 distance)))))
  (let loop ((weights (hash))
             (frontier (hash start 0))
             (distance 0))
    (if (hash-empty? frontier)
        (values
         (hash-ref weights (cons 0 0))
         (apply max (hash-values weights)))
        (let* ([new-frontier (extend-graph frontier weights distance)]
               [new-weights (hash-union weights new-frontier)])
          (loop new-weights new-frontier (add1 distance))))))

(define program
  (let ([file (file->string "input.txt")])
    (substring file 0 (- (string-length file) 1))))

(define mem
  (let* ([memlist (string-split program ",")]
         [memlist (map string->number memlist)]
         [mempairs (map cons (range (length memlist)) memlist)])
    (make-immutable-hash mempairs)))

(define board (explore-area (list 0 mem)))

(show-board board)
(dijkstra board)
