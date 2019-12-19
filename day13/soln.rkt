#lang racket
(require racket/format)

(define relative-base 0)

(define (mem-set mem pos val)
  (hash-set mem pos val))

(define (mem-ref mem pos)
  (if (hash-has-key? mem pos) (hash-ref mem pos) 0))

(define (param-value instr num pc mem)
  (let ([param (mem-ref mem (+ pc num))]
        [param-mode (remainder (quotient instr (expt 10 (add1 num))) 10)])
    (cond
      [(= 1 param-mode) param]                                 ;; immediate mode
      [(= 0 param-mode) (mem-ref mem param)]                   ;; position mode
      [(= 2 param-mode) (mem-ref mem (+ relative-base param))] ;; relative mode
      [else (begin (println (~a "Error: incorrect param-mode in instr "
                                instr " at " pc)) 0)])))

(define (write-location instr num pc mem)
  (let ([param-mode (remainder (quotient instr (expt 10 (add1 num))) 10)]
        [param (mem-ref mem (+ pc num))])
    (cond
      [(= 0 param-mode) param]                   ;; position mode
      [(= 2 param-mode) (+ relative-base param)] ;; relative-mode
      [else (begin (println (~a "Error: incorrect param-mode in instr "
                                instr " at " pc)) 0)])))

(define (run-instr program-state)
  (let* ([pc (list-ref program-state 0)]
         [mem (list-ref program-state 1)]
         [input (list-ref program-state 2)]
         [instr (mem-ref mem pc)]
         [op (remainder instr 100)])
    (cond
      [(= op 1) ;; add
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)]
             [param3 (write-location instr 3 pc mem)])
         (list (+ pc 4) (mem-set mem param3 (+ param1 param2)) input))]
      [(= op 2) ;; multiply
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)]
             [param3 (write-location instr 3 pc mem)])
         (list (+ pc 4) (mem-set mem param3 (* param1 param2)) input))]
      [(= op 3) ;; read from input
       (if (= 2 (quotient instr 100))
           (list (+ pc 2)
                 (mem-set mem relative-base (first input))
                 (rest input))
           (list (+ pc 2)
                 (mem-set mem (mem-ref mem (add1 pc)) (first input))
                 (rest input)))]
      [(= op 4) ;; write to output
       (begin
         ;; (println (param-value instr 1 pc mem))
         (list (param-value instr 1 pc mem) (+ pc 2) mem input))]
      [(= op 5) ;; jump if true
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)])
         (if (not (= 0 param1))
             (list param2 mem input)
             (list (+ pc 3) mem input)))]
      [(= op 6) ;; jump if false
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)])
         (if (= 0 param1)
             (list param2 mem input)
             (list (+ pc 3) mem input)))]
      [(= op 7) ;; set on less than
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)]
             [param3 (write-location instr 3 pc mem)])
         (if (< param1 param2)
             (list (+ pc 4) (mem-set mem param3 1) input)
             (list (+ pc 4) (mem-set mem param3 0) input)))]
      [(= op 8) ;; set on equal
       (let ([param1 (param-value instr 1 pc mem)]
             [param2 (param-value instr 2 pc mem)]
             [param3 (write-location instr 3 pc mem)])
         (if (= param1 param2)
             (list (+ pc 4) (mem-set mem param3 1) input)
             (list (+ pc 4) (mem-set mem param3 0) input)))]
      [(= op 9) ;; relative base offset
       (let ([param1 (param-value instr 1 pc mem)])
         (set! relative-base (+ relative-base param1))
         (list (+ pc 2) mem input))]
      [(= op 99) mem] ;; end program
      [else (begin (println (~a "Error Unknown Op " op
                                " pc " pc)))])))

;; (define (draw-board board)
;;   (let ([right (apply max (map car (hash-keys board)))]
;;         [left (apply min (map car (hash-keys board)))]
;;         [top (apply max (map cdr (hash-keys board)))]
;;         [bottom (apply min (map cdr (hash-keys board)))])
;;     (for ([y (in-range top (+ -1 bottom) -1)])
;;       (println
;;        (list->string
;;         (map (lambda (x) (if (= x 0) #\_ #\H))
;;              (for/list ([x (in-range left (add1 right))])
;;                (hash-ref board (cons x y) 0))))))))

(define (compute-board)
  (let* ([program (file->string "input.txt")]
         [program (substring program 0 (- (string-length program) 1))] ;; remove trailing newline
         [memlist (string-split program ",")]
         [memlist (map string->number memlist)]
         [mempairs (map cons (range (length memlist)) memlist)]
         [mem (make-immutable-hash mempairs)])
    (let loop ((program-state (list 0 mem '()))
               (game-state (hash))
               (next-tile '()))
      (cond
        [(not (list? program-state)) game-state
         ]
        [(and (= 4 (length program-state)) (= 2 (length next-tile))) ;; draw tile
         (loop (cdr program-state)
               (hash-set game-state next-tile (car program-state))
               '())]
        [(= 4 (length program-state))   ;; read tile output
         (loop (cdr program-state)
               game-state
               (append next-tile (list (car program-state))))]
        [else                           ;; no output
         (loop (run-instr program-state)
               game-state
               next-tile)]))))

(define (solve1) (println (count (lambda (x) (= x 2)) (hash-values (compute-board)))))

(define (draw-board board)
  (let ([right (apply max (map car (hash-keys board)))]
        [left (apply min (map car (hash-keys board)))]
        [top (apply max (map second (hash-keys board)))]
        [bottom (apply min (map second (hash-keys board)))])
    (for ([y (in-range top (+ -1 bottom) -1)])
      (println
       (apply string-append
              (map number->string
                   (for/list ([x (in-range left (add1 right))])
                     (hash-ref board (list x y) 0))))))))

(define (find-tile game-state tile)
  (let ([match (findf (lambda (x) (= tile (cdr x))) (hash->list game-state))])
    (if match (car (car match)) 0)))

(define destroyed 0)

(define (solve2)
  (let* ([program (file->string "input.txt")]
         [program (substring program 0 (- (string-length program) 1))] ;; remove trailing newline
         [memlist (string-split program ",")]
         [memlist (map string->number memlist)]
         [mempairs (map cons (range (length memlist)) memlist)]
         [mem (make-immutable-hash mempairs)]
         [mem (mem-set mem 0 2)])
    (let loop ((program-state (list 0 mem '(0)))
               (game-state (hash))
               (next-tile '()))
      (let*  ([ball-x (find-tile game-state 4)]
              [paddle-x (find-tile game-state 3)]
              [input (cond [(> ball-x paddle-x) (list 1)]
                           [(< ball-x paddle-x) (list -1)]
                           [else (list 0)])])
        (cond
          [(not (list? program-state)) (println "Done!")]
          [(and (= 4 (length program-state)) (equal? next-tile '(-1 0))) ;; read score
           (begin
             (set! destroyed (add1 destroyed))
             (println (~a "destroyed: " destroyed  " score: " (first program-state)))
             (loop (rest program-state) game-state '()))]
          [(and (= 4 (length program-state)) (= 2 (length next-tile))) ;; draw tile
           (loop (cdr program-state)
                 (hash-set game-state next-tile (car program-state))
                 '())]
          [(= 4 (length program-state))   ;; read tile output
           (loop (cdr program-state)
                 game-state
                 (append next-tile (list (car program-state))))]
          [else                           ;; no output
           (loop (run-instr (append (take program-state 2) (list input)))
                 game-state
                 next-tile)])))))

;; (solve1)
;; (draw-board (compute-board))
(solve2)
