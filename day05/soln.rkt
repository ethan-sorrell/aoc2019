#lang racket
(require racket/format)

(define (param-value instr num pc tokens)
  (let* ([param (vector-ref tokens (+ pc num))]
         [param-mode (remainder (quotient instr (expt 10 (add1 num))) 10)])
    (cond
      [(= 1 param-mode) param]                     ;; immediate mode
      [(= 0 param-mode) (vector-ref tokens param)] ;; position mode
      [else (begin (println (~a "Error: incorrect param-mode in instr "
                                instr " at " pc)) 0)])))

(define (run-instr state)
  (let* ([pc (vector-ref state 0)]
         [tokens (vector-ref state 1)]
         [instr (vector-ref tokens pc)]
         [op (remainder instr 100)])
    (cond
      [(= op 1) ;; add
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (+ param1 param2))
         (vector (+ pc 4) tokens))]
      [(= op 2) ;; multiply
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (* param1 param2))
         (vector (+ pc 4) tokens))]
      [(= op 3) ;; read from input
       (begin
         (vector-set! tokens (vector-ref tokens (add1 pc)) 5)
         (vector (+ pc 2) tokens))]
      [(= op 4) ;; write to output
       (begin
         (println (param-value instr 1 pc tokens))
         (vector (+ pc 2) tokens))]
      [(= op 5) ;; jump if true
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)])
         (if (not (= 0 param1))
             (vector param2 tokens)
             (vector (+ pc 3) tokens)))]
      [(= op 6) ;; jump if false
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)])
         (if (= 0 param1)
             (vector param2 tokens)
             (vector (+ pc 3) tokens)))]
      [(= op 7) ;; set on less than
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (if (< param1 param2)
             (vector-set! tokens param3 1)
             (vector-set! tokens param3 0))
         (vector (+ pc 4) tokens))]
      [(= op 8) ;; set on equal
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (if (= param1 param2)
             (vector-set! tokens param3 1)
             (vector-set! tokens param3 0))
         (vector (+ pc 4) tokens))]
      [(= op 99) tokens] ;; end program
      [else (begin (println (~a "Error Unknown Op " op
                                " pc " pc)))])))


(let* ([program (file->string "input.txt")]
       [program (substring program 0 (- (string-length program) 1))] ;; remove trailing newline
       [tokens (list->vector (string-split program ","))]
       [tokens (vector-map string->number tokens)])
  (let loop ((state (vector 0 tokens)))
    (if (not (= 2 (vector-length state))) state
        (loop (run-instr state)))))
