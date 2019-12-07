#lang racket
(require racket/format)

(define (get-param instr num pc tokens)
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
      [(= op 1)
       (let ([param1 (get-param instr 1 pc tokens)]
             [param2 (get-param instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (+ param1 param2))
         (vector (+ pc 4) tokens))]
      [(= op 2)
       (let ([param1 (get-param instr 1 pc tokens)]
             [param2 (get-param instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (* param1 param2))
         (vector (+ pc 4) tokens))]
      [(= op 3)
       (begin
         (vector-set! tokens (vector-ref tokens (add1 pc)) 1)
         (vector (+ pc 2) tokens))]
      [(= op 4) (begin
                  (println (get-param instr 1 pc tokens))
                  (vector (+ pc 2) tokens))]
      [(= op 99) tokens]
      [else (begin (println (~a "Error Unknown Op " op
                                " pc " pc)))])))


(let* ([program (file->string "input.txt")]
       [program (substring program 0 (- (string-length program) 1))] ;; remove trailing newline
       [tokens (list->vector (string-split program ","))]
       [tokens (vector-map string->number tokens)])
  (let loop ((state (vector 0 tokens)))
    (if (not (= 2 (vector-length state))) state
        (loop (run-instr state)))))
