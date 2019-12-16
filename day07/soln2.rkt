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
  (let* ([pc (list-ref state 0)]
         [tokens (list-ref state 1)]
         [input (list-ref state 2)]
         [instr (vector-ref tokens pc)]
         [op (remainder instr 100)])
    (cond
      [(= op 1) ;; add
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (+ param1 param2))
         (list (+ pc 4) tokens input))]
      [(= op 2) ;; multiply
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (vector-set! tokens param3 (* param1 param2))
         (list (+ pc 4) tokens input))]
      [(= op 3) ;; read from input
       (begin
         (vector-set! tokens (vector-ref tokens (add1 pc)) (first input))
         (list (+ pc 2) tokens (rest input)))]
      [(= op 4) ;; write to output
       (list (param-value instr 1 pc tokens) (+ pc 2) tokens input)
       #;(begin
         (println (~a "output:" (param-value instr 1 pc tokens)))
         (list (param-value instr 1 pc tokens) (+ pc 2) tokens input))]
      [(= op 5) ;; jump if true
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)])
         (if (not (= 0 param1))
             (list param2 tokens input)
             (list (+ pc 3) tokens input)))]
      [(= op 6) ;; jump if false
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)])
         (if (= 0 param1)
             (list param2 tokens input)
             (list (+ pc 3) tokens input)))]
      [(= op 7) ;; set on less than
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (if (< param1 param2)
             (vector-set! tokens param3 1)
             (vector-set! tokens param3 0))
         (list (+ pc 4) tokens input))]
      [(= op 8) ;; set on equal
       (let ([param1 (param-value instr 1 pc tokens)]
             [param2 (param-value instr 2 pc tokens)]
             [param3 (vector-ref tokens (+ pc 3))])
         (if (= param1 param2)
             (vector-set! tokens param3 1)
             (vector-set! tokens param3 0))
         (list (+ pc 4) tokens input))]
      [(= op 99) tokens] ;; end program
      [else (begin (println (~a "Error Unknown Op " op
                                " pc " pc)))])))

(define (start-acs phase-setting amp-input)
  (let* ([program (file->string "input.txt")]
         [program (substring program 0 (- (string-length program) 1))] ;; remove trailing newline
         [tokens (list->vector (string-split program ","))]
         [tokens (vector-map string->number tokens)])
    (let loop ((state (list 0 tokens (list phase-setting amp-input))))
      (if (not (and (list? state)
                    (= 3 (length state))))
          state
          (loop (run-instr state))))))

(define (run-acs state inputs)
  (let loop ((state (append state (list inputs))))
    (if (not (and (list? state)
                  (= 3 (length state))))
        state
        (loop (run-instr state)))))

(define (run-series a b c d e)
  (let* ([output-a (start-acs a 0)]
         [output-b (start-acs b (first output-a))]
         [output-c (start-acs c (first output-b))]
         [output-d (start-acs d (first output-c))]
         [output-e (start-acs e (first output-d))]
         [state-a (rest output-a)]
         [state-b (rest output-b)]
         [state-c (rest output-c)]
         [state-d (rest output-d)]
         [state-e (rest output-e)])
    (let loop ((input (first output-e))
               (state-1 state-a)
               (state-2 state-b)
               (state-3 state-c)
               (state-4 state-d)
               (state-5 state-e))
      (let ([outputs (run-acs (take state-1 2) (list input)
                              #;(append (list-ref state-1 2) (list input)))])
        (if (not (list? outputs)) input
            (loop (first outputs)
                  state-2 state-3 state-4 state-5
                  (rest outputs)))))))

(define (configure-thrusters)
  (let ([max 0])
    (for*/last ([settings (permutations (range 5 10))]
                #:when (> (apply run-series settings) max))
      (begin
        (set! max (apply run-series settings))
        max))))

(configure-thrusters)

;; (define (foo inputs state-a state-b state-c state-d state-e)
;;   (foo (run state-a) state-b state-c state-d state-e state-a)
