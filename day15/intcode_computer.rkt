#lang racket

(provide run-instr change-input)

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
                 (mem-set mem relative-base input)
                 input)
           (list (+ pc 2)
                 (mem-set mem (mem-ref mem (add1 pc)) input)
                 input))]
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

(define (change-input state input)
  (append (take state 2) (list input)))
