#lang racket
(require racket/format)

;;  take a program and program counter
;;  return a program and program counter

(define (paramMode instr num)
  (if (> (+ 2 num) (string-length instr)) 0
      (string-ref instr (- (string-length instr) (+ 2 num)))))

(define (getParam instr num pc tokens)
  (let* ([param (vector-ref tokens (+ pc num))]
         [paramMode (paramMode instr num)])
    (cond
      [(equal? #\1 paramMode) param]
      [(equal? #\0 paramMode) (vector-ref tokens (string->number param))]
      [else (begin (println (~a "Error: incorrect paramMode in instr "
                                instr " at " pc)) 0)])))

;; (define (getParam instr num pc tokens)
;;   (println pc)
;;   (println num)
;;   (println (+ pc num)))

(define (runInstr state)
  (let* ([pc (vector-ref state 0)]
         [tokens (vector-ref state 1)]
         [instr (~a (vector-ref tokens pc)
                    #:align 'right
                    #:min-width 5
                    #:left-pad-string (~a "0"))]
         [op (substring instr (- (string-length instr) 2))]
         [param1 (string->number (getParam instr 1 pc tokens))]
         [param2 (string->number (getParam instr 2 pc tokens))]
         [param3 (string->number (getParam instr 3 pc tokens))])
    (cond
      [(= (string->number op) 1)
       (begin (vector-set! tokens param3 (number->string (+ param1 param2)))
              (vector (+ pc 4) tokens))]
      [(= (string->number op) 2)
       (begin (vector-set! tokens param3 (number->string (* param1 param2)))
              (vector (+ pc 4) tokens))]
      [(= (string->number op) 3)
       (begin (vector-set! tokens param1 (number->string param1))
              (vector (+ pc 2) tokens))]
      [(= (string->number op) 4) (begin (println (vector-ref tokens param1))
                       (vector (+ pc 2) tokens))]
      [(= (string->number op) 99) tokens]
      [else (begin (println (~a "Error Unknown Op " op)))])))


(let* ([program (file->string "input.txt")]
       [program (substring program 0 (- (string-length program) 1))]
       [tokens (list->vector (string-split program ","))])
  (let loop ((state (vector 0 tokens)))
    (if (not (vector? state)) state
        (loop (runInstr state))
        ;; (runInstr state)
        )))
