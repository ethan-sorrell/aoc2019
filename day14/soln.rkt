#lang racket
(require racket/pretty)

(struct supply (type multi) #:transparent)

(define (parse-supply str)
  (match-define (list multi type) (string-split str))
  (supply type (string->number multi)))

;; ("FUEL" . (list 1 (supply "something" 3) (supply "ab" 4)
(define (parse-grammar lines)
  (for/hash ([line (in-list lines)])
    (match-define (list inputs output) (string-split line "=>"))
    (match-define (list output-multi output-type) (string-split output))
    (values output-type (cons (string->number output-multi)
                              (map parse-supply (string-split inputs ", "))))))

(define (walk-material grammar supply-out [surplus (hash)] [acc-ore 0]) ;; (cons n-ore surplus)
  (cond
    [(equal? (supply-type supply-out) "ORE") ;; we finished our walk (at ore)
     (cons (+ acc-ore (supply-multi supply-out)) surplus)]
    [(>= (hash-ref surplus (supply-type supply-out) 0) ;; we have enough surplus available
         (supply-multi supply-out))
     (cons acc-ore (hash-update surplus
                                (supply-type supply-out)
                                (lambda (x) (- x (supply-multi supply-out)))))
     ]
    [(> (hash-ref surplus (supply-type supply-out) 0) 0) ;; we have some surplus and should recur with none
     (define usable-surplus (hash-ref surplus (supply-type supply-out)))
     (define remaining-multi (- (supply-multi supply-out) usable-surplus))
     (walk-material grammar
                    (struct-copy supply supply-out [multi remaining-multi])
                    (hash-remove surplus (supply-type supply-out))
                    acc-ore)]
    [else                               ;; need to order new materials
     (match-define (cons n-created supply-in) (hash-ref grammar (supply-type supply-out)))
     (define n-rxns (ceiling (/ (supply-multi supply-out) n-created)))
     (define supply-required
       (map (lambda (x) (struct-copy supply x [multi (* n-rxns (supply-multi x))]))
            supply-in))
     (for/fold ([n-ore acc-ore]
                [surplus surplus]
                #:result (cons n-ore surplus))
               ([supply-req (in-list supply-required)])
       (match-define (cons add-ore new-surplus)
         (if (= 0 (modulo (supply-multi supply-out) n-created))
             (walk-material grammar supply-req surplus acc-ore)
             (walk-material grammar
                            supply-req
                            (hash-set surplus
                                      (supply-type supply-out)
                                      (- (* n-created n-rxns) (supply-multi supply-out)))
                            acc-ore)))
       (values (+ add-ore n-ore) new-surplus))]))

(define lines (file->lines "input.txt"))
(define grammar (parse-grammar lines))
(define ans (car (walk-material grammar (supply "FUEL" 1))))

(println ans)
