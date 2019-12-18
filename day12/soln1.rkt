#lang racket

(define (compute-gravity planet-idx other-idx coords) ;; update 2 velocities
  (map (lambda (planet-coord other-coord)
         (cond
           [(> planet-coord other-coord) -1]
           [(< planet-coord other-coord) 1]
           [else 0]))
       (list-ref coords planet-idx)
       (list-ref coords other-idx)))

(define (apply-gravity planet-idx coords vels)
  (let loop ((other-idx planet-idx)
             (new-vels vels))
    (if (= other-idx (length coords))
        new-vels
        (let* ([change (compute-gravity planet-idx other-idx coords)])
          (loop (add1 other-idx)
                (list-update
                 (list-update new-vels planet-idx
                              (lambda (x) (map + x change)))
                 other-idx
                 (lambda (y) (map - y change))))))))

(define (update-vels coords vels)       ;; return all velocities
  (let loop ((planet-idx 0)
             (new-vels vels))
    (if (= planet-idx (length coords))
        new-vels
        (loop
         (add1 planet-idx)
         (apply-gravity planet-idx coords new-vels)))))

(define (time-step coords vels)
  (let* ([new-vels (update-vels coords vels)]
         [new-coords (map (lambda (x y) (map + x y)) coords new-vels)])
    (cons new-coords new-vels)))

(define (run-n n)
  (let* ([lines (file->lines "input.txt")]
         [coord-pattern "<x=(-?[0-9]+), y=(-?[0-9]+), z=(-?[0-9]+)>"]
         [coords (map (lambda (line)
                        (map (lambda (str) (string->number str))
                             (rest (regexp-match coord-pattern line))))
                      lines)]
         [vels (for/list ([i (in-range (length coords))])
                 (list 0 0 0))])
    (let loop ((new-coords coords)
               (new-vels vels)
               (step 0))
      (let ([next-state (time-step new-coords new-vels)])
        (if (= step n) (cons new-coords new-vels)
            (loop (first next-state) (rest next-state) (add1 step)))))))

;; (run-n 100)

(define (solve1)
  (let* ([ending-state (run-n 1000)]
         [ending-coord (car ending-state)]
         [ending-vels (cdr ending-state)])
    (apply
     +
     (map *
          (map (lambda (x) (apply + (map abs x))) ending-coord)
          (map (lambda (x) (apply + (map abs x))) ending-vels)))))

(solve1)
