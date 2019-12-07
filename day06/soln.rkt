#lang racket

(require racket/set)

(define (add-orbit phrase orbits)
  (let ([object (first phrase)]
        [satellite (second phrase)])
    (hash-update orbits object (lambda (x) (cons satellite x)) '())))

(define (parse-orbits tokenlists)
  (let loop ((rem tokenlists)
             (acc (hash)))
    (if (null? rem) acc
        (loop (rest rem) (add-orbit (first rem) acc)))))

(define (calculate-orbits orbits)
  (let loop ((curr-planet "COM")
             (upstream 0))
    (let ([satellites (hash-ref orbits curr-planet '())])
      (if (null? satellites) upstream
          (+ upstream
             (apply + (map (lambda (x) (loop x (add1 upstream))) satellites)))))))

(define (parse-dlink tokenlists)
  (let loop ((rem tokenlists)
             (acc (hash)))
    (if (null? rem) acc
        (loop (rest rem) (add-orbit (first rem) (add-satellite (first rem) acc))))))

(define (add-satellite phrase orbits)
  (let ([object (second phrase)]
        [satellite (first phrase)])
    (hash-update orbits object (lambda (x) (cons satellite x)) '())))

(define (unvisited-adjacent table parents visited)
  (let loop ((acc (set))
             (rem parents))
    (if (set-empty? rem) (set-subtract acc visited)
        (loop
         (set-union acc (list->set (hash-ref table (set-first rem) '())))
         (set-rest rem)))))

;; current-level list of nodes
;; check if target is in current-level
;; otherwise, add all unvisited from current level
;; need to maintain: current-level, visited, distance

(define (min-distance table from to)
  (let loop ((current-level (set from))
             (visited (set))
             (distance 0))
    (if (set-member? current-level to) distance
        (loop
         (unvisited-adjacent table current-level visited)
         (set-union visited current-level)
         (add1 distance)))))

(let* ([lines (file->lines "input.txt")]
       [tokenlists (map (lambda (x) (string-split x ")")) lines)]
       [orbits (parse-orbits tokenlists)]
       [dlink-table (parse-dlink tokenlists)])
  (println (calculate-orbits orbits))
  (println (- (min-distance dlink-table "SAN" "YOU") 2)))
