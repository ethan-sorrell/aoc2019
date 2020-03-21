#lang racket

(define input (file->lines "input.txt"))

(define (parse-grid input)
 (for/fold ([item-locs (hash)]
            [traversable (hash)])
           ([line (in-list input)]
            [i (range +inf.0)]
            #:when #t
            [char (in-list (string->list line))]
            [j (range +inf.0)]
            #:when (not (equal? char #\#)))
   (values
    (if (equal? #\. char) item-locs (hash-set item-locs char (cons i j)))
    (if (char-upper-case? char)
        (hash-set traversable (cons i j) char)
        (hash-set traversable (cons i j) #t)))))

(define (adjacent traversable point distance new-keys-required)
  (match-define (cons x y) point)
  (for/list ([point (in-list (list (cons (add1 x) y)
                                   (cons (sub1 x) y)
                                   (cons x (add1 y))
                                   (cons x (sub1 y))))]
             #:when (hash-ref traversable point #f))
    (list point (add1 distance) new-keys-required)))

(define (hash-swap h)
  (for/hash ([(key val) (in-hash h)])
    (values val key)))

(define (crawl-maze traversable item-locs from)
  (set! item-locs (hash-remove (hash-swap item-locs) from))
  (define (bfs queue result-table)
    (cond
      [(empty? queue) result-table]
      [else
       (match-define (list point distance keys-required) (first queue))
       (cond
         [(hash-ref result-table point #f) (bfs (rest queue) result-table)]
         [else
          (define new-keys-required
            (if (char-upper-case? (hash-ref item-locs point #\a))
                (set-add keys-required (char-downcase (hash-ref item-locs point)))
                keys-required))
          (bfs (append (rest queue)
                       (adjacent traversable point distance new-keys-required))
               (hash-set result-table point (cons distance new-keys-required)))])]))
  (define distances (bfs (list (list from 0 (set))) (hash)))
  (for/hash ([(point distance) (in-hash distances)]
             #:when (not (char-upper-case? (hash-ref item-locs point #\A))))
    (values (hash-ref item-locs point) distance)))

;; create graph, as hashmap of hashmap of distances
;; key -> (key -> (keys-required . distance))
(define (create-graph traversable item-locs)
  (for/fold ([graph (hash)])
            ([(from from-loc) (in-hash item-locs)]
             #:when (not (char-lower-case? from))
             [(to to-loc) (in-hash item-locs)]
             #:when (and (not (char-lower-case? to))
                         (not (equal? from to))))
    (hash-set graph (cons from to) 1)))

(define (mst graph-nodes)
  (define keys-required
    (list->set (hash-keys (hash-remove graph-nodes #\@))))
  (let loop ((loc #\@)
             (keys-collected (set #\@))
             (distance 0))
    (cond
      [(subset? keys-required keys-collected) distance]
      [else
       (define edges (hash-ref graph-nodes loc))
       (define accessible-nodes
         (for/set ([(location value) (in-hash edges)]
                   #:when (and (not (set-member? keys-collected location))
                               (subset? (cdr value) keys-collected)))
           location))
       (for/list ([node (in-set accessible-nodes)])
         (loop node
               (set-add keys-collected node)
               (+ distance (car (hash-ref edges node)))))])))

;; (create-graph traversable item-locs)
(define-values (item-locs traversable) (parse-grid input))

(define (dynamic-solution nodes)
  (define cache (make-hash))
  (define all-keys (set-remove (list->set (hash-keys nodes)) #\@))
  (define (collect-keys-distance current-key remaining-keys)
    ;; any keys other than remaining-keys are assumed to have been collected
    (cond
      [(set-empty? remaining-keys) 0]
      [(hash-ref cache (cons current-key remaining-keys) #f)
       (hash-ref cache (cons current-key remaining-keys))]
      [else
       (define node (hash-ref nodes current-key))
       (define reachable-keys
         (for/set ([(key value) (in-hash node)]
                   #:when (and (set-member? remaining-keys key)
                               (set-empty? (set-intersect (cdr value) remaining-keys))))
           key))
       (define min-distance
         (for/fold ([result +inf.0])
                   ([key (in-set reachable-keys)])
           (min result
                (+ (car (hash-ref node key))
                   (collect-keys-distance key (set-remove remaining-keys key))))))
       (hash-set! cache (cons current-key remaining-keys) min-distance)
       min-distance]))
  (collect-keys-distance #\@ all-keys))

(define nodes
  (for/hash ([(item location) (in-hash item-locs)]
             #:when (not (char-upper-case? item)))
    (values
     item
     (crawl-maze traversable item-locs (hash-ref item-locs item)))))

(time (dynamic-solution nodes))
