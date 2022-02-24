#lang racket

(require racket/struct)

; TODO figure out a good value for max-len, currently 10 for easy testing
(define leaf-node-split-len 10)
(define leaf-node-join-len (quotient leaf-node-split-len 2))


; TODO rebuild (using get-substring-from-rope to save space)
; TODO rewrite str->rope using substrings instead of splitting?


; NODE
; a node value is either a string if it's a leaf node, or an integer
; representing the length of the left branch if it's not. Leaf nodes have empty
; left and right braches and we only care about the string they contain
(struct node (value left right)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (node) (if (leaf-node? node) 'leaf-node 'node))
      (lambda (node)
        (if (leaf-node? node)
            (list (node-value node))
            (list (node-value node) (node-left node) (node-right node))))))])

(define leaf-node
  (lambda (str) (node (string->immutable-string str) empty empty)))

(define leaf-node? (lambda (node) (string? (node-value node))))


(define str->rope
  (lambda (str)
    (cond
      [(null? str) (leaf-node "")]
      [(< (string-length str) leaf-node-split-len) (leaf-node str)]
      [else
       (let ([halfway-point (quotient (string-length str) 2)])
         (node halfway-point
               (str->rope (substring str 0 halfway-point))
               (str->rope (substring str halfway-point))))])))


(define rope->str
  (lambda (rope) (get-substring-from-rope rope 0 (rope-length rope))))


(define get-substring-from-rope
  (lambda (rope start end)
    (cond
      [(or (< start 0) (> end (rope-length rope)))
       (error "start and end must be within bounds")]
      [(> start end) (error "end must be greater than or equal to start")]

      [(= start end) ""]
      [(leaf-node? rope) (substring (node-value rope) start end)]
      [(<= end (node-value rope))
       (get-substring-from-rope (node-left rope) start end)]
      [(and (< start (node-value rope)) (> end (node-value rope)))
       (string-append-immutable
        (get-substring-from-rope (node-left rope) start (node-value rope))
        (get-substring-from-rope (node-right rope)
                                 0
                                 (- end (node-value rope))))]
      [(>= start (node-value rope))
       (get-substring-from-rope (node-right rope)
                                (- start (node-value rope))
                                (- end (node-value rope)))])))

(define rope-length
  (lambda (rope)
    (if (leaf-node? rope)
        (string-length (node-value rope))
        (+ (node-value rope) (rope-length (node-right rope))))))

(define insert-str-into-rope
  (lambda (rope str pos)
    (cond
      [(or (negative? pos) (> pos (rope-length rope)))
       (error "pos must be within bounds")]

      [(leaf-node? rope)
       (adjust (leaf-node (string-append-immutable
                           (substring (node-value rope) 0 pos)
                           str
                           (substring (node-value rope) pos))))]
      [(< pos (node-value rope))
       (node (+ (node-value rope) (string-length str))
             (insert-str-into-rope (node-left rope) str pos)
             (node-right rope))]
      [else
       (node (node-value rope)
             (node-left rope)
             (insert-str-into-rope (node-right rope)
                                   str
                                   (- pos (node-value rope))))])))

(define delete-from-rope
  (lambda (rope start end)
    (cond
      [(>= start end) (error "end must be greater than start")]
      [(or (< start 0) (> end (rope-length rope)))
       (error "start and end must be within bounds")]

      [(leaf-node? rope)
       (leaf-node (string-append-immutable (substring (node-value rope) 0 start)
                                           (substring (node-value rope) end)))]
      [(<= end (node-value rope))
       (adjust (node (- (node-value rope) (- end start))
                     (delete-from-rope (node-left rope) start end)
                     (node-right rope)))]
      [(and (< start (node-value rope)) (> end (node-value rope)))
       (adjust
        (node
         start
         (delete-from-rope (node-left rope) start (node-value rope))
         (delete-from-rope (node-right rope) 0 (- end (node-value rope)))))]
      [(>= start (node-value rope))
       (adjust (node (node-value rope)
                     (node-left rope)
                     (delete-from-rope (node-right rope)
                                       (- start (node-value rope))
                                       (- end (node-value rope)))))])))

(define adjust
  (lambda (rope)
    (cond
      [(and (leaf-node? rope) (> (rope-length rope) leaf-node-split-len))
       (str->rope (node-value rope))]
      [(< (rope-length rope) leaf-node-join-len) (leaf-node (rope->str rope))]
      [else rope])))


(define rebalance-rope (lambda (rope) (lleaves->rope (rope->lleaves rope))))

(define rope->lleaves
  (lambda (rope)
    (cond
      [(leaf-node? rope) (list rope)]
      [else
       (append (rope->lleaves (node-left rope))
               (rope->lleaves (node-right rope)))])))

(define lleaves->rope
  (lambda (lleaves)
    (cond
      [(= 1 (length lleaves)) (first lleaves)]
      [else
       (let ([halfway-point (quotient (length lleaves) 2)])
         (node (apply +
                      (map (lambda (leaf) (string-length (node-value leaf)))
                           (take lleaves halfway-point)))
               (lleaves->rope (take lleaves halfway-point))
               (lleaves->rope (drop lleaves halfway-point))))])))

(provide str->rope
         rope->str
         insert-str-into-rope
         delete-from-rope
         rebalance-rope
         get-substring-from-rope
         rope-length)
