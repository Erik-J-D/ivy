#lang racket

(define max-len 10)
(define min-len (quotient max-len 2))

(struct node (value left right))

(define leaf-node? (lambda (node) (string? (node-value node))))

(define str->rope
  (lambda (str)
    (cond
      [(null? str) (node "" empty empty)]
      [(< (string-length str) max-len) (node str empty empty)]
      [else
       (let ([halfway-point (quotient (string-length str) 2)])
         (node halfway-point
               (str->rope (substring str 0 halfway-point))
               (str->rope (substring str halfway-point))))])))

(define rope->str
  (lambda (rope)
    (cond
      [(null? rope) ""]
      [(leaf-node? rope) (node-value rope)]
      [else
       (string-append (rope->str (node-left rope))
                      (rope->str (node-right rope)))])))

(define rope-length
  (lambda (rope)
    (if (leaf-node? rope)
        (string-length (node-value rope))
        (+ (node-value rope) (rope-length (node-right rope))))))

(define insert-str-into-rope
  (lambda (rope str pos)
    (cond
      [(leaf-node? rope)
       (node (string-append (substring (node-value rope) 0 pos)
                            str
                            (substring (node-value rope) pos))
             empty
             empty)]
      [(> (node-value rope) pos)
       (node (+ (node-value rope) (string-length str))
             (insert-str-into-rope (node-left rope) str pos)
             (node-right rope))]
      [else
       (node (node-value rope)
             (node-left rope)
             (insert-str-into-rope (node-right rope)
                                   str
                                   (- pos (node-value rope))))])))

(provide str->rope
         rope->str
         insert-str-into-rope
         rope-length)
