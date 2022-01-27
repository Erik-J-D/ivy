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
  (lambda (tree)
    (cond
      [(null? tree) ""]
      [(leaf-node? tree) (node-value tree)]
      [else
       (string-append (rope->str (node-left tree))
                      (rope->str (node-right tree)))])))

(define insert-str-into-rope
  (lambda (tree str pos)
    (cond
      [(leaf-node? tree)
       (node (string-append (substring (node-value tree) 0 pos)
                            str
                            (substring (node-value tree) pos))
             empty
             empty)]
      [(> (node-value tree) pos)
       (node (+ (node-value tree) (string-length str))
             (insert-str-into-rope (node-left tree) str pos)
             (node-right tree))]
      [else
       (node (node-value tree)
             (node-left tree)
             (insert-str-into-rope (node-right tree)
                                   str
                                   (- pos (node-value tree))))])))

(provide str->rope
         rope->str
         insert-str-into-rope)
