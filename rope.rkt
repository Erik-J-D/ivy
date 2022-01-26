#lang racket

(define max-len 10)
(define min-len (quotient max-len 2))

(define str->tree
  (lambda (str)
    (cond
      [(null? str) (make-node "" empty empty)]
      [(< (string-length str) max-len) (make-node str empty empty)]
      [else
       (let ([halfway-point (quotient (string-length str) 2)])
         (make-node halfway-point
                    (str->tree (substring str 0 halfway-point))
                    (str->tree (substring str halfway-point))))])))

(define tree->str
  (lambda (tree)
    (cond
      [(null? tree) ""]
      [(leaf-node? tree) (get-node-value tree)]
      [else
       (string-append (tree->str (get-left-node tree))
                      (tree->str (get-right-node tree)))])))

(define make-node
  (lambda (value left-node right-node) (list value left-node right-node)))

(define leaf-node? (lambda (node) (string? (get-node-value node))))

(define get-node-value first)
(define get-left-node second)
(define get-right-node third)

(provide str->tree
         tree->str)
