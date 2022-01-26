#lang racket

(require rackunit
         "rope.rkt")

(define str "hello I am a small rope test")

(check-equal? (tree->str (str->tree str)) str)
