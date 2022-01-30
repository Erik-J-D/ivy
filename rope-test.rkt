#lang racket

(require rackunit
         "rope.rkt")

(define str "hello I am a small rope test")

; Basic Conversion Test
(check-equal? (rope->str (str->rope str)) str)

; Insertion Tests
; these make sense when we're breaking strings into chunks of max length 10
; but will need to be replaced with longer ones probably
(check-equal? (rope->str (insert-str-into-rope (str->rope "hello") "_" 3))
              "hel_lo")
(check-equal? (rope->str (insert-str-into-rope (str->rope str) "_" 3))
              "hel_lo I am a small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope str) "_" 8))
              "hello I _am a small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope str) "_" 13))
              "hello I am a _small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope str) "_" 20))
              "hello I am a small r_ope test")

; Deletion Tests
; Again, need long tests for bigger chunks
(check-equal? (rope->str (delete-from-rope (str->rope "hello") 2 4)) "heo")
(check-equal? (rope->str (delete-from-rope (str->rope str) 2 13))
              "hesmall rope test")
(check-equal? (rope->str (delete-from-rope (str->rope str) 2 20)) "heope test")
