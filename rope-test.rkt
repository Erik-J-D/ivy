#lang racket

(require rackunit
         "rope.rkt")

(define test-str "hello I am a small rope test")

; Basic Conversion Test
(check-equal? (rope->str (str->rope test-str)) test-str)

; Insertion Tests
; these make sense when we're breaking strings into chunks of max length 10
; but will need to be replaced with longer ones probably
(check-equal? (rope->str (insert-str-into-rope (str->rope "hello") "_" 3))
              "hel_lo")
(check-equal? (rope->str (insert-str-into-rope (str->rope test-str) "_" 3))
              "hel_lo I am a small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope test-str) "_" 8))
              "hello I _am a small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope test-str) "_" 13))
              "hello I am a _small rope test")
(check-equal? (rope->str (insert-str-into-rope (str->rope test-str) "_" 20))
              "hello I am a small r_ope test")

; Deletion Tests
; Again, need long tests for bigger chunks
(check-equal? (rope->str (delete-from-rope (str->rope "hello") 2 4)) "heo")
(check-equal? (rope->str (delete-from-rope (str->rope test-str) 2 13))
              "hesmall rope test")
(check-equal? (rope->str (delete-from-rope (str->rope test-str) 2 20))
              "heope test")
(check-equal? (rope->str (delete-from-rope (str->rope test-str) 18 20))
              "hello I am a smallope test")

; get-substring-from-rope
(for ([r `((4 8) (4 12) (4 21) (12 24) (24 25) (0 27))])
  (check-equal?
   (get-substring-from-rope (str->rope test-str) (first r) (second r))
   (substring test-str (first r) (second r))))
