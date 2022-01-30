#lang racket

(require rackunit
         "rope.rkt")

(define test-str "hello I am a small rope test")

; Basic Conversion Test
(check-equal? (rope->str (str->rope test-str)) test-str)

; Insertion Tests
; these make sense when we're breaking strings into chunks of max length 10
; but will need to be replaced with longer ones probably
(for ([r (+ 1 ( string-length test-str))])
  (check-equal?
   (rope->str (insert-str-into-rope (str->rope test-str) "¯\\_(ツ)_/¯" r))
   (string-append-immutable (substring test-str 0 r)
                            "¯\\_(ツ)_/¯"
                            (substring test-str r))))

; Deletion Tests
; Again, need long tests for bigger chunks
(for ([r `((4 8) (4 12) (4 21) (12 24) (24 25) (0 27))])
  (check-equal?
   (rope->str (delete-from-rope (str->rope test-str) (first r) (second r)))
   (string-append-immutable (substring test-str 0 (first r))
                            (substring test-str (second r)))))


; get-substring-from-rope
(for ([r `((4 8) (4 12) (4 21) (12 24) (24 25) (0 27))])
  (check-equal?
   (get-substring-from-rope (str->rope test-str) (first r) (second r))
   (substring test-str (first r) (second r))))
