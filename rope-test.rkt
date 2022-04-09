#lang racket

(require rackunit
         racket/random
         "rope.rkt")

(define test-str "hello I am a small rope test")

(define insert-str
  (lambda (str-or-rope str pos)
    (if (rope? str-or-rope)
        (insert-str-into-rope str-or-rope str pos)
        (string-append-immutable (substring str-or-rope 0 pos)
                                 str
                                 (substring str-or-rope pos)))))

(define del-region
  (lambda (str-or-rope start end)
    (if (rope? str-or-rope)
        (delete-from-rope str-or-rope start end)
        (string-append-immutable (substring str-or-rope 0 start)
                                 (substring str-or-rope end)))))

(define len
  (lambda (str-or-rope)
    (if (rope? str-or-rope)
        (rope-length str-or-rope)
        (string-length str-or-rope))))

(define (random-string)
  (apply string (random-sample "abcdefghijklmnopqrstuvwxyz" (random 20))))

(define (random-bool)
  (> 0.5 (random)))

; Basic Conversion Test
(check-equal? (rope->str (str->rope test-str)) test-str)

; get-substring-from-rope
(for ([r '((4 8) (4 12) (4 21) (12 24) (24 25) (0 27))])
  (check-equal?
   (get-substring-from-rope (str->rope test-str) (first r) (second r))
   (substring test-str (first r) (second r))))


; rebalance
(check-equal?
 (rope->str (rebalance-rope
             (insert-str-into-rope
              (str->rope "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16")
              "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfadfasdfasdf"
              10)))
 (rope->str (insert-str-into-rope
             (str->rope "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16")
             "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfadfasdfasdf"
             10)))

; Fuzz test to do random operations to rope
; do same op to a regular string and assert they behave
; the same
(define fuzz-rope-str
  (lambda (rope str count)
    (cond
      [(> count 0)
       (let* ([op (if (or (random-bool) (= 0 (string-length str)))
                      ; insert
                      (let* ([rnd-str (random-string)]
                             [l (string-length str)]
                             [pos (random (max l 1))])
                        (lambda (str-or-rope)
                          (insert-str str-or-rope rnd-str pos)))
                      ; delete
                      (let* ([l (string-length str)]
                             [end (if (> l 1) (random 1 l) 1)]
                             [start (random end)])
                        (lambda (str-or-rope)
                          (del-region str-or-rope start end))))]
              [new_rope (op rope)]
              [new_str (op str)])
         (begin
           (check-equal? (rope->str new_rope) new_str)
           ; TODO: assert balanced
           (fuzz-rope-str new_rope new_str (- count 1))))])))

(fuzz-rope-str (str->rope test-str) test-str 100000)


; balanced?
(check-true (balanced? (leaf-node "asd")))
(check-true (balanced? (node 5
                             (leaf-node "asdf1")
                             (node 5 (leaf-node "asdfg") (leaf-node "asdf")))))
(check-false
 (balanced? (node 5
                  (leaf-node "asdf1")
                  (node 10
                        (node 5 (leaf-node "zxcvb") (leaf-node "rtyui"))
                        (leaf-node "asdf")))))
