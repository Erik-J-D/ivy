#lang racket/base
(require racket/match
         racket/format
         lux
         raart)

(define row 80)
(define col 80)

(struct cursor (row col))

(define (move-cursor w dr dc)
  (match w
    [(ivy buf (cursor row col)) (ivy buf (cursor (+ dr row) (+ dc col)))]))

(define (render-line initial-r i-row line cursor-row cursor-col)
  (let-values ([(r i) (for/fold ([r initial-r] [i-col 0]) ([c (in-string line)])
                        (values (place-at r i-row i-col (if (and (= i-col cursor-col) (= i-row cursor-row)) (bg 'red (char c)) (char c))) (+ 1 i-col)))])
    r))

(define (render-buffer w initial-r)
  (match w
    [(ivy buf (cursor row col))
     (let-values ([(r i) (for/fold ([r initial-r] [i-row 0]) ([line (in-list buf)])
                           (values (render-line r i-row line row col) (+ 1 i-row)))])
       r)]))

(struct ivy (buffer cursor)
  #:methods gen:word
  [(define (word-fps w) 0.0)
   (define (word-label w ft) "Ivy")
   (define (word-event w e)
     (match e
       [(screen-size-report _ _) w]
       ["<left>"  (move-cursor w  0 -1)]
       ["<right>" (move-cursor w  0  1)]
       ["<up>"    (move-cursor w -1  0)]
       ["<down>"  (move-cursor w  1  0)]
       ["q" #f]))
   (define (word-output w)
     (without-cursor
      (crop 0 row 0 col
            (render-buffer w (blank row col)))))
   (define (word-return w)
     (~a "See-ya!"))])

(define (initial-ivy-state buffer)
  (ivy buffer (cursor 0 0)))

(define (ivy-cli buffer)
  (call-with-chaos
   (make-raart)
   (λ () (fiat-lux (initial-ivy-state buffer)))))

(provide
 ivy-cli)
