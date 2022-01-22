#lang racket/base
(require racket/match
         racket/format
         lux
         raart)

(define height 80)
(define width 80)

(struct cursor (row col) #:transparent)

(define (move-cursor c dr dc)
  (match c
    [(cursor row col) (cursor (max 0 (+ dr row)) (max 0 (+ dc col)))]))

(define (render-line r cursor row line)
  (define line-len (min width (string-length line)))

  (define (render-line-with-cursor)
    (let* ([real-cur-pos (cursor-col cursor)]
           [cur-pos (min real-cur-pos line-len)]
           [line-before-cursor (substring line 0 cur-pos)]
           [char-at-cursor (cond
                             [(= cur-pos line-len) #\space]
                             [else (string-ref line cur-pos)])]
           [line-after-cursor (substring line (min line-len (+ cur-pos 1)))])
      (let ([rendered
             (place-at*
              r
              [row 0 (text line-before-cursor)]
              [row cur-pos (bg 'white (fg 'black (char char-at-cursor)))]
              [row (+ 1 cur-pos) (text line-after-cursor)])])

        (if (not (= cur-pos real-cur-pos))
            ;; If our cursor is beyond the end-of-line, render a cursor shadow
            ;; where our pointer actually is.
            (place-at rendered row real-cur-pos (bg 'brblack (char #\space)))
            rendered))))

  (cond
    [(= row (cursor-row cursor)) (render-line-with-cursor)]
    [else (place-at r row 0 (text (substring line 0 line-len)))]))

(define (render-buffer w initial-r)
  (match w
    [(ivy buf cursor)
     (let-values ([(r i)
                   (for/fold ([r initial-r] [row 0]) ([line (in-list buf)])
                     (values (render-line r cursor row line) (+ row 1)))])
       r)]))

(struct ivy (buffer cursor)
  #:methods gen:word
  [(define (word-fps w)
     0.0)
   (define (word-label w ft)
     "Ivy")
   (define (word-event w e)
     (match w
       [(ivy buf cur) (match e
                        [(screen-size-report _ _) w]
                        ["<left>" (ivy buf (move-cursor cur 0 -1))]
                        ["<right>" (ivy buf (move-cursor cur 0 1))]
                        ["<up>" (ivy buf (move-cursor cur -1 0))]
                        ["<down>" (ivy buf (move-cursor cur 1 0))]
                        ["q" #f])]))
   (define (word-output w)
     (without-cursor
      (crop 0 width 0 height (render-buffer w (blank width height)))))
   (define (word-return w)
     (~a "See-ya!"))])

(define (initial-ivy-state buffer)
  (ivy buffer (cursor 0 0)))

(define (ivy-cli buffer)
  (call-with-chaos (make-raart) (λ () (fiat-lux (initial-ivy-state buffer)))))

(provide ivy-cli)
