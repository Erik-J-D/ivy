#lang racket
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

(define (render-line-with-cursor cursor-pos line)
  (let* ([line-len (min width (string-length line))]
         [capped-cursor-pos (min cursor-pos line-len)]
         [char-at-cursor (cond
                           [(= capped-cursor-pos line-len) #\space]
                           [else (string-ref line capped-cursor-pos)])])
    (let ([rendered
           (happend
            (text (substring line 0 capped-cursor-pos))
            (bg 'white (fg 'black (char char-at-cursor)))
            (text (substring line (min line-len (+ capped-cursor-pos 1)))))])

      (cond
        ;; If our cursor is beyond the end-of-line, render a shadow cursor
        ;; where our pointer actually is.
        [(not (= capped-cursor-pos cursor-pos))
         (place-at rendered 0 cursor-pos (bg 'brblack (char #\space)))]
        [else rendered]))))

(define (render-buffer w initial-r)
  (match w
    [(ivy buf (cursor cur-row cur-col))
     (vappend*
      (append (map text (take buf cur-row))
              (list (render-line-with-cursor cur-col (list-ref buf cur-row)))
              (map text (drop buf (+ 1 cur-row))))
      #:halign 'left)]))

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
