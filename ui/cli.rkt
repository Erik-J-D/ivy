#lang racket
(require racket/match
         racket/format
         lux
         raart
         "../rope.rkt")

(define height 80)
(define width 80)

(struct cursor (row col) #:transparent)

(define (move-cursor c dr dc)
  (match c
    [(cursor row col) (cursor (max 0 (+ dr row)) (max 0 (+ dc col)))]))

(define (snap-cursor c buf)
  (match c
    [(cursor row col)
     (let* ([capped-row (min row (- (length buf) 1))]
            [cursor-line (list-ref buf capped-row)]
            [capped-col (min col (string-length cursor-line))])
       (cursor capped-row capped-col))]))

(define (render-line-with-cursor cursor line)
  (let* ([line-len (min width (string-length line))]
         [capped-cursor-pos (min (cursor-col cursor) line-len)]
         [char-at-cursor (cond
                           [(= capped-cursor-pos line-len) #\space]
                           [else (string-ref line capped-cursor-pos)])])
    (happend (text (substring line 0 capped-cursor-pos))
             (bg 'white (fg 'black (char char-at-cursor)))
             (text (substring line (min line-len (+ capped-cursor-pos 1)))))))

(define (render-buffer buf cursor initial-r)
  (let* ([snapped-cursor (snap-cursor cursor buf)]
         [lines-before-cursor (map text (take buf (cursor-row snapped-cursor)))]
         [line-with-cursor (render-line-with-cursor
                            snapped-cursor
                            (list-ref buf (cursor-row snapped-cursor)))]
         [lines-after-cursor
          (map text (drop buf (+ 1 (cursor-row snapped-cursor))))]
         [rendered-lines (vappend* (append lines-before-cursor
                                           (list line-with-cursor)
                                           lines-after-cursor)
                                   #:halign 'left)])
    (place-at* (blank width height)
               [(cursor-row cursor)
                (cursor-col cursor)
                (bg 'brblack (char #\space))] ;; shadow cursor
               [0 0 (crop 0 width 0 height rendered-lines)])))

(define (render-error err)
  (let ([context (continuation-mark-set->context (exn-continuation-marks err))])
    (vappend*
     (append (list (bg 'red (text "A WILD BUG APPEARS!")))
             (map text (string-split (~a err) "\n"))
             (list (happend*
                    (list (vappend* (map text (map ~a (map car context)))
                                    #:halign 'right)
                          (bg 'brblack (vline (length context)))
                          (vappend* (map text (map ~a (map cdr context)))
                                    #:halign 'left))
                    #:valign 'top)))
     #:halign 'left)))

(define (render-ivy w)
  (match w
    [(ivy rope cursor)
     (with-handlers ([exn? render-error]) ;; catch errors and renders them
       (without-cursor
        (crop 0
              width
              0
              height
              (render-buffer rope cursor (blank width height)))))]))

(define (insert-buf buf cur to-insert)
  (match cur
    [(cursor row col)
     (let ([cursor-line (list-ref buf row)])
       (append (take buf row)
               (list (string-append (substring cursor-line 0 col)
                                    to-insert
                                    (substring cursor-line col)))
               (drop buf (+ row 1))))]))

(define (delete-buf buf cur n-to-delete)
  (match cur
    [[cursor row col]
     (let ([cursor-line (list-ref buf row)])
       (append (take buf row)
               (list (string-append
                      (substring cursor-line 0 (max 0 (- col n-to-delete)))
                      (substring cursor-line col)))
               (drop buf (+ row 1))))]))

(define (handle-event state event)
  (match state
    [(ivy buf cur)
     (match event
       [(screen-size-report _w _h) state]
       ["<left>" (ivy buf (move-cursor cur 0 -1))]
       ["<right>" (ivy buf (move-cursor cur 0 1))]
       ["<up>" (ivy buf (move-cursor cur -1 0))]
       ["<down>" (ivy buf (move-cursor cur 1 0))]
       ["<backspace>" (ivy (delete-buf buf cur 1) (move-cursor cur 0 -1))]
       ["q" #f] ;; quit
       [c
        (ivy (insert-buf buf cur c) (move-cursor cur 0 (string-length c)))])]))

(struct ivy (buffer cursor)
  #:methods gen:word
  [(define (word-fps w)
     0.0)
   (define (word-label w ft)
     "Ivy")
   (define (word-event w e)
     (handle-event w e))
   (define (word-output w)
     (render-ivy w))
   (define (word-return w)
     (~a "See-ya!"))])

(define (initial-ivy-state rope)
  (ivy rope (cursor 0 0)))

(define (ivy-cli rope)
  (call-with-chaos (make-raart) (λ () (fiat-lux (initial-ivy-state rope)))))

(provide ivy-cli)
