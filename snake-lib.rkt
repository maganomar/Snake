#lang htdp/isl+

(require "provide.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide (struct-out game)
         (struct-out snake)
         play-game
         board-length)

(define board-length 50)
(define cell-length (* 2 5))

(define alive-color 'green)
(define dead-color 'red)
(define food-color 'yellow)
(define score-color 'gray)
(define board
  (rectangle (* cell-length board-length) 
             (* cell-length board-length)
             'solid
             'black))

(define tick-freq 1/20)

;; a game is
;; (make-game snake food nat) 
(define-struct game (snake food ticks)) 

;; a direction is either
;; - 'up
;; - 'down
;; - 'left
;; - 'right

;; a snake is
;; (make-snake direction body)
(define-struct snake (heading segments))

;; a body is either
;; - (cons posn empty)
;; - (cons posn body)

;; a food is either
;; - empty
;; - (cons posn food)

;; random-free-posn : game -> posn
(define (random-free-posn g)
  (nth-free-posn (random (num-free-posns g)) (make-posn 1 1) g))

;; num-free-posns : game -> posn
(define (num-free-posns g)
  (- (sqr board-length)
     (length (snake-segments (game-snake g)))
     (length (game-food g))))

(check-expect 
 (num-free-posns
  (make-game (make-snake 'up (list (make-posn 5 1))) 
             (list (make-posn 9 1) (make-posn 9 2))
             5))
 (- (sqr board-length) 3))

;; nth-free-posn : nat posn game -> posn
(define (nth-free-posn n p g)
  (cond [(zero? n) (next-free-posn p g)]
        [else
         (nth-free-posn (- n 1) (next-posn (next-free-posn p g)) g)]))

(check-expect 
 (nth-free-posn
  0
  (make-posn 1 1)
  (make-game (make-snake 'up (list (make-posn 9 9))) empty 5))
 (make-posn 1 1))
(check-expect 
 (nth-free-posn
  1
  (make-posn 1 1)
  (make-game (make-snake 'up (list (make-posn 9 9))) empty 5))
 (make-posn 2 1))
(check-expect 
 (nth-free-posn
  board-length
  (make-posn 1 1)
  (make-game (make-snake 'up (list (make-posn 9 9))) empty 5))
 (make-posn 1 2))
(check-expect 
 (nth-free-posn
  board-length
  (make-posn 1 1)
  (make-game (make-snake 'up (list (make-posn 5 1))) (list (make-posn 9 1)) 5))
 (make-posn 3 2))

;; next-free-posn : posn game -> posn
(define (next-free-posn p g)
  (cond [(occupied? p g)
         (next-free-posn 
          (make-posn
           (cond [(= (posn-x p) board-length) 1]
                 [else (+ (posn-x p) 1)])
           (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
                 [else (posn-y p)]))
          g)]
        [else p]))

(check-expect 
 (next-free-posn
  (make-posn 49 2)
  (make-game (make-snake 'up (list (make-posn 49 2)))
             (list (make-posn 50 2))
             5))
 (make-posn 1 3))

;; next-posn : posn -> posn
(define (next-posn p)
  (make-posn
   (cond [(= (posn-x p) board-length) 1]
         [else (+ (posn-x p) 1)])
   (cond [(= (posn-x p) board-length) (+ (posn-y p) 1)]
         [else (posn-y p)])))

(check-expect (next-posn (make-posn 1 1)) (make-posn 2 1))
(check-expect (next-posn (make-posn 50 1)) (make-posn 1 2))

;; occupied? : posn game -> boolean
(define (occupied? p g)
  (or (member p (game-food g))
      (member p (snake-segments (game-snake g)))))

(check-expect 
 (occupied? (make-posn 1 2)
            (make-game (make-snake 'up (list (make-posn 1 2)))
                       (list (make-posn 3 4))
                       5))
 true)
(check-expect 
 (occupied? (make-posn 1 2)
            (make-game (make-snake 'up (list (make-posn 3 4)))
                       (list (make-posn 1 2))
                       5))
 true)
(check-expect 
 (occupied? (make-posn 0 2)
            (make-game (make-snake 'up (list (make-posn 1 2)))
                       (list (make-posn 3 4))
                       5))
 false)

;; item : color -> image
(define (item c)
  (circle (/ cell-length 2) 'solid c))

(define alive-segment (item alive-color))
(define dead-segment (item dead-color))
(define food-morsel (item food-color))

;; place-items : image list-of-posn image -> image
(define (render-items item locations background)
  (foldr (lambda (location image)
           (place-item item location image))
         background
         locations))

;; place-item : image posn image -> image
(define (place-item item location background)
  (overlay/xy item
              (- (* cell-length (- (posn-x location) 1)))
              (- (* cell-length (- board-length (posn-y location))))
              background))

;; play-game : game
;              (game -> game) 
;;             (game direction -> game)
;;             (game posn -> game)
;;             (game -> nat) 
;;             (game -> boolean) 
;;             -> game
(define (play-game initial-game advance-game add-food change-direction game-score game-over?)
  (local [;; render-game : game -> image
          (define (render-game g)
            (render-items 
             (cond [(game-over? g) dead-segment]
                   [else alive-segment])
             (snake-segments (game-snake g))
             (render-items food-morsel 
                           (game-food g)
                           (render-score g board))))
          
          ;; render-score : game -> image
          (define (render-score game background)
            (overlay/align 'right 'top  
                           (text (string-append "Score: "
                                                (number->string (game-score game))) 
                                 12
                                 score-color)
                           background))
          
          ;; game-tick : game -> game
          (define (game-tick g)
            (local [(define morsels (length (game-food g)))
                    (define f (advance-game g))]
              (cond [(= (length (game-food f)) morsels) f]
                    [else (add-food f (random-free-posn f))])))
          
          ;; game-key : game key-event -> game
          (define (game-key g e)
            (cond [(member e '("up" "down" "left" "right"))
                   (change-direction g (string->symbol e))]
                  [else g]))]
    (big-bang initial-game
              (on-tick game-tick tick-freq)
              (on-key game-key)
              (stop-when game-over? render-game)
              (to-draw render-game))))