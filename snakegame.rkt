;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snakegame) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
(require "snake-lib.rkt")

(define mygame (make-game
                (make-snake
                 'up
                 (list (make-posn 25 25)))
                empty
                0))


;add-food : game posn -> game
;adds a morsel of food at the specified board position 
(define (add-food game loc)
  (make-game (game-snake game)
             (append (list loc) (game-food game))
             (game-ticks game)))

(check-expect (add-food mygame (make-posn 4 5))
              (make-game 
               (game-snake mygame)
               (list (make-posn 4 5))
               (game-ticks mygame)))



;change-direction : game direction -> game
;changes the direction in which the snake is traveling
(define (change-direction game dir)
  (make-game (new-direction game dir)
             (game-food game)
             (game-ticks game)))

(check-expect (change-direction mygame 'down)
              (make-game
               (make-snake 
                'down 
                (snake-segments (game-snake mygame)))
               (game-food mygame)
               (game-ticks mygame)))

;new-direction : game direction -> snake 
;returns snake with new direction
(define (new-direction game dir)
  (make-snake dir
              (snake-segments (game-snake game))))

(check-expect (new-direction mygame 'down)
              (make-snake 
               'down 
               (snake-segments (game-snake mygame))))


;game-score : game -> nat
;computes player's score based on snake length and time taken to reach length
;the score adds on 25 per length, while it aslo goes down as time elapses
(define (game-score game)
  (cond [(< 0 (- (* 100 (snake-length (game-snake game))) (game-ticks game)))
         (- (* 100 (snake-length (game-snake game ))) (game-ticks game))]
        [else 0]))

(check-expect (game-score mygame) 100)

;snake-length : snake -> nat
;returns the length of the snake
(define (snake-length s)
  (length (snake-segments s)))

(check-expect (snake-length (game-snake mygame)) 1)

(define mygame2 (make-game (make-snake 
                             'up
                             (list (make-posn 25 25) 
                                   (make-posn 25 24)))
                            empty 
                            0))

(check-expect(snake-length (game-snake mygame2)) 2)


;game-over? : game -> boolean 
;ends game if snake runs into itself of a wall
(define (game-over? g)
  (or 
   (self-hit (game-snake g))
   (wall-hit (game-snake g))))

(check-expect (game-over? (make-game 
                           (make-snake
                            'left
                            (list (make-posn 23 24)
                                  (make-posn 23 25)
                                  (make-posn 23 24)))
                           empty 0 )) true)

(check-expect (game-over? (make-game
                          (make-snake
                           'left
                           (list (make-posn -1 34)))
                          empty
                          0))
              true)

(check-expect (game-over? mygame) false)

;check-self : snake -> boolean 
;returns true if the snake hits itself (first segment at same location asnother segment)
(define (self-hit s)
  (member? (get-snake-head (snake-segments s))
           (get-rest-of-snake (snake-segments s))))

; check-wall : snake -> boolean
; returns true if the snake hits a wall (x or y < 1 or > 50)
(define (wall-hit s)
  (or
    (or 
     (> 1 (get-snake-x s))
     (< 50 (get-snake-x s)))
    (or
     (> 1 (get-snake-y s))
     (< 50 (get-snake-y s)))
  )
)



(check-expect (wall-hit (make-snake
                            'left
                            (list (make-posn -1 20)))) true)


; get-snake-x : snake -> number
; returns the x coordinate of the first segment of the snake body, returns
; -10 if body is empty
(define (get-snake-x s)
  (cond [(empty? (snake-segments s)) -10]
        [else (posn-x (first (snake-segments s)))]))

(check-expect (get-snake-x (game-snake mygame)) 25)

(check-expect (get-snake-x (make-snake
                            'right
                            empty)) -10)

; get-snake-y : snake -> number
; returns the y coordinate of the first segment of the snake body, returns
; -10 if body is empty
(define (get-snake-y s)
  (cond [(empty? (snake-segments s)) -10]
        [else (posn-y (first (snake-segments s)))]))

(check-expect (get-snake-y (game-snake mygame)) 25)

(check-expect (get-snake-y (make-snake
                            'right
                            empty)) -10)

; get-snake-head : body -> posn
; returns the coordinates of the snake's head
(define (get-snake-head b)
  (first b))

(check-expect (get-snake-head (snake-segments (game-snake mygame2))) (make-posn 25 25))

; get-rest-of-snake : body -> posn
; returns a list of coordinates of the body of the snake, without the head
(define (get-rest-of-snake b)
  (cond [(empty? b) empty]
        [else 
         (rest b)]))

(check-expect (get-rest-of-snake (snake-segments (game-snake mygame2))) (list (make-posn 25 24)))

; advance-game : game -> game
; moves game forward one step:
; -increases game's tick
; -snake might eat and grow
;    - if it eats, it adds a segment in the location of food
; -snake gains/loses a segment unless it eats
; -new segment is determined by the previous head(heading)
; -snake loses its oldest segment
(define (advance-game game)
  (move-snake game))

; increment-tick : game -> number
; increments tick by 1
(define (increment-tick game)
  (+ 1 (game-ticks game)))


; move-snake : game -> game
; moves the snake one space forward in the directions it's moving
; and checks if it eats
(define (move-snake game)
  (cond [(eats? game)
         (make-game
          (grow (game-snake game))
          (remove-food
           (game-food game)
           (one-in-front (game-snake game)))
          (increment-tick game))]
        [else 
         (make-game
          (remove-last-segment (add-segment (game-snake game)))
          (game-food game)
          (increment-tick game))]))

(check-expect (move-snake mygame)
              (make-game (make-snake
                          (snake-heading (game-snake mygame))
                          (list (make-posn 25 26)))
                         (game-food mygame) 1))



; eats? : game -> boolean
; checks if there is food one space in front of the snake 
; in the direction it's heading
(define (eats? game)
  (member? (one-in-front (game-snake game)) (game-food game)))

(check-expect (eats? mygame) false)


;one-in-front : snake -> posn
;returns pson that is one in front of the snake in the direction it is facing
(define (one-in-front s)
  (cond [ (symbol=? (snake-heading s)'up)
          (make-posn
           (get-snake-x s)
           (+ 1 (get-snake-y s)))]
        [(symbol=? (snake-heading s) 'down)
         (make-posn
          (get-snake-x s)
          (- (get-snake-y s)1))]
        [(symbol=? (snake-heading s) 'right)
         (make-posn
          (+ 1 (get-snake-x s))
          (get-snake-y s))]
        [(symbol=? (snake-heading s) 'left)
         (make-posn
          (-(get-snake-x s) 1)
          (get-snake-y s))]))

(check-expect (one-in-front (make-snake
                             'up
                             (list (make-posn 3 5)
                                   (make-posn 3 4))))(make-posn 3 6))

;grow : snake -> snake
;if snake eats, add segment in location of food, removes food
(define (grow s)
  (add-segment s ))

(check-expect (grow (game-snake mygame))
              (make-snake
               (snake-heading (game-snake mygame))
               (list (make-posn 25 26)
                     (make-posn 25 25))))

;add-segment : snake -> snake
;adds segment in direction snake is facing
(define (add-segment s)
  (make-snake
   (snake-heading s)
   (append 
    (list (one-in-front s))
    (snake-segments s))))

(check-expect (add-segment (game-snake mygame))
              (make-snake 'up
                          (list (make-posn 25 26)
                                (make-posn 25 25))))

;remove-food : food lacation -> food
;removes food from location
(define (remove-food f loc)
  (remove loc f))

(define mygame3 (make-game 
                 (make-snake
                  'up
                  (list (make-posn 25 25)
                        (make-posn 25 24)))
                 (list (make-posn 25 26)
                       (make-posn 45 45))
                 0))
(check-expect (remove-food (game-food mygame3) (one-in-front (game-snake mygame3)))
              (list (make-posn 45 45)))

;get-last-element : list-of-X -> X
;returns the last value in a lsist
(define (get-last-element lox)
  (cond[(empty? lox)empty]
       [else (first (reverse lox))]))

(check-expect (get-last-element (list 1 2 3 4 5))5)
(check-expect (get-last-element empty)empty)

;remove-last-segment : snake -> snake
;removes the last segment of a snake 
(define (remove-last-segment s)
  (make-snake 
   (snake-heading s)
   (remove (get-last-element (snake-segments s))
           (snake-segments s))))

(check-expect (remove-last-segment (game-snake mygame3))
              (make-snake
               'up
               (list (make-posn 25 25))))

(define game-start (make-game
                    (make-snake
                     'right
                     (list (make-posn 26 23)
                           (make-posn 25 23)
                           (make-posn 24 23)
                           (make-posn 23 23)))
                    (list (make-posn 10 10)
                          (make-posn 15 29)
                          (make-posn 34 40)
                          (make-posn 43 19))
                    0))
(play-game game-start
           advance-game
           add-food
           change-direction
           game-score
           game-over?)
                                   

                                   