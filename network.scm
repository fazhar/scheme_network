;An implementation of the game Network in Scheme.
;
;Integers used to describe players:
;
;   0 - Player with black pieces
;   1 - Player with white pieces
;   -1 - No player
(define BLACK 0)
(define WHITE 1)
(define NONE -1)

;Utility functions
(define (contains? lst item comp)
    (if (null? lst)
        #f
        (or (= 0 (comp item (car lst))) (contains? (cdr lst) item comp))))
    
;NOTE, this function is inclusive
(define (in-range? x range comp)
    (and (<= (comp (car range) x) 0)(>= (comp (cadr range) x) 0)))


;Position is a list where the first item is the x-coordinate, and the second is the y coordinate
(define (position x y)
    (+ (* 8 y) x))

(define (comp-positions p1 p2) 
    (- p1 p2))

(define (x-coor p) (remainder p 8))
(define (y-coor p) (quotient p 8))

(define (delta-x p1 p2)
    (- (x-coor p2) (x-coor p1)))

(define (delta-y p1 p2)
    (- (y-coor p2) (y-coor p1)))

(define (neighbors? p1 p2)
    (and (>= 1 (abs (delta-x p1 p2)))(>= 1 (abs (delta-y p1 p2)))))

;list of null-spaces
(define null-space (list (position 0 0) (position 0 7) (position 7 0) (position 7 7)))

;Getting valid positions and manipulating the board.

(define (game black-list white-list)
    (list black-list white-list))

(define (pieces-for color game)(cond ((= color BLACK) (black-pieces game))((= color WHITE) (white-pieces game)) (else nil)))
(define (black-pieces game) (car game))
(define (white-pieces game) (cadr game))

;Maps func to all the positions from start to (7, 7)
(define (game-map func start)
    (if (>= 0 (comp-positions (position 7 7) start))
        (list (func start))
        (cons (func start) (game-map func 
            (if (> 7 (x-coor start)) 
                (position (+ 1 (x-coor start)) (y-coor start))
                (position 0 (+ 1 (y-coor start))))))))

(define (print-game game1)
    (let ((whites (white-pieces game1))(blacks (black-pieces game1)))
    (reduce string-append (game-map 
        (lambda (tile)
            (string-append (if (= 0 (x-coor tile)) "\t" "")
            (string-append
            (cond
                ((contains? null-space tile comp-positions)  "# ")
                ((contains? whites tile comp-positions) "W ")
                ((contains? blacks tile comp-positions) "B ")
                (else  "+ "))
            (if (= 7 (x-coor tile)) "\n" "")))) (position 0 0)))))
    

;Returns whether position is a valid move for color. 

(define (is-valid? position color game)
    (let ((blacks (black-pieces game))(whites (white-pieces game)))
        (and
            (not (or (contains? blacks position comp-positions) (contains? whites position comp-positions)))
            (let ((x (x-coor position))(y (y-coor position))(comp (lambda (x y) (- x y))))
            (if (= color BLACK) 
                (and (and (in-range? x (list 1 6) comp)(in-range? y (list 0 7) comp))(is-valid-helper? blacks position))
                (and (and (in-range? x (list 0 7) comp)(in-range? y (list 1 6) comp))(is-valid-helper? whites position)))))))

;Helper for the is-valid? function
(define (is-valid-helper? mypieces position)  
        (>= 1 (reduce + (map (lambda (p) (if (neighbors? position p) 1 0)) mypieces))))

;Return a new game board with the step or add move if the move is valid. Otherwise, return nil.
(define (make-add-move current-game color position)
    (if (is-valid? position color current-game)
        (if (= BLACK color)
            (game (cons position (black-pieces current-game)) (white-pieces current-game))
            (game (black-pieces current-game) (cons position (white-pieces current-game))))
        nil))

(define (make-step-move current-game color position-from position-to)
    (if (and (not (= 0 (comp-positions position-from position-to)))(contains? (pieces-for color current-game) position-from comp-positions))
        (let ((blacks (black-pieces current-game))(whites (white-pieces current-game))) 
        (if (= color BLACK)
            (make-add-move (game (remove position-from blacks) whites) BLACK position-to)
            (make-add-move (game blacks (remove position-from whites)) WHITE position-to)))
        nil))
    
;Finding Networks and determining if a player has won.
