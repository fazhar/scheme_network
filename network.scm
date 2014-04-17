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

(define (caddr l) (car (cdr (cdr l))))

(define (insert-sorted lst item comp-less-than)
    (if (null? lst) (list item)
        (if (comp-less-than item (car lst)) 
            (cons item lst)
            (cons (car lst) (insert-sorted (cdr lst) item comp-less-than)))))
            

(define (contains? lst item comp)
    (if (null? lst)
        #f
        (or (comp item (car lst)) (contains? (cdr lst) item comp))))
    

;Position is a list where the first item is the x-coordinate, and the second is the y coordinate
(define (position x y)
    (list x y))
(define (position=? p1 p2) 
    (equal? p1 p2))
(define (x-coor p) (car p))
(define (y-coor p) (cadr p))
(define (delta-x p1 p2)
    (- (x-coor p2) (x-coor p1)))
(define (delta-y p1 p2)
    (- (y-coor p2) (y-coor p1)))
(define (neighbors? p1 p2)
    (and (>= 1 (abs (delta-x p1 p2)))(>= 1 (abs (delta-x p1 p2)))))

;Getting valid positions and manipulating the board.

(define (game black-list white-list)
    (list black-list white-list))

(define (black-pieces game) (car game))
(define (white-pieces game) (cadr game))

;Returns whether position is a valid move for color. 

(define (is-valid? position color game)
    (let ((blacks (black-pieces game))(whites (white-pieces game)))
        (and
            (not (or (contains? blacks position position=?) (contains? whites position position=?))) 
            (if (= color BLACK) 
                (is-valid-helper? blacks position #f)
                (is-valid-helper? whites position #f)))))

;Helper for the is-valid? function
(define (is-valid-helper? mypieces position has-neighbor) 
    (or (null? mypieces)
        (let ((cur (car mypieces)) (cur-neighbor? (neighbors? position (car mypieces))))
        (and (not (and has-neighbor cur-neighbor?))
            (is-valid-helper? (cdr pieces) position (or has-neighbor cur-neighbor?))))))

;Return a new game board if the move is valid. Otherwise, return nil.
(define (make-add-move current-game color position)
    (if (is-valid? position color current-game)
        (if (= BLACK color)
            (game (sorted-insert (black-pieces current-game) position) (white-pieces current-game))
            (game (black-pieces current-game) (insert-sorted (white-pieces current-game) position)))))


;Guide for checking directions the pieces last went in
(define LEFT 0)
(define UP-LEFT 1)
(define UP 2)
(define UP-RIGHT 3)
(define RIGHT 4)
(define RIGHT-DOWN 5)
(define DOWN 6)
(define DOWN-LEFT 7)

;Finding Networks and determining if a player has won.
(define (find-network player goal-area fulladjlist)
    (if (null? goal-area) (nil) 
    (let (network (depth-first-search player (car goal-area) (find-adjlist (car goal-area) fulladjlist) fulladjlist)
    (if (null? network)
        (find-network player (cdr goal-area) fulladjlist)
        (network)))))

(define (depth-first-search player start))

;Helper method for dfs
(define (dfs player start vertices fulladjlist visited direction length-so-far)
)

;Helper method for dfs. Finds correct adjacency list.
(define (find-adjlist pos fulladjlist)
    (if (position=? pos (car (car fulladjlist)))
        (car fulladjlist)
        (find-adjlist pos (cdr fulladjlist))))

;Method that checks the direction from pos1 to pos2
(define (get-dir pos1 pos2)
    (cond ()))






