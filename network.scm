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
        (if (> 0 (comp (car lst))) 
            (cons item lst)
            (cons (car lst) (insert-sorted (cdr lst) item comp)))))

(define (contains? lst item comp)
    (if (null? lst)
        #f
        (or (= 0 (comp item (car lst))) (contains? (cdr lst) item comp))))
    
;NOTE, this function is inclusive
(define (in-range? x range comp)
    (and (<= (comp (car range) x) 0)(>= (comp (cadr range) x) 0)))


;Position is a list where the first item is the x-coordinate, and the second is the y coordinate
(define (position x y)
    (list x y))
(define (comp-positions p1 p2) 
    (cond
        ((equal? p1 p2) 0)
        ((or (< (y-coor p1)(y-coor p2))(and (= (y-coor p1)(y-coor p2))(< (x-coor p1)(x-coor p2)))) -1)
        (else 1)))
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
            (not (or (contains? blacks position comp-positions) (contains? whites position comp-positions))) 
            (if (= color BLACK) 
                (is-valid-helper? blacks position #f)
                (is-valid-helper? whites position #f)))))

;Helper for the is-valid? function
(define (is-valid-helper? mypieces position has-neighbor) 
    (or (null? mypieces)
        (let ((cur (car mypieces)) (cur-neighbor? (neighbors? position (car mypieces))))
        (and (not (and has-neighbor cur-neighbor?))
            (is-valid-helper? (cdr mypieces) position (or has-neighbor cur-neighbor?))))))

;Return a new game board if the move is valid. Otherwise, return nil.
(define (make-add-move current-game color position)
    (if (is-valid? position color current-game)
        (if (= BLACK color)
            (game (sorted-insert position (black-pieces current-game)) (white-pieces current-game))
            (game (black-pieces current-game) (insert-sorted position (white-pieces current-game))))))


;Guide for checking directions the pieces last went in
(define LEFT 0)
(define UP-LEFT 1)
(define UP 2)
(define UP-RIGHT 3)
(define RIGHT 4)
(define DOWN-RIGHT 5)
(define DOWN 6)
(define DOWN-LEFT 7)
(define NO-DIRECTION 8)

;Finding Networks and determining if a player has won.
(define (find-network player goal-area fulladjlist)
    (if (null? goal-area) (nil) 
    (let (network (depth-first-search player (car goal-area) (find-adjlist (car goal-area) fulladjlist) fulladjlist '() NO-DIRECTION 0)
    (if (null? network)
        (find-network player (cdr goal-area) fulladjlist)
        (network))))))

(define (depth-first-search player vertex graph visited direction length-so-far)
    (if (< length-so-far 6) nil
        (if (= player BLACK) 
            (if (in-range? '(0 0) (y-coor vertex)) nil
                (if (in-range? '(7 7) (y-coor vertex))) visited
                nil)
            (if (in-range? '(0 0) (x-coor vertex)) nil
                (if (in-range? '(7 7) (x-coor vertex))) visited
                nil)))
    (define vertices (find-adjlist vertex graph))
    (define (dfs-helper network next) 
        (if (not (null? network))
            network
            (let ((dir (get-dir vertex next)))
                (if (= dir direction) nil
                    (if (contains? visited next position=?) nil
                        (depth-first-search player next graph (cons vertex visited) dir (+ length-so-far 1)))))))
    (reduce dfs-helper (cons nil vertices)))

;Helper method for dfs. Finds correct adjacency list.
(define (find-adjlist pos fulladjlist)
    (if (= (comp-positions pos (car (car fulladjlist))) 0)
        (car fulladjlist)
        (find-adjlist pos (cdr fulladjlist))))

;Method that checks the direction from pos1 to pos2
(define (get-dir pos1 pos2)
    (if (= (comp-positions pos1 pos2) 0) NO-DIRECTION)
    (if (= (delta-x pos1 pos2) 0) 
        (if (< 0 (delta-y pos1 pos2)) DOWN)
        UP)
    (if (= (delta-y pos1 pos2) 0) 
        (if (< 0 (delta-x pos1 pos2)) LEFT)
        RIGHT)
    (if (= (delta-x pos1 pos2) (delta-y pos1 pos2)) 
        (if (< 0 (delta-y pos1 pos2)) UP-LEFT)
        DOWN-RIGHT)
    (if (= (delta-x pos1 pos2) (- 0 (delta-y pos1 pos2)))
        (if (< 0 (delta-y pos1 pos2)) UP-RIGHT)
        DOWN-LEFT))
