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

;-----------------------------UTILITY-FUNCTIONS----------------------------;

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

;Filter function
(define (filter lst comp-function)
    (if (null? lst) lst
        (if (comp-function (car lst)) 
            (cons (car lst) (filter (cdr lst) comp-function))
            (filter (cdr lst) comp-function))))
    
;NOTE, this function is inclusive
(define (in-range? x range comp)
    (and (<= (comp (car range) x) 0)(>= (comp (cadr range) x) 0)))

;--------------------------------------------------------------------------;


;---------------------------------POSITION---------------------------------;

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
    (and (not (= 0 (comp-positions p1 p2)))(and (>= 1 (abs (delta-x p1 p2)))(>= 1 (abs (delta-y p1 p2))))))

(define (is-start-goal-piece? position color)
    (if (= color BLACK)
        (= 0 (y-coor position))
        (= 0 (x-coor position))))

;Checks is a piece is closer than a another piece in the same direction
(define (is-closer? pos1 pos2 direction) 
    (cond   ((= direction NO-DIRECTION) #f)
            ((= direction LEFT) )
            ((= direction UP-LEFT))
            ((= direction UP))
            ((= direction UP-RIGHT))
            ((= direction RIGHT))
            ((= direction DOWN-RIGHT))
            ((= direction DOWN))
            ((= direction DOWN-LEFT)))

;Function that checks if a position is in another positions place.
(define (in-lin-of-sight? pos1 pos2 opposite-pieces)
    nil)

;Function that checks the direction from pos1 to pos2
(define (get-dir pos1 pos2)
    (cond ((= (comp-positions pos1 pos2) 0) NO-DIRECTION)
          ((= (delta-x pos1 pos2) 0) 
            (if (< 0 (delta-y pos1 pos2)) DOWN) UP)
          ((= (delta-y pos1 pos2) 0) 
            (if (< 0 (delta-x pos1 pos2)) LEFT) RIGHT)
          ((= (delta-x pos1 pos2) (delta-y pos1 pos2)) 
            (if (< 0 (delta-y pos1 pos2)) UP-LEFT) DOWN-RIGHT)
          ((= (delta-x pos1 pos2) (- 0 (delta-y pos1 pos2)))
            (if (< 0 (delta-y pos1 pos2)) UP-RIGHT) DOWN-LEFT)
          (else NO-DIRECTION)))

;--------------------------------------------------------------------------;

;------------------------------DIRECTED-PIECE------------------------------;

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
    (if (null? game1) 
    "Invalid board"
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
            (if (= 7 (x-coor tile)) "\n" "")))) (position 0 0))))))
    

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
(define  (is-valid-helper? pieces position)
    (> 2 (reduce 
        (lambda (current-count piece)
            (if (<= 2 current-count)
                current-count
                (+ current-count (if (neighbors? position piece) (+ 1 (count-neighbors (remove piece pieces) piece)) 0))))  
        (cons 0 pieces))))

(define (count-neighbors pieces position)  
    (reduce + (map (lambda (piece) (if (neighbors? piece position) 1 0)) pieces)))

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

(define (get-valid-positions current-game color)
    (filter (game-map (lambda (position) (if (is-valid? position color current-game) position nil)) (position 0 0))(lambda (x) (not (null? x)))))

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

;A has-won method for AI
(define (has-Won? player game)
   (let winner 
        (if (= player BLACK) (find-network player (get-goal-pieces player (black-pieces game)) 
                                (construct-graph (black-pieces game) (white-pieces game))))
                             (find-network player (get-goal-pieces player (white-pieces game))
                                (construct-graph (white-pieces game) (black-pieces game))))
   (if (null? winner) #f #t))

;Finding pieces if first goal area
(define (get-goal-pieces player pieces)
    (if (= BLACK player) (filter pieces (lambda (pieces) (= (y-coor piece) 0)))
                         (filter pieces (lambda (pieces) (= (x-coor piece) 0)))))

;Finding Networks and determining if a player has won.
(define (find-network player goal-area fulladjlist)
    (if (null? goal-area) (nil) 
    (let (network (depth-first-search player (car goal-area) (find-adjlist (car goal-area) fulladjlist) fulladjlist '() NO-DIRECTION 0)
    (if (null? network)
        (find-network player (cdr goal-area) fulladjlist)
        (network))))))

;Function thats takes a list of pieces and returns a graph represents the pieces of the specified player
(define (construct-graph pieces opposite-pieces)
    (map (lambda (piece) (find-edges piece pieces opposite-pieces nil) pieces))

;Helper function for construct-graph 
(define (find-edges piece pieces opposite-pieces with-direction) 
    (if (null? pieces) 
        (check-blocks piece with-direction opposite-pieces)
        (let (direction (in-lin-of-sight? piece (car pieces) opposite-pieces) 
            (if (not (= direction NO-DIRECTION)) (find-edges piece (cdr pieces) opposite-pieces 
                                                    (insert-sorted (directed-piece direction piece) with-direction))
                                                 (find-edges piece (cdr pieces) opposite-pieces with-direction))))))


;DIRECTED PIECE

;A new piece thats has a direction
(define (directed-piece direction piece) (cons direction piece))

;Compares two directions
(define (comp-directed-piece p1 p2) 
    (cond ((= (direction p1) (direction p2)) 0)
          ((< (direction p1) (direction p2)) -1)
          (else 1)))

;Gives the direction of the directed-piece
(define (direction directed-piece) (car directed-piece)) 

;Checks to see is one piece blocks another in the same direction
(define check-blocks)

(define (depth-first-search player vertex graph visited direction length-so-far)
    (if (>= length-so-far 5)
        (if (= player BLACK) (if (= 0 (y-coor vertex)) nil (if (= 7 (y-coor vertex)) (cons vertex visited) nil))
                             (if (= 0 (x-coor vertex)) nil (if (= 7 (x-coor vertex)) (cons vertex visited) nil)))
        (begin 
            (define vertices (filter (cdr (find-adjlist vertex graph)) (lambda (x) (not (contains? visited x comp-positions)))))
            (define (dfs-helper network next) 
                (if (not (null? network))
                    network
                    (let ((dir (get-dir vertex next)))
                        (if (= dir direction) nil
                            (if (contains? visited next comp-positions) nil
                                (depth-first-search player next graph (cons vertex visited) dir (+ length-so-far 1)))))))
            (reduce dfs-helper (cons nil vertices)))))

;Helper function for dfs. Finds correct adjacency list.
(define (find-adjlist pos fulladjlist)
    (if (= (comp-positions pos (car (car fulladjlist))) 0)
        (car fulladjlist)
        (find-adjlist pos (cdr fulladjlist))))

