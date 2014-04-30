;An implementation of the game Network in Scheme.
;

;-----------------------------CONSTANTS------------------------------------;
;Integers used to describe players:
(define BLACK 0)
(define WHITE 1)
(define NONE -1)

;Guide for checking directions
(define LEFT 0)
(define UP-LEFT 1)
(define UP 2)
(define UP-RIGHT 3)
(define RIGHT 4)
(define DOWN-RIGHT 5)
(define DOWN 6)
(define DOWN-LEFT 7)
(define NO-DIRECTION 8)
;--------------------------------------------------------------------------;





;-----------------------------UTILITY-FUNCTIONS----------------------------;

(define (caddr l) (car (cdr (cdr l))))

(define (sum lst) (reduce + lst))

(define (contains? lst item comp)
    (if (null? lst)
        #f
        (or (= 0 (comp item (car lst))) (contains? (cdr lst) item comp))))

(define (find lst item comp)
    (if (null? lst) nil
        (if (= 0 (comp item (car lst))) (car lst)
            (contains? (cdr lst) item comp))))

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

;Finds the distance between between two positions
(define (distance p1 p2) (+ (abs (delta-x p1 p2)) (abs (delta-y p1 p2))))

;Function that checks the direction from pos1 to pos2
(define (get-dir pos1 pos2)
    (cond ((= (comp-positions pos1 pos2) 0) NO-DIRECTION)
          ((= (delta-x pos1 pos2) 0) 
            (if (< 0 (delta-y pos1 pos2)) DOWN UP))
          ((= (delta-y pos1 pos2) 0) 
            (if (< 0 (delta-x pos1 pos2)) RIGHT LEFT))
          ((= (delta-x pos1 pos2) (delta-y pos1 pos2)) 
            (if (< 0 (delta-y pos1 pos2)) DOWN-RIGHT UP-LEFT))
          ((= (delta-x pos1 pos2) (- 0 (delta-y pos1 pos2)))
            (if (< 0 (delta-y pos1 pos2)) DOWN-LEFT UP-RIGHT))
          (else NO-DIRECTION)))

;--------------------------------------------------------------------------;





;-----------------------------DIRECTED-POSITION----------------------------;
;A new piece thats has a direction
(define (dir-pos dir p) (cons dir p))

;Gets the direction of a directed position
(define (direction pos) (car pos))

(define (get-position pos) (cdr pos))

;Compares two directions
(define (comp-dir-pos p1 p2) 
    (cond ((= (direction p1) (direction p2)) 0)
          ((< (direction p1) (direction p2)) -1)
          (else 1)))

;Gets the closer of p1 and p2.
(define (get-closer pos p1 p2)
    (if (< (distance pos (get-position p1)) (distance pos (get-position p2))) p1 p2))

;Insert the item into the list in its correct sorted position. 
;If it equals another item use replace to determine if the item should be replaced.
(define (insert-sorted lst item comp replace?)
    (if (null? lst) (list item)
        (cond ((= 0 (comp item (car lst))) 
                (cons (replace? item (car lst)) (cdr lst)))
              ((> 0 (comp item (car lst))) 
                (cons item lst))
              (else (cons (car lst) (insert-sorted (cdr lst) item comp replace?))))))

;Checks to see is one piece blocks another in the same direction
(define (check-blocks pos pos-lst other-pos-lst)
    (if (null? pos-lst) pos-lst
        (cond ((< (comp-dir-pos (car pos-lst) (car other-pos-lst)) 0)
                (cons (get-position (car pos-lst)) (check-blocks pos (cdr pos-lst) other-pos-lst)))
              ((> (comp-dir-pos (car pos-lst) (car other-pos-lst)) 0)
                (check-blocks pos pos-lst (cdr other-pos-lst)))
              ((= (comp-dir-pos (car pos-lst) (car other-pos-lst)) 0)
                (let ((temp-pos (get-position (get-closer pos (car pos-lst) (car other-pos-lst)))))
                    (if (= (comp-positions temp-pos (get-position (car other-pos-lst))) 0)
                        (check-blocks pos (cdr pos-lst) (cdr other-pos-lst))
                        (cons (get-position (car pos-lst)) (check-blocks pos (cdr pos-lst) (cdr other-pos-lst)))))))))

(define (build-dir-lst pos pos-lst new-lst)
    (if (null? pos-lst)
        new-lst
        (build-dir-lst pos (cdr pos-lst) (insert-sorted new-lst (dir-pos (get-dir pos (car pos-lst)) (car pos-lst)) comp-dir-pos (lambda (p1 p2) (get-closer pos p1 p2))))))
;--------------------------------------------------------------------------;





;-----------------------------------GAME-----------------------------------;


(define (other player) (- 1 player))

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

(define (is-valid? position color game1)
    (let ((blacks (black-pieces game1)) (whites (white-pieces game1)))
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

;Finding pieces if first goal area
(define (get-goal-pieces player pieces)
    (if (= BLACK player) (filter pieces (lambda (piece) (= (y-coor piece) 0)))
                         (filter pieces (lambda (piece) (= (x-coor piece) 0)))))

;A method for AI to check if a player has won
(define (has-won? player game)
    (let ((other-winner
            (if (and (= (length (black-pieces game)) 10) (= (length (white-pieces game)) 10))
                (if (= player BLACK) (find-network (other player) (get-goal-pieces player (white-pieces game)) 
                                        (construct-graph (white-pieces game) (black-pieces game)))
                                     (find-network (other player) (get-goal-pieces player (black-pieces game))
                                        (construct-graph (black-pieces game) (white-pieces game))))
                nil)))
       (let ((winner
                (if (= player BLACK) (find-network player (get-goal-pieces player (black-pieces game)) 
                                        (construct-graph (black-pieces game) (white-pieces game)))
                                    (find-network player (get-goal-pieces player (white-pieces game))
                                        (construct-graph (white-pieces game) (black-pieces game))))))
       (if (null? winner) #f (if (null? other-winner) #t #f)))))

(define (get-user-input player game move-type)
    nil)

;--------------------------------------------------------------------------;






;----------------------------FINDING-NETWORKS------------------------------;
;Finding Networks and determining if a player has won.
(define (find-network player goal-area fulladjlist)
    (if (null? goal-area) nil
        (let ((network (depth-first-search player (car goal-area) fulladjlist '() NO-DIRECTION 0)))
            (if (null? network)
                (find-network player (cdr goal-area) fulladjlist)
                network))))

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
(define (find-adjlist pos graph)
    (if (= (comp-positions pos (car (car graph))) 0)
        (car graph)
        (find-adjlist pos (cdr graph))))

;Function thats takes a list of pieces and returns a graph represents the pieces of the specified player
(define (construct-graph pos-lst other-pos-lst)
    (map (lambda (pos) (cons pos (find-edges pos pos-lst other-pos-lst nil))) pos-lst))

;Helper function for construct-graph 
(define (find-edges pos pos-lst other-pos-lst with-direction) 
    (if (null? pos-lst)
        (check-blocks pos with-direction (build-dir-lst pos other-pos-lst '()))
        (let ((direction (get-dir pos (car pos-lst))))
            (if (= direction NO-DIRECTION)
                (find-edges pos (cdr pos-lst) other-pos-lst with-direction)
                (find-edges pos (cdr pos-lst) other-pos-lst (insert-sorted with-direction (dir-pos direction (car pos-lst)) comp-dir-pos (lambda (p1 p2) (get-closer pos p1 p2))))))))

;--------------------------------------------------------------------------;



