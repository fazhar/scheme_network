;Constructs a add move which can be applied to any board
(define (add-move pos color)
    (lambda (current-game) (make-add-move current-game color pos)))

;Contructs a step move which can be applied to any board
(define (step-move pos-from pos-to color)
    (lambda (current-game) (make-step-move current-game color pos-from pos-to)))

;A factory for generating a series of moves 
(define (make-step-moves-for pos-from color destinations)
    (map (lambda (pos-to) (step-move pos-from pos-to color)) (filter destinations (lambda (dest)(not (= dest pos-from))))))

;Find's valid moves for player of color
(define (find-moves current-game color)
    (let ((my-pieces (pieces-for color current-game)) (other-pieces (pieces-for (other color) current-game))) 
        (if (> 10 (length my-pieces))
            (map (lambda (pos) (add-move pos color)) (get-valid-positions current-game color))
            (let ((factory (lambda (pos)(make-step-moves-for pos color (get-valid-positions (if (= color BLACK) (game (remove pos my-pieces) other-pieces)(game other-pieces (remove pos my-piece))) color)))))
                (reduce append (map  factory mypieces))))))

(define (scored-move score move)
    (list score move))

(define (score s-move)
    (car s-move))

(define (move s-move)
    (cadr s-move))

(define (choose-move game1 player)
    (let ((choice (alpha-beta game1 player -1000 1000 2)))
        (begin
            (display (score choice))
            (move choice))))

(define (pos-sum graph)
    (if (null? graph)
        0
        (let ((vertex (car (car graph))) (edges (cdr (car graph))))
            (+ 50 (sum (cons 0 (map (lambda (edge) (distance vertex edge)) edges))) (pos-sum (cdr graph))))))
    
(define (heuristic current-game color)
    (let (
        (graph (construct-graph (pieces-for color current-game)(pieces-for (other color) current-game)))
        (valid-positions (get-valid-positions current-game color)))
        (+ (pos-sum graph) (length valid-positions))))

(define (alpha-beta current-game player my-best opponents-best depth)
    (let ((moves (find-moves current-game player)))
        (define (inner-loop move-set best)
            (if (null? move-set)
                best
                (let ((new-game ((car move-set) current-game)))
                    (if (has-won? player new-game)
                        (scored-move 1000 (car move-set))
                        (if (>= (score best) opponents-best)
                            best
                            (let ((reply (alpha-beta new-game (other player) (- opponents-best) (- my-best) (- depth 1))))
                                (inner-loop (cdr move-set) 
                                    (if (> (- (score reply)) (score best))
                                        (scored-move (- (score reply)) (car move-set))
                                        best))))))))
            
        (if (zero? depth)
            (scored-move  (- (heuristic current-game player) (heuristic current-game (other player))) nil)
            (inner-loop moves (scored-move my-best (car moves))))))
