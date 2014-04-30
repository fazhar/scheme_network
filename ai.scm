;Constructs a add move which can be applied to any board
(define (add-move pos color)
    (lambda (current-game) (make-add-move current-game color pos)))

;Contructs a step move which can be applied to any board
(define (step-move pos-from pos-to color)
    (lambda (current-game) (make-step-move current-game color pos-from pos-to)))

;A factory for generating a series of moves 
(define (make-step-moves-for pos-from color destinations)
    (map (lambda (pos-to) (step-move pos-from pos-to color)) (filter destinations (lambda (dest)(not (= dest pos-from))))))

(define (find-moves current-game color)
    (let ((whites (white-pieces current-game)) (blacks (black-pieces current-game)))
    (let ((mypieces (if (= color BLACK) blacks whites)))
        (if (> 10 (length mypieces))
            (map (lambda (pos) (add-move pos color)) (get-valid-positions current-game color))
            (let ((factory (lambda (pos)(make-step-moves-for pos color (get-valid-positions (if (= color BLACK) (game (remove pos blacks) whites)(game blacks (remove pos whites))) color)))))
                (reduce append (map  factory mypieces)))))))

(define (scored-move score move)
    (list score move))

(define (score s-move)
    (car s-move))

(define (move s-move)
    (cadr s-move))

(define (choose-move game1 player)
    (move (alpha-beta game1 player -1000 1000 2 1)))

(define (pos-sum graph)
    (if (null? graph)
        0
        (let ((vertex (car (car graph))) (edges (cdr (car graph))))
            (+ 40 (/ (sum (cons 0 (map (lambda (edge) (distance vertex edge)) edges))) 2) (pos-sum (cdr graph))))))
    
(define (heuristic current-game color)
    (let (
        (graph (construct-graph (pieces-for color current-game)(pieces-for (other color) current-game)))
        (valid-positions (get-valid-positions current-game color)))
        (pos-sum graph)))


(define (alpha-beta current-game player my-best opponents-best depth main?)
    (let ((moves (find-moves current-game player)))
        (if (zero? depth)
            (scored-move (- (heuristic current-game player) (heuristic current-game (other player))) nil)
            (reduce (lambda (best current)
                (cond 
                    ((= (score best) 1000) best)
                    ((>= (score best) opponents-best) best)
                    (else (let ((new-game (current current-game)))
                        (if (has-won? player current-game)
                            (scored-move 1000 current)
                            (let ((reply (alpha-beta new-game (other player) (- opponents-best) (- (score best)) (- depth 1) (- main?))))
                                (if (> (- (score reply)) (score best))
                                    (scored-move (score reply) current)
                                    best)))))))
                (cons (scored-move my-best (car moves)) moves)))))
                    
                
