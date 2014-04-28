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

(define (choose-move game1 player)
    (alpha-beta game1 player -1000 1000 2 1))

(define (heuristic current-game color)
    0)

(define (alpha-beta current-game player my-best opponents-best depth main?)
    (let ((moves (find-moves game1 player)))
        (if (zero? depth)
            (* main? (heuristic current-game player))
            (reduce (lambda (best move)
                (cond 
                `   ((= 1000 best) best)
                    ((>= (* main? best) (* main? opponents-best)) best)
                    (else (let ((new-game (move current-game)))
                        (if (has-Won? player current-game)
                            1000           
                            (let ((reply (- (alpha-beta current-game (other player) (- opponentsBest) (- myBest) (- depth 1) (- main?)))))
                                (if (reply > best)
                                    reply
                                    best))))
                (cons my-best moves))))))))
                    
                
