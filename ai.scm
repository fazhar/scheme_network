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

(define (choose-move game1 color)
    (let ((moves (find-moves game1 color)))
        
    )))

(define (heuristic current-game color)
    0)

(define (alpha-beta current-game color myBest opponentBest depth)
    (let ((moves (find-moves game1 color))
        (if (= depth 0)
            (heuristic current-game color)
            (reduce 
                (lambda (move)))))))
                    
                
