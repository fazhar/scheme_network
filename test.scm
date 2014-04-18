(load "network.scm")

;Tests for is-valid?
(define blacks (list (position 1 2) (position 3 4) (position 4 4)))
(define whites (list (position 5 4) (position 2 2) (position 6 6))) 
(define game1 (game blacks whites))
(display (list #f (is-valid? (position 1 2) BLACK game1)))
(display (list #f (is-valid? (position 2 2) BLACK game1)))
(display (list #t (is-valid? (position 6 7) BLACK game1)))
(display (list #t (is-valid? (position 1 1) BLACK game1)))
(display (list #f (is-valid? (position 2 3) BLACK game1)))
(display (list #f (is-valid? (position 7 4) BLACK game1)))
(display (list #f (is-valid? (position 0 3) BLACK game1)))
(newline)
(display (list #f (is-valid? (position 2 7) WHITE game1)))
(display (list #f (is-valid? (position 4 0) WHITE game1)))
(display (list #f (is-valid? (position 6 5) WHITE game1)))
(display (list #t (is-valid? (position 1 3) WHITE game1)))
(display (list #t (is-valid? (position 5 3) WHITE game1)))
(display (list #t (is-valid? (position 2 3) WHITE game1)))
(display (list #t (is-valid? (position 6 3) WHITE game1)))
(newline)
(newline)
(display (print-game game1))
(newline) 
(display (print-game (make-add-move game1 BLACK (position 5 6))))
(newline)
(display (print-game (make-add-move game1 WHITE (position 5 3))))
(newline)
(display (print-game (make-step-move game1 BLACK (position 1 2) (position 1 1))))  
