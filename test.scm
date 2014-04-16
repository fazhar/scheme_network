(load "network.scm")

;Tests for is-valid?
(define blacks (list (position 1 2) (position 3 4) (position 4 4)))
(define whites (list (position 5 4) (position 2 2) (position 6 6))) 
(define game1 (game blacks whites))
(display (is-valid? (position 1 2) BLACK game1))
(display (is-valid? (position 2 2) BLACK game1))
(display (is-valid? (position 6 7) BLACK game1))
(display (is-valid? (position 1 1) BLACK game1))
(display (is-valid? (position 2 3) BLACK game1))
