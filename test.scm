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

;Tests for depth-first-search
(define graph (list (list (position 0 1) (position 1 1))
					(list (position 1 1) (position 0 1) (position 1 5) (position 5 5))
					(list (position 1 5) (position 1 1) (position 5 1) (position 5 5))
					(list (position 5 1) (position 1 5) (position 7 3))
					(list (position 5 5) (position 1 1) (position 1 5))
					(list (position 7 3) (position 5 1) (position 5 5))))

(display (depth-first-search WHITE (position 0 1) graph nil NO-DIRECTION 0))