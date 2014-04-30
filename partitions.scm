(define (partition n mp mv)
	(cons (partition n mp mv) (partition n-1 mp mv))