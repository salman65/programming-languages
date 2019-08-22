#lang typed/racket


(define-type Arith (U Num Mul Add))

(struct Num ([n : Number]))
(struct Mul ([l : Arith]
             [r : Arith]))
(struct Add ([l : Arith]
             [r : Arith]))

(: interp (Arith -> Number))
(define (interp exp)
  (cond [(Num? exp) (Num-n exp)]
        [(Mul? exp) (* (interp (Mul-l exp))
                       (interp (Mul-r exp)))]
        [(Add? exp) (+ (interp (Add-l exp))
                       (interp (Add-r exp)))]))

;(interp (Mul (Num 2) (Num 5)))
;(interp (Mul (Add (Num 3) (Num 2)) (Num 5)))
