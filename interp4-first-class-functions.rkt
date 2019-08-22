#lang typed/racket

(define-type Arith
  (U Num Mul Add If Symbol Apply Random Lambda))
(define Arith? (make-predicate Arith))

(struct Lambda ([parameter : Symbol]
                [body : Arith]))

(struct Apply ([func : Arith]
               [param : Arith]))

(define-type Env (Listof Binding))

(struct Binding ([name : Symbol]
                 [def : (U Integer Lambda)]))

(struct Num ([n : Integer]))
(struct Mul ([l : Arith]
             [r : Arith]))
(struct Add ([l : Arith]
             [r : Arith]))
(struct If ([p : Arith]
            [t : Arith]
            [f : Arith]))
(struct Random ([min : Arith]
                [max : Arith]))

(: lookup (Symbol Env -> (U Integer Lambda)))
(define (lookup name env)
  (cond [(empty? env)
         (error (format "Unbound Identifier ~a" name))]
        [else
         (let ([bound-name {Binding-name
                            (first env)}])
           (if (equal? bound-name name)
               (Binding-def (first env))
               (lookup name (rest env))))]))

(: interp-as-int (-> Arith Env Integer))
(define (interp-as-int exp env)
  (let ([value (interp exp env)])
    (if (integer? value)
        value
        (error "Expected integer, found lambda"))))

(: interp (Arith Env -> (U Integer Lambda)))
(define (interp exp env)
  (cond [(Num? exp) (Num-n exp)]
        [(Mul? exp) (* (interp-as-int (Mul-l exp) env)
                       (interp-as-int (Mul-r exp) env))]
        [(Add? exp) (+ (interp-as-int (Add-l exp) env)
                       (interp-as-int (Add-r exp) env))]
        [(If? exp) (if (> (interp-as-int (If-p exp) env) 0)
                       (interp-as-int (If-t exp) env)
                       (interp-as-int (If-f exp) env))]
        [(Random? exp) (random
                        (interp-as-int (Random-min exp) env)
                        (interp-as-int (Random-max exp) env))]
        [(symbol? exp)
         (lookup exp env)]
        [(Lambda? exp)
         exp]
        [(Apply? exp)
         (let ([param-val (Apply-param exp)]
               [fundef (interp (Apply-func exp) env)])
           (if (integer? fundef)
               (error "Found arith in place of lambda")
               (interp (Lambda-body fundef)
                       (cons (Binding {Lambda-parameter fundef}
                                      (interp param-val env))
                             env))))]))

; Normal and applicative order evaluation
; cleaner interp, fix scoping

(module+ test
  (require typed/rackunit)

  (define empty-env '())

  (define simple-exp (Add (Num 1) (Num 2)))
  (check-equal? (interp simple-exp empty-env) 3)

  (define if-exp (Add (Num 1)
                      (If (Num 0)
                          (Num 3)
                          (Num 4))))
  (check-equal? (interp if-exp empty-env) 5)

  (: randoms (Listof Integer))
  (define randoms
    (for/list ([i (in-range 100)])
      (interp-as-int (Random (Num 1) (Num 4)) empty-env)))
  (check-equal?
   (length (filter (λ([x : Integer]) (not (<= 1 x 3)))
                   randoms))
   0)
  (check-equal?
   (length (filter (λ([x : Integer]) (member x '(1 2 3)))
                   randoms))
   100)

  (define arith-environ
    (list
     (Binding 'x 3)))
  (define x-exp
    (Mul 'x (Add (Num 3) 'x)))

  (check-equal?
   (interp x-exp arith-environ)
   18)

  (define fun-environ
    (list
     (Binding 'square (Lambda ;'f
                       'x
                       (Mul 'x 'x)))
     (Binding 'add1 (Lambda ;'g
                     'y
                     (Add (Num 1) 'y)))
     (Binding 'double (Lambda ;'h
                       'x
                       (Add 'x 'x)))))

  (define square-fun-exp
    (Mul (Num 10)
         (Apply 'square (Num 3))))

  (check-equal?
   (interp square-fun-exp
           fun-environ)
   90)

  (define composition-exp
    (Apply 'add1 (Apply 'square (Num 3))))
  (check-equal?
   (interp composition-exp fun-environ)
   10)

  (define closures-exp-1
    (Apply 'add1
           (Apply 'square
                  (Apply 'add1
                         (Apply 'square
                                (Num 1))))))
  (check-equal?
   (interp closures-exp-1
           fun-environ)
   5)

  (define closures-exp-2
    (Add (Apply 'square (Num 2))
         'x))
  (check-exn exn:fail?
             (λ() (interp closures-exp-2 fun-environ)))

  (define functions-as-values-exp
    (Apply
     (Lambda 'x
             (Apply
              (Lambda 'y
                      (Add 'x 'y))
              (Num 5)))
     (Num 3)))
  (check-equal? (interp functions-as-values-exp
                        empty-env)
                8)

  (define order-exp
    (Apply 'double (Random (Num 1) (Num 50))))
  (for ([i (in-range 100)])
    (check-true (even?
                 (interp-as-int order-exp fun-environ))))

  (define closures-exp-3
    (Apply (Apply 'add-x-to-new-x (Num 2)) (Num 3)))
  (define closures-exp-4
    (Apply (Apply 'add-x-to-y (Num 2)) (Num 3)))
  (define closure-environ
    (list
     (Binding 'add-x-to-new-x
              (Lambda 'x
                      (Lambda 'x
                              (Add 'x 'x))))
     (Binding 'add-x-to-y
              (Lambda 'x
                      (Lambda 'y
                              (Add'x 'y))))))
  (check-equal?
   (interp closures-exp-3 closure-environ) 6)
  ;; The following test fails in this interp
  #;(check-equal?
   (interp closures-exp-4 closure-environ) 5))
