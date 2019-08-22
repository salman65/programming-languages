#lang typed/racket

(define-type Arith
  (U Num Mul Add If Symbol FunApp))
(define Arith? (make-predicate Arith))

(struct FunDef (;[name : Symbol]
                [parameter : Symbol]
                [body : Arith]))

(struct FunApp ([name : Symbol]
                [param : Arith]))

(define-type Env (Listof Binding))

(struct Binding ([name : Symbol]
                 [def : (U Arith FunDef)]))

(struct Num ([n : Integer]))
(struct Mul ([l : Arith]
             [r : Arith]))
(struct Add ([l : Arith]
             [r : Arith]))
(struct If ([p : Arith]
            [t : Arith]
            [f : Arith]))

(: lookup (Symbol Env -> (U Arith FunDef)))
(define (lookup name env)
  (cond [(empty? env)
         (error "Unbound Identifier")]
        [else
         (let ([bound-name {Binding-name
                            (first env)}])
           (if (equal? bound-name name)
               (Binding-def (first env))
               (lookup name (rest env))))]))
               

(: interp (Arith Env -> Integer))
(define (interp exp env)
  (cond [(Num? exp) (Num-n exp)]
        [(Mul? exp) (* (interp (Mul-l exp) env)
                       (interp (Mul-r exp) env))]
        [(Add? exp) (+ (interp (Add-l exp) env)
                       (interp (Add-r exp) env))]
        [(If? exp) (if (> (interp (If-p exp) env) 0)
                       (interp (If-t exp) env)
                       (interp (If-f exp) env))]
        [(symbol? exp)
         (let ([found (lookup exp env)])
           (if (Arith? found)
               (interp found env)
               (error "found fundef")))]
        [(FunApp? exp)
         (let ([param-val (FunApp-param exp)]
               [fundef (lookup (FunApp-name exp) env)])
           (if (Arith? fundef)
               (error "Found arith in place of fundef")
               (interp (FunDef-body fundef)
                       (cons (Binding {FunDef-parameter fundef}
                                      param-val)
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

  (define arith-environ
    (list
     (Binding 'x (Num 3))))
  (define x-exp
    (Mul 'x (Add (Num 3) 'x)))

  (check-equal?
   (interp x-exp arith-environ)
   18)

  (define fun-environ
    (list
     (Binding 'square (FunDef ;'f
                       'x
                       (Mul 'x 'x)))
     (Binding 'add1 (FunDef ;'g
                     'y
                     (Add (Num 1) 'y)))))

  (define square-fun-exp
    (Mul (Num 10)
         (FunApp 'square (Num 3))))

  (check-equal?
   (interp square-fun-exp
           fun-environ)
   90)

  (define composition-exp
    (FunApp 'add1 (FunApp 'square (Num 3))))
  (check-equal?
   (interp composition-exp fun-environ)
   10)

  (define closures-exp-1
    (FunApp 'add1
            (FunApp 'square
                    (FunApp 'add1
                            (FunApp 'square
                                    (Num 1))))))
  (check-equal?
   (interp closures-exp-1
           fun-environ)
   5)

  (define closures-exp-2
    (Add (FunApp 'square (Num 2))
         'x))
  (check-exn exn:fail?
   (λ() (interp closures-exp-2 fun-environ))))
