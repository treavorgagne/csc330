#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)

;; designed based on test examples
(define (mupllist->racketlist lst)
  (cond [(apair? lst) (cons (mupllist->racketlist (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)))]
        [(aunit? lst) null]
        [(var? lst) lst]
        [(int? lst) lst]))

;; designed based on test examples
(define (racketlist->mupllist lst)
  (cond [(null? lst) (aunit)]
        [(list? lst) (letrec ([head (car lst)])
                      (apair (if (or (int? head) (var? head))
                                head
                                (racketlist->mupllist head)) 
                              (racketlist->mupllist (cdr lst))))]))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        ;; designed based on test examples
        [(int? e) e]
        ;; designed based on test examples and descriptions
        [(ifgreater? e)
          (letrec ([e1 (eval-under-env (ifgreater-e1 e) env)]
                [e2 (eval-under-env (ifgreater-e2 e) env)])
            (if (and (int? e1) (int? e2))
                (if (> (int-num e1) (int-num e2)) 
                    (eval-under-env (ifgreater-e3 e) env) 
                    (eval-under-env (ifgreater-e4 e) env))
                (error "MUPL ifgreater applied to non-number")))]
        ;; designed based on test examples and descriptions
        [(fst? e) 
          (letrec ([pair (eval-under-env (fst-e e) env)])
            (if (apair? pair) 
                (apair-e1 pair) 
                (error "MUPL fst applied to non-pair")))]
        ;; designed based on test examples and descriptions
        [(snd? e) 
          (letrec ([pair (eval-under-env (snd-e e) env)])
            (if (apair? pair) 
                (apair-e2 pair) 
                (error "MUPL snd applied to non-pair")))]
        ;; designed based on test examples and descriptions
        [(isaunit? e) 
          (letrec ([unit (eval-under-env (isaunit-e e) env)])
            (if (aunit? unit) (int 1) (int 0)))]
        ;; designed based on test examples and descriptions
        [(aunit? e) e]
        ;; designed based on test examples and descriptions
        [(apair? e) 
          (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        ;; designed based on test examples and descriptions
        [(mlet? e) 
          (eval-under-env (mlet-body e) (cons (cons (mlet-var e) (eval-under-env (mlet-e e) env)) env))]
        ;; designed based on test examples and descriptions
        [(fun? e) (closure env e)]
        ;; designed based on test examples and descriptions
        [(call? e) 
          (letrec ([f-exp (eval-under-env (call-funexp e) env)]
                   [act (eval-under-env (call-actual e) env)]) 
            (if (closure? f-exp)
                (letrec ([func (closure-fun f-exp)]
                          [fname (fun-nameopt func)])
                  (eval-under-env 
                    (fun-body func)
                    (if fname
                        (cons (cons (fun-formal func) act) (cons (cons fname f-exp) (closure-env f-exp)))
                        (cons (cons (fun-formal func) act) (closure-env f-exp)))))
                (error "MUPL first expression is not a closure")))]
        ;; one for each type of expression
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
;; note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C
;; ;; designed based on test examples and descriptions
(define (ifaunit e1 e2 e3) 
  (let ([v1 (isaunit e1)]
        [zero (int 0)])
  (ifgreater v1 zero e2 e3)))

;; designed based on test examples and descriptions
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (letrec ([head (car lstlst)]
               [tail (cdr lstlst)]
               [headhead (car head)]
               [tailhead (cdr head)])
        (mlet headhead tailhead (mlet* tail e2)))))

;; designed based on test examples and descriptions
(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1 
    (mlet "_y" e2
      (letrec ([v1 (var "_x")]
               [v2 (var "_y")]
               [veqexp (ifgreater v2 v1 e4 e3)])
        (ifgreater v1 v2 e4 veqexp)))))

;; Problem D

;; designed based on test examples and descriptions and hints
(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifaunit (var "lst")
                     (aunit)
                     (letrec ([varf (var "f")]
                              [varMM (var "mupl-map")]
                              [pair1 (fst (var "lst"))]
                              [pair2 (snd (var "lst"))]
                              [varApair (apair (call varf pair1) (call (call varMM varf) pair2))])
                      varApair )))))

;; designed based on test addtwo and descriptions and hints
(define mupl-mapAddN
  (mlet "map" mupl-map
    (fun "add-i-value" "i"
        (letrec ([mulpmap (var "map")]
                 [addi (fun #f "mapElement" (add (var "mapElement") (var "i")))])
        (call mulpmap addi)))))
