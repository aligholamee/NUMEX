;          NUMEX interpreter
; ========================================
; [] File Name : numexLang.rkt
;
; [] Creation Date : December 2017
;
; [] Created By : Ali Gholami (aligholami7596@gmail.com)
; ========================================
;

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs
(struct var  (string) #:transparent)  ;; a variable
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct bool (b)      #:transparent)  ;; a boolean value, e.g., (bool #t)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct mult (e1 e2)  #:transparent)  ;; multiply two expressions
(struct neg  (e1)     #:transparent)  ;; negate the expression
(struct islthan (e1 e2) #:transparent) ;; is less than
(struct ifzero (e1 e2 e3) #:transparent) ;; tests e1
(struct ifgthan (e1 e2 e3 e4) #:transparent) ;; tests if e1 is greater than e2
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0
(struct mlet (s e1 e2)  #:transparent) ;; a local bounder which the value of e1 is bound to s in the expression e2
(struct apair (e1 e2)   #:transparent) ;; pair constructor
(struct first (e1)      #:transparent) ;; the first element of the pair e1
(struct second (e2)     #:transparent) ;; the second element of the pair e2
(struct closure (env fun) #:transparent) ;; a closure is not in "source" programs; it is what functions evaluate to

; Converts racket lists to numex lists
(define (racketlist->numexlist xs) (cond [(null? xs) (munit)]
                                         [true (apair (car xs) (racketlist->numexlist (cdr xs)))]))

; Converts the numex lists to racket lists
(define (numexlist->racketlist xs) (cond [(munit? xs) '()]
                                         [true (cons (apair-e1 xs) (numexlist->racketlist (apair-e2 xs)))]))

;; Lookup for a variable in an environment
(define (envlookup env str) ;; env is a racket list apparantly :D
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(eq? (car (car env)) str) (cdr (car env))]
        [true (envlookup (cdr env) str)]
		))

;; Environment creator function
(define (createNewEnv env actuals)
  (cond [(null? env) actuals]
        [true (cons (car env) (createNewEnv (cdr env) actuals))]))

;; Creates the essential environment
(define (generateNewEnv env freevars)
  (cond [(null? env) null]
        [true (cond
                [(set-member? freevars (car (car env))) (cons (car env) (generateNewEnv (cdr env) freevars))]
                [true (generateNewEnv (cdr env) freevars)])]))


; The helper function of the eval-exp
(define (eval-under-env-c e env)
  (cond [(var? e)
         (cond [(string? (var-string e))(envlookup env (var-string e))]
               [true (error (format "Dude! Variable contains string!"))])]
        
        ; Addition
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "numex addition applied to non-number")))]
        
        ; Multiplication
        [(mult? e)
         (let ([v1 (eval-under-env-c (mult-e1 e) env)]
               [v2 (eval-under-env-c (mult-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (* (int-num v1)
                       (int-num v2)))
               (error "numex multiplication applied to non-number")))]
        
        ; Negation
        [(neg? e)
         (let ([v1 (eval-under-env-c (neg-e1 e) env)])
           (if (int? v1) (int (- (int-num v1)))
               (error "numex negation applied to non-number")))]
        
        ; Integer value
        [(int? e)
         (cond
           [(integer? (int-num e)) e]
           [true (error (format "Dude! Wrong thing in int!"))])]

        ; Is less than comparison
        [(islthan? e)
         (let ([v1 (eval-under-env-c (islthan-e1 e) env)]
               [v2 (eval-under-env-c (islthan-e2 e) env)])
           (cond
             [(int? v1) (cond
                          [(int? v2) (cond
                                       [(< (int-num v1) (int-num v2)) (int 1)]
                                       [true (int 0)])]
                          [true (error (format "Dude! islthan bad second argument"))])]
             [true (error (format "Dude! islthan bad first argument!"))]))]
        
        ; Is zero condition
        [(ifzero? e)
         (cond
           [(string? (ifzero-e1 e)) (error (format "ifzero bad argument"))]
           [true (let ([v1 (eval-under-env-c (ifzero-e1 e) env)])
                   (cond
                     [(int? v1) (cond [(eq? (int-num v1) 0) (eval-under-env-c (ifzero-e2 e) env)]
                                      [true (eval-under-env-c (ifzero-e3 e) env)])]
                     [true (error (format "Dude! Argument won't be evaluated to an integer!"))]))])]
        
        ; If greater condition
        [(ifgthan? e)
         (cond
           [(string? (ifgthan-e1 e)) (error (format "ifgthan bad argument"))]
           [(string? (ifgthan-e2 e)) (error (format "ifgthan bad argument"))]
           [true (let ([v1 (eval-under-env-c (ifgthan-e1 e) env)]
                       [v2 (eval-under-env-c (ifgthan-e2 e) env)])
                   (cond
                     [(> (int-num v1) (int-num v2)) (eval-under-env-c (ifgthan-e3 e) env)]
                     [true (eval-under-env-c (ifgthan-e4 e) env)]))])]

        ; Function declaration
        [(fun-challenge? e)
         (closure (generateNewEnv env (set-remove (fun-challenge-freevars e) (fun-challenge-nameopt e))) e)]

        ; Function call
        [(call? e)
          (let ([funClosure (eval-under-env-c (call-funexp e) env)])
            (cond
              [(closure? funClosure) (let ([functionDeclaration (closure-fun funClosure)])
                                       (let ([evaluatedActual (eval-under-env-c (call-actual e) env)])
                                         (eval-under-env-c (fun-body functionDeclaration) (cons (cons (fun-formal functionDeclaration) evaluatedActual)
                                                                                          (cons (cons (fun-nameopt functionDeclaration) funClosure) (closure-env funClosure))))))]
              [true (error (format "Dude! numex Pass a closure in call!"))]))]   

        ; apair handler
        [(apair? e)
         (let([v1 (eval-under-env-c (apair-e1 e) env)]
              [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]

        ; First and Second handler
        [(first? e)
         (let ([v1 (eval-under-env-c (first-e1 e) env)])
           (cond
             [(apair? v1) (apair-e1 v1)]
             [true (error (format "Dude. Pass a pair within the first."))]))]

        [(second? e)
         (let ([v1 (eval-under-env-c (second-e2 e) env)])
           (cond
             [(apair? v1) (apair-e2 v1)]
             [true (error (format "Dude. Pass a pair within the second."))]))]

        ; ismunit handler
        [(ismunit? e)
         (let ([v1 (eval-under-env-c (ismunit-e e) env)])
           (cond
             [(munit? v1) (int 1)]
             [true (int 0)]))]

        ; munit
        [(munit? e)
           (munit)]

        ; mlet handler
        [(mlet? e)
         (define sName (mlet-s e))
         (let ([v1 (eval-under-env-c (mlet-e1 e) env)])
           (eval-under-env-c (mlet-e2 e) (cons (cons sName v1) env)))]

        ; Closure handler
        [(closure? e) e]
        
        
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([calculate-free-variable (lambda (e)
       (cond
         [(var? e) (set (var-string e))]
         [(add? e) (set-union (calculate-free-variable (add-e1 e)) (calculate-free-variable (add-e2 e)))]
         [(mult? e) (set-union (calculate-free-variable (mult-e1 e)) (calculate-free-variable (mult-e2 e)))]
         [(neg? e) (calculate-free-variable (neg-e1 e))]
         [(int? e) (set)]
         [(islthan? e) (set-union (calculate-free-variable (islthan-e1 e)) (calculate-free-variable (islthan-e2 e)))]
         [(ifzero? e) (set-union (calculate-free-variable (ifzero-e1 e)) (calculate-free-variable (ifzero-e2 e)) (calculate-free-variable (ifzero-e3 e)))]
         [(ifgthan? e) (set-union (calculate-free-variable (ifgthan-e1 e)) (calculate-free-variable (ifgthan-e2 e)) (calculate-free-variable (ifgthan-e3 e)) (calculate-free-variable (ifgthan-e4 e)))]
         [(fun? e) (set-remove (set-remove (calculate-free-variable (fun-body e)) (fun-formal e)) (fun-nameopt e))]
         [(call? e) (set-union (calculate-free-variable (call-funexp e)) (calculate-free-variable (call-actual e)))]
         [(apair? e) (set-union (calculate-free-variable (apair-e1 e)) (calculate-free-variable (apair-e2 e)))]   
         [(first? e) (calculate-free-variable (first-e1 e))]
         [(second? e) (calculate-free-variable (second-e2 e))]
         [(ismunit? e) (calculate-free-variable (ismunit-e e))]
         [(munit? e) (set)]
         [(mlet? e) (set-union (calculate-free-variable (mlet-e1 e)) (set-remove (calculate-free-variable (mlet-e2 e)) (mlet-s e)))]
         [(closure? e) (set)]
         ))])
   
         (letrec ([generate-exp (lambda (e)                              
                                  (cond [(var? e) e]
        
                                        ; Addition
                                        [(add? e) e]
        
                                        ; Multiplication
                                        [(mult? e) e]
        
                                        ; Negation
                                        [(neg? e) e]
        
                                        ; Integer value
                                        [(int? e) e]

                                        ; Is less than comparison
                                        [(islthan? e) e]
        
                                        ; Is zero condition
                                        [(ifzero? e) e]
        
                                        ; If greater condition
                                        [(ifgthan? e) e]

                                        ; Function declaration
                                        [(fun? e) (fun-challenge (fun-nameopt e) (fun-formal e) (generate-exp (fun-body e)) (calculate-free-variable e))]

                                        ; Function call
                                        [(call? e) (call (generate-exp (call-funexp e)) (generate-exp (call-actual e)))]   

                                        ; apair handler
                                        [(apair? e) e]

                                        ; First and Second handler
                                        [(first? e) e]

                                        [(second? e) e]

                                        ; ismunit handler
                                        [(ismunit? e) e]

                                        ; munit
                                        [(munit? e) e]

                                        ; mlet handler
                                        [(mlet? e) (mlet (mlet-s e) (generate-exp (mlet-e1 e)) (generate-exp (mlet-e2 e)))]

                                        ; Closure handler
                                        [(closure? e) e]
    ))])
           
           (generate-exp e))))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))



;; Expanding the NUMXES
;; Defining Macros

;; Macro #1
;(define (ifmunit e1 e2 e3) (cond [(equal? (ismunit e1) (int 1)) e2] [true e3]))
;(define (ifmunit e1 e2 e3) (cond [(eq? (munit? e1) #t) e2] [true e3]))
(define (ifmunit e1 e2 e3) (ifzero (ismunit e1) e2 e3))

;(struct mlet (s e1 e2)  #:transparent)
;; Macro #2
;(define (mlet* pairList finalExp) (mlet (car) (cons env)) )
;(define (mlet* pairList finalExp)(call (fun "generateList" "List" (cond [(null? (var "List")) finalExp] [true (mlet (car (car pairList)) (cdr (car pairList)) (call (var "generateList") (cdr (var "List"))))])) finalExp))
(define (mlet* pairList finalExp)(cond [(null? pairList) finalExp] [true (mlet (car (car pairList)) (cdr (car pairList)) (mlet* (cdr pairList) finalExp))]))
                                                                                 
(define program (fun "adderFunction" "someVariable" (add (int 1) (var "someVariable"))))
(define program2 (mlet "amoo" (int 5) (add (int 1) (var "amoo"))))

; Macro #3
(define (ifeq e1 e2 e3 e4)
   (let ([v1 e1]
         [v2 e2])
  (ifgthan v1 v2 e4 (ifgthan v2 v1 e4 e3))))



;; New NUMEX functions | Internal functions

;(define (numex-map e) (fun "map" "list" (apair (call e (car (var "list"))) (call (var "map") (cdr (var "list"))))))
  ;;(call (fun "numex-map" "numexFunction"
          ;; (fun "applied-numex-map" "numexList" (call (fun "numexFunction" null )


;(define numex-map (fun "final" "func" (fun "map" "list" (cond [(eq? (ismunit (var "list")) (int 1))]
                                                          ; [#t (apair (call (var "func") (first (var "list"))) (call (var "map") (second(var "list"))))]))))

; numex-map final version
(define numex-map (fun "final" "func" (fun "map" "list" (ifeq (ismunit (var "list")) (int 1) (munit)
                                                           (apair (call (var "func") (first (var "list"))) (call (var "map") (second(var "list"))))))))
; numex-mapAddn
;(define numex-mapAddn (numex-map (add (var "i") (
(define numex-mapAddN (fun null "i" (fun null "someList" (call (call numex-map (fun "addition"  "x" (add (var "x") (var "i")))) (var "someList")))))
                                 
;; Challenge Problem
(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

