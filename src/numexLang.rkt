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
(struct isgthan (e1 e2 e3 e4 e5) #:transparent) ;; tests if e1 is greater than e2
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0
(struct mlet (s e1 e2)  #:transparent) ;; a local bunder which thevalue of e1 is bound to s in the expression e2
(struct apair (e1 e2)   #:transparent) ;; pair constructor
(struct first (e1)      #:transparent) ;; the first element of the pair e1
(struct second (e2)     #:transparent) ;; the second element of the pair e2
(struct closure (env fun) #:transparent) ;; a closure is not in "source" programs; it is what functions evaluate to

; Converts racket lists to numex lists(type of integer specifically)
(define (racketlist->numexlist xs) (cond [(null? xs) (munit)]
                                         [true (apair (int (car xs)) (racketlist->numexlist (cdr xs)))]))

; Converts the numex lists to racket lists(type of integer specifically)
(define (numexlist->racketlist xs) (cond [(munit? xs) null]
                                         [true (cons (int-num (apair-e1 xs)) (numexlist->racketlist (apair-e2 xs)))]))
