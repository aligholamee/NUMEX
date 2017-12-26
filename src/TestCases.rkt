#lang racket

;; syntax: https://docs.racket-lang.org/rackunit/api.html

(require "solution.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)

(define tests
  (test-suite
   "Project Tests"

   ;1
   ;(check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   ;2
   (check-equal? (eval-exp (islthan (int 2) (int 2))) (int 0) "islthan simple test")
   
   ;3
 ;  (check-exn (lambda (x) (string=? (exn-message x) "numex addition applied to non-number"))
  ;            (lambda () (eval-exp (add (int 2) (munit))))
   ;           "add bad argument")

   (check-equal? (eval-exp (ifzero (int 2) (int 2) (int 3))) (int 3) "ifzero simple test")

(check-equal? (eval-exp (neg (int 5) )) (int -5) "neg simple test")

(check-equal? (eval-exp (neg (int 0) )) (int 0) "neg simple test zero")

(check-equal? (eval-exp (neg (neg (int -9)) )) (int -9) "neg simple test double")

   (check-exn exn:fail?
              (lambda () (eval-exp (islthan (int 2) (int "a"))))
              "islthan bad argument")


   (check-exn exn:fail?
              (lambda () (eval-exp (ifzero (munit) (int 2) (int 3))))
              "ifzero bad argument")


   (check-exn exn:fail?
              (lambda () (eval-exp (var munit)))
              "var bad argument")

(check-exn exn:fail?
              (lambda () (eval-exp (var (int 2))))
              "var bad argument 2")
   
   ;when it should raise an error
   (check-exn exn:fail?
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-exn exn:fail?
              (lambda () (eval-exp (mult (munit) (int 3))))
              "mult bad argument")

   (check-exn exn:fail?
              (lambda () (eval-exp (neg (munit))))
              "neg bad argument")

   ;when it should run without raising any error
  ; (check-not-exn
    ;          (lambda () (eval-exp (add (int 2) (int 3))))
   ;           "add suitable arguments")
   
   (check-equal? (numexlist->racketlist
                  (eval-exp (call (call numex-mapAddN (int 9))
                                  (racketlist->numexlist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 19) (int 18) (int 24))
                 "provided combined test using problems 1, 2, and 4")

   (test-equal? "Numex list -> Racket list #1"
    (list (int 3) (int 4) (int 9))
    (numexlist->racketlist (apair (int 3) (apair (int 4) (apair (int 9) (munit))))))


(test-equal? "numex list -> Racket list #2"
    (apair (list (int 42) (var "x")) (apair (list (int 43) (var "y")) (munit)))
    (racketlist->numexlist (list (list (int 42) (var "x")) (list (int 43) (var "y"))))
  )

  (test-equal? "R list -> M list #3"
   (apair (var "foo") (apair (int 17) (munit)))
   (racketlist->numexlist (list (var "foo") (int 17))))


         ; racketlist->numexlist
   (check-equal? (racketlist->numexlist '(1 2 3 4))
                  (apair 1 (apair 2 (apair 3 (apair 4 (munit))))) "simple racketlist")
   (check-equal? (racketlist->numexlist '()) (munit) "empty racketlist")
   
   ; numexlist->racketlist
   (check-equal? (numexlist->racketlist (apair 1 (apair 2 (apair 3 (apair 4 (munit))))))
                  '(1 2 3 4) "simple numexlist")
   (check-equal? (numexlist->racketlist (munit)) '() "empty numexlist")
   

   
  (test-equal? "Local scoping"
               (int 2)
               (eval-exp (mlet "f1"
                               (fun "f1" "a" (mlet "x" (var "a") (fun "f2" "z" (add (var "x") (int 1)))))
                               (mlet "f3" (fun "f3" "f" (mlet "x" (int 1729) (call (var "f") (munit)))) 
                                     (call (var "f3") (call (var "f1") (int 1)))))))

  (test-equal? "basic-call"
               (int 43)
               (eval-exp (call (fun "incr" "x" (add (var "x") (int 1))) (int 42))))

 (test-equal? "basic-call mult neg"
               (int 10)
               (eval-exp (call (fun "incr" "x" (neg (mult (var "x") (int 2)))) (int -5))))

  
  (test-equal? "ifgthan with invalid e4"
               (int 0)
               (eval-exp (ifgthan (add (int 2) (int 2)) (mult (int 2) (int 1)) (add (int 3) (int -3)) (neg (add "wrong" "bad")))))

(test-equal? "ifzero with invalid e4"
               (int 2)
               (eval-exp (ifzero (add (int 2) (int -2)) (mult (int 2) (int 1)) (neg (add "wrong" "bad")))))


  (test-equal? "first/second test"
     (apair (int 21) (int 4))
     (eval-exp (apair (first (apair (mult (int 7) (int 3)) (int 2)))
                 (second (apair (int 3) (int 4))) )))

  (test-equal? "Sum over list"
     (int 6)
     (eval-exp (mlet "fnc"
       (fun "f1" "x" 
            (ifgthan (ismunit (var "x")) (int 0) 
                       (int 0) 
                       (add (first (var "x")) (call (var "f1") (second (var "x"))))))
       (call (var "fnc") (apair (int 1) (apair (int 2) (apair (int 3) (munit))))))))

  (test-equal? "ifmunit test #1"
   (int 2)
   (eval-exp (ifmunit (munit) (int 2) (int 3))))

  (test-equal? "ifmunit test #2"
   (int 3)
   (eval-exp (ifmunit (int 3) (int 2) (int 3))))
  
   
    (check-equal? (int 1)
     (eval-exp (mlet* (cons (cons "x" (int 1)) null) (var "x")))
     )

    (check-equal? 
    (int 20)
     (eval-exp (mlet* (list (cons "f" (int 2)) (cons "y" (int 15))) (add (var "f") (add (var "y") (int 3)))))
     )

 
    (check-equal? (int 1)
    (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (int 1) (int 2)))
    )


    (check-equal? (int 1)
    (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (islthan (int 1) (int 2)) (int 2)))
    )



    (check-equal? (int 0)
    (eval-exp (ifeq (int 2) (add (int 1) (int 1)) (mult (islthan (int 1) (int 2)) (int 0)) (int 2)))
    )

    
    (check-equal? 
    (int 2)
    (eval-exp (ifeq (int 2) (add (int 1) (int 2)) (int 1) (int 2))))

  (test-case "numex-map"
    (define addtwo (fun "addone" "x" (add (var "x") (int 2))))
    (define numex-map-addtwo (call numex-map addtwo))
    (check-equal? (eval-exp (call numex-map-addtwo (munit))) (munit))

    (define my-numex-list (apair (int 23) (apair (int 42) (munit))))
    (define my-answers (apair (int 25) (apair (int 44) (munit))))
    (check-equal? (eval-exp (call numex-map-addtwo my-numex-list)) my-answers))

  (test-case "numex-mapAddN"
    (define input (apair (int 25) (apair (int 44) (munit))))
    (define output (apair (int 26) (apair (int 45) (munit))))
    (check-equal? (eval-exp (call (call numex-mapAddN (int 1)) input)) output))

    (check-equal? (eval-exp (call (call numex-mapAddN (int 7))
                                  (racketlist->numexlist '())))
                 (munit) "mapAddN empty list")
    (check-equal? (eval-exp (call (call numex-mapAddN (int 7))
                                  (racketlist->numexlist (list (int 3) (int 4) (int 9)))))
                 (racketlist->numexlist (list (int 10) (int 11) (int 16))) "mapAddN +7")
   (check-equal? (eval-exp (call (call numex-mapAddN (int 7))
                                  (racketlist->numexlist (list (int 3)))))
                 (racketlist->numexlist (list (int 10))) "mapAddN single item list")
; add
   (check-equal? (eval-exp (add (int 3) (int 4))) (int 7) "simple add")
   (check-equal? (eval-exp (add (add (int 1) (int 2)) (add (int 3) (int 4)))) (int 10) "complex add")
   (check-exn #rx"numex" (lambda () (eval-exp (add (int 3) (munit)))) "add exception")

; mult
 (check-equal? (eval-exp (mult (int 5) (int -4))) (int -20) "simple mult")
   (check-equal? (eval-exp (mult (mult (int 3) (int 2)) (mult (int 3) (int 4)))) (int 72) "complex mult")
   (check-exn #rx"numex" (lambda () (eval-exp (mult (int 3) (munit)))) "mult exception")

   
   ; int
   (check-equal? (eval-exp (int 5)) (int 5) "int evaluation")
   
   ; munit
   (check-equal? (eval-exp (munit)) (munit) "munit evaluation")
   
   ; closure
   (check-equal? (eval-exp (closure '() (fun null "x" (var "x"))))
                 (closure '() (fun null "x" (var "x"))) "closure evaluation")
   
   ; mlet and var
   (check-equal? (eval-exp (mlet "x" (add (int 1) (int 1)) (var "x"))) (int 2) "mlet and var 1")
   (check-equal? (eval-exp (mlet "x" (int 1) (var "x"))) (int 1) "mlet and var 2")
   (check-exn #rx"unbound" (lambda () (eval-exp (var "x"))) "var exception")
   
   ; fun
   (check-equal? (eval-exp (fun null "x" (var "x")))
                 (closure '() (fun null "x" (var "x"))) "fun evaluation")
   (check-equal? (eval-exp (mlet "x" (int 1) (fun null "a" (var "x"))))
                 (closure (list (cons "x" (int 1))) (fun null "a" (var "x"))) "mlet and fun evaluation")
   
   ; ifgthan
   (check-equal? (eval-exp (ifgthan (int 1) (int 2) (int 3) (int 4))) (int 4) "simple ifgthan, false")
   (check-equal? (eval-exp (ifgthan (add (int 0)(int 1)) (add (int 0)(int 2)) (int 3) (int 4))) (int 4) "complex ifgthan, false")
   (check-equal? (eval-exp (ifgthan (int 1) (int 2) (int 3) (add (int 2)(int 2)))) (int 4) "complex ifgthan, false 2")
   (check-equal? (eval-exp (ifgthan (int 2) (int 1) (int 3) (int 4))) (int 3) "simple ifgthan, true")
   (check-equal? (eval-exp (ifgthan (add (int 0)(int 2)) (add (int 1) (int 0)) (int 3) (int 4))) (int 3) "complex ifgthan, true")
   (check-equal? (eval-exp (ifgthan (int 2) (int 1) (add (int 1)(int 2)) (int 4))) (int 3) "complex ifgthan, true 2")
   (check-exn #rx"numex" (lambda () (eval-exp (ifgthan "1" (int 2) (int 3) (int 4)))) "ifgthan exception")

   ; ifzero
   (check-equal? (eval-exp (ifzero (int 1) (int 2) (int 3))) (int 3) "simple ifzero, false")
   (check-equal? (eval-exp (ifzero (add (int 0)(int 0)) (neg (int 2)) (int 3) )) (int -2) "complex ifzero, false")
   (check-equal? (eval-exp (ifzero (int 1) (int 2) (islthan (int 0) (int -1)) )) (int 0) "complex ifzero, false 2")
   (check-equal? (eval-exp (ifzero (int 0) (int 4) (int 3) )) (int 4) "simple ifzero, true")
   (check-equal? (eval-exp (ifzero (mult (int 0)(int 2)) (add (int 1) (int 0)) (int 3))) (int 1) "complex ifzero, true")
   (check-equal? (eval-exp (ifzero (int 0) (add (int 1)(int 2)) (int 4))) (int 3) "complex ifzero, true 2")
   (check-exn #rx"numex" (lambda () (eval-exp (ifzero "1" (int 2) (int 3) ))) "ifzero exception")
   
   ; apair
   (check-equal? (eval-exp (apair (int 1) (int 1))) (apair (int 1) (int 1)) "int apair")
   (check-equal? (eval-exp (mlet "x" (int 1) (apair (var "x") (var "x"))))
                 (apair (int 1) (int 1)) "var apair")
   
   ; first
   (check-equal? (eval-exp (first (apair (int 1) (int 2)))) (int 1) "simple first")
   (check-equal? (eval-exp (mlet "x" (apair (int 1) (int 2)) (first (var "x")))) (int 1) "mlet and first")
   (check-exn #rx"numex" (lambda () (eval-exp (first (add (int 1) (int 2))))) "first exception")
   
   ; second
   (check-equal? (eval-exp (second (apair (int 1) (int 2)))) (int 2) "second evaluation")
   (check-equal? (eval-exp (mlet "x" (apair (int 1) (int 2)) (second (var "x")))) (int 2) "mlet and second")
   (check-exn #rx"numex" (lambda () (eval-exp (second (add (int 1) (int 2))))) "second exception")
   
   ; ismunit
   (check-equal? (eval-exp (ismunit (munit))) (int 1) "simple ismunit true")
   (check-equal? (eval-exp (mlet "x" (munit) (ismunit (var "x")))) (int 1) "mlet ismunit true")
   (check-equal? (eval-exp (ismunit (int 0))) (int 0) "simple ismunit false")
   (check-equal? (eval-exp (mlet "x" (int 0) (ismunit (var "x")))) (int 0) "mlet ismunit false")
   
   ; call
   (check-equal? (eval-exp (mlet "double" (fun "double" "x" (add (var "x") (var "x")))
                                  (call (var "double") (int 10))))
                 (int 20) "double function, non-recursive")
   (check-equal?
    (eval-exp
     (mlet "range"
           (fun "range" "lo"
                (fun null "hi"
                     (ifgthan (var "lo") (var "hi") (munit)
                                (apair (var "lo") (call (call (var "range") (add (int 1) (var "lo"))) (var "hi"))))))
           (call (call (var "range") (int 5)) (int 8))))
    (apair (int 5) (apair (int 6) (apair (int 7) (apair (int 8) (munit))))) "range function, recursive")

(check-equal?
 (eval-exp (call (fun "a" "b" (ifgthan (var "b") (int 5) (add (var "b") (int 3))
                         (call (var "a") (mult (int 2) (int 3))  ))) (int 2))
             )(int 9) "simple recursive call 2")





(check-equal?
 (eval-exp (call (fun "a" "b" (ifeq (var "b") (int 1) (int 3)
                         (mlet "b" (add (var "b") (int -1)) (call (var "a") (var "b")  )))) (int 2))
            ) (int 3) "simple recursive call")


(check-equal?
 (eval-exp (call (call (call (fun "a" "b" (fun "x" "y" (fun "w" "r" (neg (mult (add (var "b") (var "y")) (var "r"))))))
                       (int 2))
                       (int 3))
                       (int 5))
            ) (int -25) "higher order call")

(check-equal?
 (eval-exp (call (fun "a" "b" (ifzero (var "b") (int 3)
                                      (mlet* (list (cons "b" (add (var "b") (int -1)))) (add (int 1) (call (var "a") (var "b"))))
                                      )) (int 2))
             )(int 5) "complex recursive call 2")

   
   (check-exn #rx"numex" (lambda () (eval-exp (call (int 1) (int 2)))) "call exception")
   
   ; else
   (check-exn #rx"numex" (lambda () (eval-exp (list (int 1) (int 2)))) "bad expression exception")

   ; ifmunit
   (check-equal? (eval-exp (ifmunit (munit) (add (int 1)(int 2)) (add (int 3)(int 4)))) (int 3) "ifmunit true")
   (check-equal? (eval-exp (ifmunit (int 0) (add (int 1)(int 2)) (add (int 3)(int 4)))) (int 7) "ifmunit false")
   
   ; mlet*
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "y" (int 2))) (add (var "x")(var "y"))))
                 (int 3) "normal mlet* evaluation")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1))) (var "x")))
                 (int 1) "single variable mlet* evaluation")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 1)) (cons "x" (int 2))) (var "x")))
                 (int 2) "shadowing mlet* evaluation")
  
   ; ifeq
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 2) (int 3)))
                 (int 2) "simple ifeq true evaluation")
   (check-equal? (eval-exp (ifeq (int 0) (int 1) (int 2) (int 3)))
                 (int 3) "simple ifeq false evaluation")
   (check-equal? (eval-exp (ifeq (add (int 1)(int 1)) (int 2) (int 2) (int 3)))
                 (int 2) "complex ifeq true evaluation")
   (check-equal? (eval-exp (ifeq (add (int 1)(int 1)) (int 1) (int 2) (int 3)))
                 (int 3) "complex ifeq false evaluation")

   ; numex-map
   (check-equal? (eval-exp
                  (call (call numex-map (fun null "x" (add (int 1) (var "x"))))
                   (apair (int 1) (apair (int 2) (munit)))))
                 (apair (int 2) (apair (int 3) (munit))) "map normal list")
   (check-equal? (eval-exp
                  (call (call numex-map (fun null "x" (add (int 1) (var "x"))))
                   (apair (int 1) (munit))))
                 (apair (int 2) (munit)) "map single item list")
   (check-equal? (eval-exp
                  (call (call numex-map (fun null "x" (add (int 1) (var "x"))))
                   (munit)))
                 (munit) "map empty list")








   ))

(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade.txt" #:exists 'replace))
(pretty-write (- 100 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)
