#lang racket

;; syntax: https://docs.racket-lang.org/rackunit/api.html

(require "solution.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

(require rackunit)

(define envList (list (cons "x" (int 2))
					  (cons "y" (int 3))
					  (cons "z" (int 4))
					  (cons "p" (int 5))
					  (cons "q" (int 6))
					  (cons "t" (int 7))
					  (cons "a" (int 8))
					  (cons "b" (int 9))
					  (cons "u" (int 10))
					  (cons "s" (int 11))))

(define tests
  (test-suite
   "Project Tests - Challenging Part"

   ;1
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun "f" "a" (add (var "a") (var "u"))))) 
				(set "u") 
				"compute-free-vars test #1")
					
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun "f" "a" (mult (var "t") (var "u")))))
				(set "t" "u") 
				"compute-free-vars test #2")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun "f" "a" (neg (mult (var "t") (var "u"))))))
				(set "t" "u") 
				"compute-free-vars test #3")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "a" (var "t"))))
				(set "t") 
				"compute-free-vars test #4")				
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "a" (int 2))))
				(set) 
				"compute-free-vars test #6")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "a" (islthan (add (var "x") (var "y")) (neg (var "y"))))))
				(set "y" "x") 
				"compute-free-vars test #7")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (ifzero (mult (var "x") (var "y")) (neg (var "p")) (add (var "s")(var "t"))))))
				(set "y" "p" "s" "t") 
				"compute-free-vars test #8")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z"))))))
				(set "y" "p" "s" "z") 
				"compute-free-vars test #9")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun "y" "x" (call (fun "z" "p" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z")))) (var "p")))))
				(set "s" "p") 
				"compute-free-vars test #10")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun "y" "x" (fun "z" "p" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z")))))))
				(set "s") 
				"compute-free-vars test #11")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (apair (var "a")(var "b")))))
				(set "a" "b") 
				"compute-free-vars test #12")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (first (apair (var "a")(var "x"))))))
				(set "a") 
				"compute-free-vars test #13")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (second (first (apair (apair (var "a")(var "b"))(munit)))))))
				(set "a" "b") 
				"compute-free-vars test #14")	

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (ismunit (first (apair (apair (var "a")(var "x"))(munit)))))))
				(set "a") 
				"compute-free-vars test #15")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (ismunit (var "a")))))
				(set "a") 
				"compute-free-vars test #16")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (munit))))
				(set) 
				"compute-free-vars test #17")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (mlet "y" (int 2) (add (var "x")(var "y"))))))
				(set) 
				"compute-free-vars test #18")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (mlet "y" (int 2) (mlet "x" (int 3) (add (var "x")(var "y")))))))
				(set) 
				"compute-free-vars test #19")

(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (mlet* (list (cons "y" (int 2)) (cons "p" (int 3))) (add (mult (var "p")(var "q"))(add (var "x")(var "y")))))))
				(set "q") 
				"compute-free-vars test #20")
				
					
(check-equal? 	(closure-env (eval-exp-c (mlet* (list (cons "x" (int 1)) (cons "y" (int 2))) 
				(fun "f" "y" (add (var "x") (var "y"))))))
				(list (cons "x" (int 1)))
				"eval-exp-c test #1")
		
(check-equal? 	(closure-env (eval-exp-c (mlet* (list (cons "w" (int 3)) (cons "x" (int 1)) (cons "y" (int 2))) 
				(fun "f" "y" (add (var "x") (var "y"))))))
				(list (cons "x" (int 1)))
				"eval-exp-c test #2")		
				
(check-equal? 	(closure-env (eval-exp-c (mlet* (list (cons "w" (int 3)) (cons "x" (int 1)) (cons "y" (int 2))) 
				(fun "f" "y" (add (var "x") (var "y"))))))
				(list (cons "x" (int 1)))
				"eval-exp-c test #3")
				

;(check-equal? 	(closure-env (eval-exp (mlet* (list (cons "w" (int 3)) (cons "x" (int 1)) (cons "y" (int 2))) 
;				(fun "f" "y" (add (var "x") (var "y"))))))
;				(list (cons "y" (int 2)) (cons "x" (int 1)) (cons "w" (int 3)))
;				"eval-exp-c test #4")	


(check-equal? 	(closure-env (eval-exp-c (fun null "x" (mlet "y" (int 2) (mlet "x" (int 3) (add (var "x")(var "y")))))))
				'() 
				"eval-exp-c test #4")	
				
(check-equal? 	(closure-env (eval-exp-c (mlet* (list (cons "u" (int 3)) (cons "w" (int 2)))(fun "f" "a" (add (var "a") (var "u"))))))
				(list (cons "u" (int 3)))
				"eval-exp-c test #5")
					
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun "f" "a" (mult (var "t") (var "u"))))))
				(list (cons "u" (int 10)) (cons "t" (int 7))) ;OR any permutation
				"eval-exp-c test #6")
					
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun "f" "a" (add (call (fun null "a" (neg (var "a"))) (int 2)) (var "b"))))))
				(list (cons "b" (int 9)))
				"eval-exp-c test #7")					
					
					
					
					
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun "f" "a" (neg (mult (var "t") (var "u")))))))
				(list (cons "u" (int 10)) (cons "t" (int 7))) ;OR any permutation
				"eval-exp-c test #8")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (var "t")))))
				(list (cons "t" (int 7)))
				"eval-exp-c test #9")				
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (int 2)))))
				'() 
				"eval-exp-c test #10")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (islthan (add (var "x") (var "y")) (neg (var "y")))))))
				(list (cons "y" (int 3)) (cons "x" (int 2))) ;OR any permutation
				"eval-exp-c test #11")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (ifzero (mult (var "x") (var "y")) (neg (var "p")) (add (var "s")(var "t")))))))
				(list (cons "s" (int 11)) (cons "t" (int 7))(cons "p" (int 5))(cons "y" (int 3))) ;OR any permutation
				"eval-exp-c test #12")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z")))))))
				
				(list (cons "s" (int 11))(cons "p" (int 5)) (cons "z" (int 4))(cons "y" (int 3))) ;OR any permutation
				"eval-exp-c test #9")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun "y" "x" (call (fun "z" "p" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z")))) (var "p"))))))
				
				(list (cons "s" (int 11))(cons "p" (int 5))) ;OR any permutation
				"eval-exp-c test #10")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun "y" "x" (fun "z" "p" (ifgthan (neg (mult (var "x") (var "y"))) (neg (var "p")) (add (var "s")(var "x")) (add (var "s")(var "z"))))))))
				
				(list (cons "s" (int 11)))
				"eval-exp-c test #11")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (apair (var "a")(var "b"))))))
				
				(list (cons "b" (int 9))(cons "a" (int 8)) ) ;OR any permutation
				"eval-exp-c test #12")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (first (apair (var "a")(var "x")))))))
				
				(list (cons "a" (int 8)))
				"eval-exp-c test #13")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (second (first (apair (apair (var "a")(var "b"))(munit))))))))
				(list (cons "b" (int 9))(cons "a" (int 8)) ) ;OR any permutation
				"eval-exp-c test #14")	

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (ismunit (first (apair (apair (var "a")(var "x"))(munit))))))))
				(list (cons "a" (int 8)))
				"eval-exp-c test #15")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (ismunit (var "a"))))))
				(list (cons "a" (int 8)))
				"eval-exp-c test #16")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (munit)))))
				'()
				"eval-exp-c test #17")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (mlet "y" (int 2) (add (var "x")(var "y")))))))
				'()
				"eval-exp-c test #18")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (mlet "y" (int 2) (mlet "x" (int 3) (add (var "x")(var "y"))))))))
				'()
				"eval-exp-c test #19")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (mlet* (list (cons "y" (int 2)) (cons "p" (int 3))) (add (mult (var "p")(var "q"))(add (var "x")(var "y"))))))))
				(list (cons "q" (int 6)))
				"eval-exp-c test #20")
					
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (mlet "f1"
                               (fun "f1" "a" (mlet "x" (var "a") (fun "f2" "z" (add (var "x") (var "s")))))
                               (mlet "f3" (fun "f3" "f" (mlet "x" (int 1729) (call (var "f") (munit)))) 
                                     (call (var "f3") (call (var "f1") (var "p")))))))))
				(list (cons "s" (int 11))(cons "p" (int 5)) );OR any permutation
				"eval-exp-c test #21")
	
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" (mlet "fnc"
       (fun "f1" "x" 
            (ifgthan (ismunit (var "x")) (var "p") 
                       (int 0) 
                       (add (first (var "x")) (call (var "f1") (second (var "x"))))))
       (call (var "fnc") (apair (var "s") (apair (var "s") (apair (var "b") (munit))))))))))
				
				(list  (cons "s" (int 11)) (cons "b" (int 9))(cons "p" (int 5)));OR any permutation
				"eval-exp-c test #22")	
				
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" 				
     (mlet "range"
           (fun "range" "lo"
                (fun null "hi"
                     (ifgthan (var "t") (var "hi") (munit)
                                (apair (var "lo") (call (call (var "range") (add (int 1) (var "p"))) (var "hi"))))))
           (call (call (var "u") (int 5)) (int 8)))))))
		   (list  (cons "u" (int 10)) (cons "t" (int 7))(cons "p" (int 5)));OR any permutation
				"eval-exp-c test #23")

(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "x" 	
(call (fun "a" "b" (ifgthan (var "b") (var "u") (add (var "b") (int 3))
                         (call (var "a") (mult (var "t") (int 3))  ))) (var "s"))))))
			(list  (cons "s" (int 11))(cons "u" (int 10)) (cons "t" (int 7)));OR any permutation
			"eval-exp-c test #24")
			
			
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (neg (var "a"))))))
				'() 
				"eval-exp-c test #25")				
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (add (ismunit (var "b")) (var "c"))))))
				(list  (cons "b" (int 9))) 
				"eval-exp-c test #26")
				
(check-equal? 	(fun-challenge-freevars (compute-free-vars (fun null "x" (add (ismunit (var "b")) (var "c")))))
				(set "b" "c") 
				"compute-free-vars test #26-2")
				
(check-equal? 	(closure-env (eval-exp-c (mlet* envList (fun null "a" (ifmunit (var "b") (var "p")(var "t"))))))
				(list  (cons "b" (int 9)) (cons "t" (int 7))(cons "p" (int 5)));OR any permutation
				"eval-exp-c test #27")				
				
   ))

(require rackunit/text-ui)
(require rackunit/log)
;; runs the test
;(run-tests tests)


(define result (run-tests tests))

(define out (open-output-file "grade-Challenging.txt" #:exists 'replace))
(pretty-write (- 100 result) out #:newline? #t)
(pretty-write (test-log) out #:newline? #f)
(close-output-port out)

;(define out2 (open-output-file "summary.txt" #:exists 'replace))
;(write (test-log) out2)
;(close-output-port out2)
