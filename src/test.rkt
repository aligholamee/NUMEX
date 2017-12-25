;; CASE 1: Recursion | Function Call | Parameter Passing
(define program4 (call (fun "func1" "arg1" (ifzero (var "arg1") (int 4) (mlet "insider" (add (var "arg1") (int -1)) (add (call (var "func1") (var "insider")) (int 1))))) (int 5)))

;; CASE 2: mlet handling
(define program5 (mlet "a" (int 1) (mlet "b" (int 2) (add (var "a") (var "b")))))