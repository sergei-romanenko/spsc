(ns spsc-test
	(:use [spsc]))
	
(def prog1
	'(((f-f x) 
		x)
	((g-app (Cons x xs) ys)
		(Cons x (g-app xs ys)))
	((g-app (Nil) ys) 
		ys)))
		
(defn test-me []
	(println (get-g-fun prog1 'g-app 'Cons))
	(println (get-f-fun prog1 'f-f))
	(println (unfold-g-call '(g-app (Cons x (Nil)) (Cons y (Nil))) prog1))
	(println (unfold-g-call '(g-app (Nil) (Cons y (Nil))) prog1))
	(println (unfold-f-call '(f-f z) prog1))
	(println (inter '(g-app (Cons x (Nil)) (Cons y (Nil))) prog1))
	(println (find-sub 'x 'y))
	(println (find-sub '(f-f x y) '(f-f y x)))
	(println (find-sub '(f-f x y) '(f-f x x)))
	(println (find-sub '(f-f x x) '(f-f y x)))
	(println (vars '(f-f (C x y x) x y z)))
	(println "-------------------")
	)

