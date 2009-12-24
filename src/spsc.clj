(ns spsc)

(defn err
	[msg]
	(throw (new Exception (str msg))))

(defn kind
	[expr]
	(if (symbol? expr)
		(if (Character/isLowerCase (first (str expr)))
			'variable
			(err (str "Unexpected expression: " expr)))
		(let [[h] expr]
			(cond
				(.startsWith (name h) "f-") 'f-call
				(.startsWith (name h) "g-") 'g-call
				(Character/isUpperCase (first (name h))) 'constructor
				:else (err (str "Unexpected expression:" expr))
				))))
		
(defn get-f-fun 
	[defs f-name]
	(first (filter #(let [ [[n :as expr]] %] (and (= 'f-call (kind expr)) (= f-name n))) defs)))
	
(defn get-g-fun
	[defs g-name c-name]
	(first (filter #(let [ [[n c :as expr]] %] (and (= 'g-call (kind expr)) (= g-name n) (= c-name (first c)))) defs)))
	
(defn apply-sub
	[sub expr]
	(if (= 'variable (kind expr))
		(sub expr expr)
		(cons (first expr) (map #(apply-sub sub %) (rest expr)))))
			
(defn unfold-f-call
	[f-call defs]
	(let [ [f-name & args] f-call, [[_ & f-args] f-body] (get-f-fun defs f-name)] 
		(apply-sub (zipmap f-args args) f-body)))
	
(defn unfold-g-call
	[g-call defs]
	(let [ [g-name [c-name & cas] & args] g-call, [[_ [_ & c-args] & g-args ] g-body] (get-g-fun defs g-name c-name)]
		(apply-sub (zipmap (concat c-args g-args) (concat cas args)) g-body)))
		
(defn inter 
	[expr defs]
	(let [k (kind expr)]
		(cond
			(= 'variable k) expr
			(= 'f-call k) (inter (unfold-f-call expr defs) defs)
			(= 'g-call k) (inter (unfold-g-call expr defs) defs)
			(= 'constructor k) (let [[c-name & c-args] expr] (cons c-name (map #(inter % defs) c-args))) 
			)))

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
	)