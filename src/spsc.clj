(ns spsc)

(defn err [msg] (throw (new Exception (str msg))))

(defn kind
	"kind of expression - one of the following: variable / f-call / g-call / constructor"
	[expr]
	(if (symbol? expr)
		(if (Character/isLowerCase (first (name expr)))
			'variable
			(err (str "Unexpected expression: " expr)))
		(let [fun (name (first expr))]
			(cond
				(= "let" fun) 'let-expr
				(.startsWith fun "f-") 'f-call
				(.startsWith fun "g-") 'g-call
				(Character/isUpperCase (first fun)) 'constructor
				:else (err (str "Unexpected expression:" expr))))))
		
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

;; interpreter stuff			
(defn unfold-f-call
	[f-call defs]
	(let [ [f-name & args] f-call, [[_ & f-args] f-body] (get-f-fun defs f-name)] 
		(apply-sub (zipmap f-args args) f-body)))
	
(defn unfold-g-call
	[g-call defs]
	(let [ [g-name [c-name & cas] & args] g-call, [[_ [_ & c-args] & g-args ] g-body] (get-g-fun defs g-name c-name)]
		(apply-sub (zipmap (concat c-args g-args) (concat cas args)) g-body)))
		
(defn inter
	"interpret expr in the context of defs"
	[expr defs]
	(cond
		(= 'variable (kind expr)) expr
		(= 'f-call (kind expr)) (inter (unfold-f-call expr defs) defs)
		(= 'g-call (kind expr)) (inter (unfold-g-call expr defs) defs)
		(= 'constructor (kind expr)) (let [[c-name & c-args] expr] (cons c-name (map #(inter % defs) c-args)))))

;; finding substitution
(defn find-sub-seq)	

(defn find-sub
	([e1 e2] (find-sub e1 e2 {}))
	([e1 e2 sub]
	(let [kind1 (kind e1), kind2 (kind e2)]
		(if (= 'variable kind1)
			(cond
				(not (contains? sub e1)) (merge sub {e1 e2})
				(= (sub e1) e2) sub 
				:else nil)
			(let [ [f1 & args1] e1, [f2 & args2] e2] 
				(if (= f1 f2) (find-sub-seq args1 args2 sub) nil))))))
			
(defn find-sub-seq
	[es1 es2 sub]
	(cond
		(nil? sub) nil 
		(not (= (count es1) (count es2))) nil
		(empty? es1) sub
		:else (find-sub-seq (rest es1) (rest es2) (find-sub (first es1) (first es2) sub))
		))
		
(defn vars
	[expr]
	(if (= 'variable (kind expr)) 
		(list expr)
		(reduce #(concat %1 (remove (set %1) %2)) () (map vars (rest expr)))))
		
(defn trivial?
	[expr]
	(let [k (kind expr)]
		(not (or (= 'f-call k) (= 'g-call k)))))
		
(defn inst?
	[e1 e2]
	(not (nil? (find-sub e1 e2))))
	
(defn equiv?
	[e1 e2]
	(and (inst? e1 e2) (inst? e2 e1)))