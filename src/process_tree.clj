(ns process-tree
	(:use [spsc] [process-tree]))

(defstruct contraction :var :pattern)
(defstruct tree-node :id :parent-id :expr :contraction)
(defstruct tree :root-id :node-map :children-map)

(defn ancestor-nodes 
	[t tn]
	(if (nil? (:parent-id tn)) 
		nil 
	(let [pn ((:parent-id tn) (:node-map t))]
		 (cons pn (ancestor-nodes t pn)))))

(defn f-node [t tn]
	(let 
		[exp (:expr tn)]
		(first (filter #(let [e1 (:expr %)] (and (not (trivial? e1) (equiv? exp e1)))) (ancestor-nodes tn)))))

(defn processed?
	[t tn]
	(let [expr (:expr tn), k (kind expr)]
		(cond 
			(= 'variable k) true
			(= 'constructor k) (nil? (rest expr))
			:else (not (nil? (f-node t tn))))))
	
(defn create-tree
	[e]
	(let [n (struct tree-node (gensym) nil e nil)]
	(struct tree (:id n) {(:id n) n} {})))

(defn get-node
	[t node-id]
	(node-id (:node-map t)))	
	
(defn add-children
	[t node-id ecs]
	(let
		[child-nodes (map #(let [[e c] %] (struct tree-node (gensym) node-id e c) ) ecs),
		 new-ids (map :id child-nodes),
		 new-node-map (merge (:node-map t) (zipmap new-ids child-nodes)),
		 new-children-map (merge (:children-map t) {node-id new-ids})]
		(struct tree (:root-id t) new-node-map new-children-map)))

(defn replace-node
	[t node-id e]
	(if (= node-id (:root-id t)) 
		(create-tree e)
		(let 
			[node-map (:node-map t),
			children-map (:children-map t),
			node-old (node-id node-map),
			parent-id (:parent-id node-old),
			node-new (struct tree-node (gensym) parent-id e (:contraction node-old)),
			children-old (parent-id children-map),
			id-new (:id node-new),
			children-new (replace {node-id id-new} children-old)]
			(struct tree (:root-id t) (assoc (dissoc node-map node-id) id-new node-new) (assoc children-map parent-id children-new)))))

(defn leaves-ids
  ([t] (leaves-ids t (:root-id t)))
  ([t node-id] 
  	(let [ch-ids (node-id (:children-map t))] 
  		(if (nil? ch-ids) (list node-id) (mapcat #(leaves-ids t %) ch-ids)))))