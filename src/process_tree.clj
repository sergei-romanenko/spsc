(ns process-tree)

(defstruct contraction :var :pattern)
(defstruct tree-node :expr :parent :contraction)
(defstruct tree :root :children-map) 

; node structure
(defn ancestor-nodes
	[tn])
	
(defn processed?
	[tn])
	
(defn f-node
	[tn])


; tree structure
(defn add-children
	[t tn ecs]
	(struct tree (:root t) (merge (:children-map t) {tn (map #(let [[e c] ecs] (struct tree-node e tn c) ecs))} )))

(defn replace-node
	[t tn e]
	(if (= tn (:root t))
		(struct tree (struct tree-node e) {})
	))

(defn leaves
  ([t] (leaves t (:root t)))
  ([t tn] 
  	(let [ch-nodes ((:children t) tn)] 
  		(if (nil? ch-nodes) (list tn) (mapcat #(leaves t %) ch-nodes)))))