var residuator = function(tree) {
	var function_counter = 0;
	var defs = [];
	
	return {
		residuate: function() {
			defs = [];
			function_counter = 0;
			var new_goal = this.walk(tree.root);
			var new_program = sll_lang.program(defs);
			return [new_goal, new_program];
		},
		
		
		walk: function(node) {
			var exp = node.exp;
			switch (exp.kind) {
			case 'Variable': 
				return exp;
			case 'Let':
				var body = this.walk(node.children[0]);
				var map = {};
				for (var i = 0; i < exp.bindings.length; i++) {
					map[exp.bindings[i][0]] = this.walk(node.children[i + 1]);
				}
				return sll_algebra.apply_subst(body, map);
			case 'Constructor':
				var args = [];
				for (var i = 0; i < node.children.length; i++) {
					args[i] = this.walk(node.children[i]);
				}
				return sll_lang.constructor(exp.name, args);
			case 'FCall':
			case 'GCall':
				var func_node = node.get_functional_node();
				if (func_node) {
					var map = sll_algebra.match_against(func_node.exp, node.exp);
					if (func_node.children[0].contraction) {
						return sll_algebra.apply_subst(sll_lang.fcall(func_node.sig[0], func_node.sig[1]), map);
					} else {
						return sll_algebra.apply_subst(sll_lang.gcall(func_node.sig[0], func_node.sig[1]), map);
					}
				} else {
					return this.walkCall(node, exp.name, exp.args);
				}
			}
		},
		
		walkCall: function (node, name, args) {
			var vs = sll_algebra.vars(node.exp);
			if (node.children[0].contraction) {
				
				var gname = this.new_function_name(name, 'g'); 
				var sig = [gname, vs];
				node.sig = sig;
				
				for (var i = 0; i < node.children.length; i++) {
					var cn = node.children[i];
					defs.push(sll_lang.grule(gname, cn.contraction[1], vs.slice(1), this.walk(cn)));
				}
				
				return sll_lang.gcall(gname, vs);
				
			} else {
				
				var func_node = false;
				for (var i = 0; i< tree.leaves().length; i ++) {
					var leaf = tree.leaves()[i];
					if (leaf.get_functional_node() == node) {
						func_node = true;
						break;
					}
				}
				
				if (func_node) {
					var fname = this.new_function_name(name, 'f');
					var sig = [fname, vs];
					node.sig = sig;
					defs.push(sll_lang.frule(fname, vs, this.walk(node.children[0])));
					return sll_lang.fcall(fname, vs);
				} else {
					return this.walk(node.children[0]);
				}
				
			}
		},
		
		// kind is 'f' or 'g' - used as a prefix
		new_function_name: function(original_name, kind) {
			function_counter++;
			return kind + original_name.slice(1) + function_counter;
		}
	};
};