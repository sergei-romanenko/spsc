var node = function(exp, contraction) {
	return {
		exp: exp, 
		contraction: contraction,
		children: [],
		ancestors: function () {
			if (this.parent) {
				return [this.parent].concat(this.parent.ancestors());
			} else {
				return [];
			}
		},
		leaves: function () {
			var ls = [];
			if (this.children.length > 0) {
				for (var i = 0; i< this.children.length; i++) {
					ls.push(this.children[i].leaves());
				}
				return Array.prototype.concat.apply([], ls);
			} else {
				return [this];
			}
		},
		is_processed: function () {
			switch (this.exp.kind) {
			case 'Variable':
				return true;
			case 'Constructor':
				return this.exp.args.length == 0;
			case 'FCall':
			case 'GCall':
				return this.get_functional_node() != null;
			default:
				return false;
			}
		},
		get_functional_node: function(){
			switch (this.exp.kind) {
			case 'FCall':
			case 'GCall':
				var ancs = this.ancestors();
				for (var i = 0; i < ancs.length; i ++) {
					if (ancs[i].exp.kind == this.exp.kind && sll_algebra.equiv(this.exp, ancs[i].exp)) {
						return ancs[i];
					}
				}
			default: return null;
			}
		},
		toString: function(indent) {
			var ind = indent || '';
			var chs = [];
			for (var i = 0; i < this.children.length; i++) {
				chs.push(this.children[i].toString(ind + '    '));
			}
			return [ind + '|__' + this.exp.toString()].concat(chs).join('\n ');
		}
	};
};

var tree = function(exp) {
	return {
		root: node(exp, null),
		// tc = [exp, contraction]*
		add_children: function(n, tc) {
			for (var i = 0; i < tc.length; i++) {
				var child_node = node(tc[i][0], tc[i][1]);
				child_node.parent = n;
				n.children.push(child_node);
			}
			return this;
		},
		leaves: function() {
			return this.root.leaves();
		},
		get_unprocessed_leaf: function() {
			var all_leaves = this.leaves();
			for (var i = 0; i < all_leaves.length; i++) {
				if (!all_leaves[i].is_processed()) {
					return all_leaves[i];
				}
			}
			return null;
		},
		replace: function(n, exp) {
			if (n == this.root) {
				this.root = node(exp, null);
			} else {
				var new_node = node(exp, node.contraction);
				new_node.parent = n.parent;
				for (var i = 0; i < n.parent.children.length; i++) {
					if (n.parent.children[i] == n) {
						n.parent.children[i] = new_node;
					}
				}
			}
		},
		toString: function() {
			return this.root.toString();
		}
	};
};