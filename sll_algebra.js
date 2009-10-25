var sll_algebra = {
	shell_equals: function(e1, e2) {
		return (e1.kind == e2.kind) && (e1.name == e2.name);
	},
		
	equals:  function(e1, e2) {
		var sh_eq = this.shell_equals(e1, e2);
		if (!sh_eq) {
			return false;
		}
		if (e1.args.length != e2.args.length) {
			return false;
		}
		for (var i = 0; i < e1.args.length; i++) {
			if (!this.equals(e1.args[i], e2.args[i])) {
				return false;
			}
		}
		if (e1.pattern && !this.equals(e1.pattern, e2.pattern)) {
			return false;
		}
		if (e1.exp && !this.equals(e1.exp, e2.exp)) {
			return false;
		}
		return true;
	},

	replace_args: function(exp, args) {
		return {kind: exp.kind, name: exp.name, args: args, toString: exp.toString};
	},
	
	apply_subst: function(exp, map) {
		switch (exp.kind) {
		case 'Variable':
			return map[exp.name] || exp;
		default:
			var args = [];
			for (var i = 0; i < exp.args.length; i++) {
				args.push(this.apply_subst(exp.args[i], map));
			}
			return this.replace_args(exp, args);
		}
	},
	
	match_against: function(exp1, exp2) {
		var that = this;
		var map = {};
		var walk = function(e1, e2) {
			if (e1.kind == 'Variable') {
				if (map[e1.name]) {
					return that.equals(map[e1.name], e2);
				} else {
					map[e1.name] = e2;
					return true;
				}
			}
			if (e2.kind == 'Variable') {
				return false;
			}
			if (!that.shell_equals(e1, e2)) {
				return false;
			}
			for (var i = 0; i < e1.args.length; i++) {
				if ( !walk(e1.args[i], e2.args[i]) ) {
					return false;
				}
			}
			return true;
		}
		if (walk(exp1, exp2)) {
			return map;
		} else {
			return null;
		}
	},
	
	subst_equals: function (sub1, sub2) {
		if (sub1 == null || sub2 == null) {
			return sub1 == sub2;
		}
		for (var n in sub1) {
			if (!sub2[n]) {
				return false;
			}
			if (!this.equals(sub1[n], sub2[n])) {
				return false;
			}
		}
		for (var n in sub2) {
			if (!sub1[n]) {
				return false;
			}
			if (!this.equals(sub2[n], sub1[n])) {
				return false;
			}
		}
		return true;
	},
	
	fresh_var: function () {
		var i = 0;
		return function () {
			i++;
			return sll_lang.variable('v_' + i);
		};
	}(),
	
	// test whether e2 is an instance of e1
	instance_of: function(e1, e2) {
		return this.match_against(e1, e2) != null;
	},
	
	equiv: function(e1, e2) {
		return this.instance_of(e1, e2) && this.instance_of(e2, e1);
	},
	
	vars: function(exp) {
		switch (exp.kind) {
		case 'Variable':
			return [exp];
		default:
			var names = {}, all_vars = [];
			for (var i = 0; i < exp.args.length; i++) {
				all_vars = all_vars.concat(this.vars(exp.args[i]));
			}
			// now we need to remove duplicates
			var unique_vars = [], gathered_names = {};
			for (var i = 0; i < all_vars.length; i++) {
				var v = all_vars[i];
				if (!gathered_names[v.name]) {
					gathered_names[v.name] = true;
					unique_vars.push(v);
				}
			}
			return unique_vars;
		}
	}
};