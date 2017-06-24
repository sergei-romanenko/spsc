var gen = function(exp, m1, m2) {
	return {exp: exp, m1: m1, m2: m2};
};

var msg = {
	msg: function(e1, e2) {
		var v = sll_algebra.fresh_var();
		var m1 = {}, m2 = {};
		m1[v.name] = e1;
		m2[v.name] = e2;
		var g = gen(v, m1, m2);
		var exp;
		do {
			exp = g.exp;
			g = this.common_functor(g);
			g = this.common_subst(g);
		} while (!sll_algebra.equals(exp, g.exp));
		return g;
	},
	
	common_functor: function(g) {
		var exp1, exp2;
		for (var v in g.m1) {
			exp1 = g.m1[v];
			exp2 = g.m2[v];
			if (exp1 && exp2 && 
				exp1.kind != 'Let' && exp2.kind != 'Let' &&
				sll_algebra.shell_equals(exp1, exp2)) {
				
				var fresh_vars = [];
				
				var m1 = this.copy_map(g.m1);
				var m2 = this.copy_map(g.m2);
				for (var i = 0; i < exp1.args.length; i++) {
					var fresh_var = sll_algebra.fresh_var();
					fresh_vars.push(fresh_var);
					m1[fresh_var.name] = exp1.args[i];
					m2[fresh_var.name] = exp2.args[i];
				}
				var m = {};
				m[v] = sll_algebra.replace_args(exp1, fresh_vars);
				var exp = sll_algebra.apply_subst(g.exp, m);
				
				delete m1[v]; 
				delete m2[v];
				
				return gen(exp, m1, m2);
			}
		}
		return g;
	},
	
	common_subst: function(g) {
		for (var v1 in g.m1) {
			for (var v2 in g.m1) {
				if (v1 != v2 && g.m2[v1] && g.m2[v2] &&
						sll_algebra.equals(g.m1[v1], g.m1[v2]) &&
						sll_algebra.equals(g.m2[v1], g.m2[v2])) {
					
					var sub = {};
					sub[v1] = sll_lang.variable(v2);
					
					var m1 = this.copy_map(g.m1);
					var m2 = this.copy_map(g.m2);
					
					delete m1[v1];
					delete m2[v1];
					
					var exp = sll_algebra.apply_subst(g.exp, sub);
					return gen(exp, m1, m2);
				}
			}
		}
		return g;
	},
	
	copy_map: function(m) {
		var m1 = {};
		for (var k in m) {
			m1[k] = m[k];
		}
		return m1;
	}
};