var he = {
	
	smart_he: function(exp1, exp2) {
		return this.var_attacked(exp1) == this.var_attacked(exp2) && this.he(exp1, exp2);
	},
	
	he: function(exp1, exp2) {
		return this.he_by_diving(exp1, exp2) || this.he_by_coupling(exp1, exp2);
	},
	
	he_by_diving: function(exp1, exp2) {
		switch (exp2.kind) {
		case 'Constructor':
		case 'FCall':
		case 'GCall':
			for (var i = 0; i < exp2.args.length; i++) {
				if (this.he(exp1, exp2.args[i])) {
					return true;
				}
			}
			return false;
		default:
			return false;
		}
	},
	
	he_by_coupling: function(exp1, exp2) {
		if (exp1.kind == 'Let' || exp2.kind == 'Let') {
			return false;
		}
		if (exp1.kind == 'Variable' && exp2.kind == 'Variable') {
			return true;
		}
		if (sll_algebra.shell_equals(exp1, exp2) && exp1.args.length == exp2.args.length) {
			for (var i = 0; i < exp1.args.length; i++) {
				if (!this.he(exp1.args[i], exp2.args[i])) {
					return false;
				}
			}
			return true;
		}
		return false;
	},
	
	var_attacked: function(exp) {
		switch(exp.kind) {
		case 'Variable':
			return true;
		case 'GCall':
			return this.var_attacked(exp.args[0]);
		default:
			return false;
		}
	}
};