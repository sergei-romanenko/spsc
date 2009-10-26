var he = {
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