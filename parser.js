var Parser = {
	
	parse_error: function (text, re) { 
    	this.message = "cannot match '" + text.substring(0, 10) + " ...' against " + re ; 
	},
	
	token: function(re) {
		return function(text) {
			var mx = text.match(re);
			if (mx) { 
				return ([mx[0], text.substring(mx[0].length) ]); 
			} else { 
				throw new Parser.parse_error(text, re); 
			}
		};
	},
	repeat1: function(rule) {
		return function(text) {
			var mx = rule.call(this, text);
			var s = mx[1];
			var result = [mx[0]];
			while (s.length > 0) {
				try {
					r1 = rule.call(this, s);
					result.push(r1[0]);
					s = r1[1];
				} catch (e) {
					return [result, s];
				}
			}
			return [result, s];
		};
	},
	repeat0: function(rule) {
		return function(text) {
			var result = [];
			var s = text;
			try {
				mx = rule.call(this, text);
				result.push(mx[0]);
				s = mx[1];
			} catch (e) {
				return [[], text];
			}
			while (s.length > 0) {
				try {
					r1 = rule.call(this, s);
					result.push(r1[0]);
					s = r1[1];
				} catch (e) {
					return [result, s];
				}
			}
			return [result, s];
		};
	},
	repeat0_del: function(rule, del_rule) {
		return function(text) {
			try {
				mx1 = rule.call(this, text);
				var result = [mx1[0]];
				var s = mx1[1];
				mx2 = Parser.repeat0(Parser.and([del_rule, rule]))(s);
				rs = mx2[0];
				for (var i = 0; i < rs.length; i ++) {
					result.push(rs[i][1]);
				}
				return [result, mx2[1]];
			} catch (e) {
				return [[], text];
			}
		};
	},
	repeat1_del: function(rule, del_rule) {
		return function(text) {
			mx1 = rule.call(this, text);
			var result = [mx1[0]];
			var s = mx1[1];
			mx2 = Parser.repeat0(Parser.and([del_rule, rule]))(s);
			rs = mx2[0];
			for (var i = 0; i < rs.length; i ++) {
				result.push(rs[i][1]);
			}
			return [result, mx2[1]];
		};
	},
	and: function(rules) {
		return function(text) {
			var result = [];
			var s = text;
			for (var i = 0; i < rules.length ; i++) {
				var mx = rules[i].call(this, s);
				result.push(mx[0]);
				s = mx[1];
			}
			return [result, s];
		};
	},	
	or: function(rules) {
		return function(text) {
			for (var i = 0; i < rules.length ; i++) {
				try {
					return rules[i].call(this, text);
				} catch (e) {
					//ignore
				}
			}
			throw new Parser.parse_error(text, '');
		};
	},
	process: function(rule, fn) {
		return function(text) {
			var mx = rule.call(this, text);
			return [fn(mx[0]), mx[1]];
		};
	}
};