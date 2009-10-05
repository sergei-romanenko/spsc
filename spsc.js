
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
function remove_ws() {
	var code = document.getElementById('code').value;
	var code2 = code.replace(/\s*/g, '');
	alert(code2);
}

var P = Parser;
var g = {
	v_name: P.token(/^[a-z]\w*/),
	c_name: P.token(/^[A-Z]\w*/),
	g_name: P.token(/^g\w*/),
	f_name: P.token(/^f\w*/),
	lparen: P.token(/^\(/),
	rparen: P.token(/^\)/),
	comma:  P.token(/^,/),
	empty:  P.token('')
};

g.pattern = P.process(P.and([g.c_name, g.lparen, P.repeat0_del(g.v_name, g.comma), g.rparen]),
				function(result) {return {'type': 'PAT', 'name': result[0],'args': result[2]};});
g.f_lhs = P.process(P.and([g.f_name, g.lparen, P.repeat1_del(g.v_name, g.comma), g.rparen]),
				function(result) {return {'type': 'FLHS', 'name': result[0], 'args': result[2]};});
g.g_lhs = P.process(P.and([g.g_name, g.lparen, g.pattern, 
                                     P.or([
                                           	P.process(P.and([g.comma, P.repeat1_del(g.v_name, g.comma)]), function(r){return r[1];}), 
                                           	P.process(g.empty, function(r){return[];})
                                           ]), 
                                     g.rparen]),
				function(result) {return {'type': 'GLHS', 'name': result[0], 'pattern': result[2], 'args': result[3]};});

var p = {};

(function() {
var o = (p.rules = {
	vrb: function(t) {
		return P.process(g.v_name, function(v){return {'type': 'VRB', 'name': v};})(t);
	},
	ctr: function(t) {
		return P.process(P.and([g.c_name, g.lparen, P.repeat0_del(o.term, g.comma), g.rparen]),
					function(result) {return {'type': 'CTR', 'name': result[0], 'args': result[2]};})(t);
	},
	term: function(t) {
		return P.or([o.ctr, o.vrb])(t);
	}
});
})();


var test1 = function () {
	return p.rules.term('A(A())');
}

var test2 = function () {
	return p.rules.term('a');
}
