//////////////////////////
//
// Basic Parser Combinator
//
//////////////////////////

var parser = {
	Success: function (result, next) {
		this.result = result;
		this.next = next;
		this.successful = true;
	},
	Error: function (s, re) { 
    	this.message = "cannot match '" + s.substring(0, 10) + " ...' against " + re ;
    	this.successful = false;
	},
	token: function(re) {
		return function(text) {
			var mx = text.match(re);
			return mx ? new parser.Success(mx[0], text.substring(mx[0].length)) : new parser.Error(text, re);
		};
	},
	repeat: function(rule) {
		return function(text) {
			var final_result = [];
			var s = text;
			var pr;
			while (s.length > 0) {
				pr = rule.call(this, s);
				if (pr.successful) {
					final_result.push(pr.result);
					s = pr.next;
				} else {
					break;
				}
			}
			return new parser.Success(final_result, s);
		};
	},
	repeat_sep: function(rule, del_rule) {
		return function(text) {
			var pr1 = rule.call(this, text);
			if (!pr1.successful) {
				return new parser.Success([], text);
			}
			var pr2 = parser.repeat(parser.and([del_rule, rule]))(pr1.next);
			var result = [pr1.result];
			for (var i = 0; i < pr2.result.length; i++) {
				result.push(pr2.result[i][1]);
			}
			return new parser.Success(result, pr2.next);
		};
	},
	and: function(rules) {
		return function(text) {
			var result = [], s = text, pr = null;
			for (var i = 0; i < rules.length ; i++) {
				pr = rules[i].call(this, s);
				if (!pr.successful) {
					return pr;
				}
				result.push(pr.result);
				s = pr.next;
			}
			return new parser.Success(result, pr.next);
		};
	},	
	or: function(rules) {
		return function(text) {
			for (var i = 0; i < rules.length ; i++) {
				var pr = rules[i].call(this, text);
				if (pr.successful) {break;}
			}
			return pr;
		};
	},
	transform: function(rule, fn) {
		return function(text) {
			var pr = rule.call(this, text);
			return pr.successful ? new parser.Success(fn(pr.result), pr.next) : pr;
		};
	}
};

//////////////////////////
//
// SLL Abstract Syntax
//
//////////////////////////

var sll_lang = {
		Pattern: function(name, args) {
			this.kind = 'Pattern';
			this.name = name;
			this.args = args;
		},
		Variable: function (name) {
			this.kind = 'Variable';
			this.name = name;
			this.args = [];
		},
		Constructor: function (name, args) {
			this.kind = 'Constructor';
			this.name = name;
			this.args = args;
		},
		FCall: function (name, args) {
			this.kind = 'FCall';
			this.name = name;
			this.args = args;
		},
		GCall: function (name, args) {
			this.kind = 'GCall';
			this.name = name;
			this.args = args;
		},
		FRule: function (name, args, exp) {
			this.kind = 'FRule';
			this.name = name;
			this.args = args;
			this.exp = exp;
		},
		GRule: function (name, pattern, args, exp) {
			this.kind = 'GRule';
			this.name = name;
			this.pattern = pattern;
			this.args = args;
			this.exp = exp;
		},
		Program: function (rules) {
			this.kind = 'Program';
			this.rules = rules;
			this.f = {};
			this.g = {};
			this.gs = {};
			for (var i = 0; i < rules.length; i++) {
				var rule = rules[i];
				switch (rule.kind) {
				case 'FRule': 
					this.f[rule.name] = rule;
					break;
				case 'GRule':
					this.g[rule.name + '_' + rule.pattern.name] = rule;
					if (!this.gs[rule.name]) {
						this.gs[rule.name] = [];
					}
					this.gs[rule.name].push(rule);
					break;
				}
			}
		}
};

var sll_algebra = {
	// the generic method for testing equality
	// of all sll syntax objects
	equals:  function(e1, e2) {
		var sh_eq = (e1.kind == e2.kind) && (e1.name == e2.name);
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
		return {kind: exp.kind, name: exp.name, args: args};
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
	}
};

//////////////////////////
//
// SLL Parser
//
//////////////////////////

var tokens = {
	v_name: parser.token(/^[a-z]\w*/), c_name: parser.token(/^[A-Z]\w*/),
	g_name: parser.token(/^g\w*/), f_name: parser.token(/^f\w*/),
	lparen: parser.token(/^\(/), rparen: parser.token(/^\)/),
	eq: parser.token(/^=/), comma: parser.token(/^,/),
	semicolon: parser.token(/^;/), eof: parser.token(/^$/)
};

var t = tokens, p = parser;

var sll_parser = {
	ptr: 
		function(s) { 
			var p_par = 
				p.transform(
					p.and([t.c_name, t.lparen, p.repeat_sep(sll_parser.vrb, t.comma), t.rparen]),
					function(r) {return new sll_lang.Pattern(r[0], r[2]);}
				);
			return p_par(s);
		},
	vrb: 
		function(s) {
			var v_par = 
				p.transform(
					t.v_name,
					function(r) {return new sll_lang.Variable(r)}
				);
			return v_par(s);
		},
	ctr:
		function(s) {
			var c_par = 
				p.transform(
					p.and([t.c_name, t.lparen, p.repeat_sep(sll_parser.exp, t.comma), t.rparen]),
					function(r) {return new sll_lang.Constructor(r[0], r[2]);}
				);
			return c_par(s);
		},
	fcall:
		function(s) {
			var f_par = 
				p.transform(
					p.and([t.f_name, t.lparen, p.repeat_sep(sll_parser.exp, t.comma), t.rparen]),
					function(r) {return new sll_lang.FCall(r[0], r[2]);}						
				);
			return f_par(s);
		},
	gcall:
		function(s) {
			var g_par = 
				p.transform(
					p.and([t.g_name, t.lparen, p.repeat_sep(sll_parser.exp, t.comma), t.rparen]),
					function(r) {return new sll_lang.GCall(r[0], r[2]);}						
				);
			return g_par(s);
		},
	exp: 
		function(s) {
			var t_par = p.or([sll_parser.ctr, sll_parser.fcall, sll_parser.gcall, sll_parser.vrb]);
			return t_par(s);
		},
	frule:
		function(s) {
			var f_par = 
				p.transform(
					p.and([t.f_name, 
							t.lparen, 
							p.repeat_sep(sll_parser.vrb, t.comma), 
							t.rparen, 
							t.eq, 
							sll_parser.exp, 
							t.semicolon]),
					function(r) {return new sll_lang.FRule(r[0], r[2], r[5]);}
				);
			return f_par(s);
		},
	grule:
		function(s) {
			var g_par = 
				p.transform(
					p.and([t.g_name, 
							t.lparen, 
							sll_parser.ptr, 
							p.repeat(p.and([t.comma, sll_parser.vrb])), 
							t.rparen, 
							t.eq, 
							sll_parser.exp, 
							t.semicolon]),
					function(r) {
						var vars = [];
						for (var i = 0; i < r[3].length; i++) {
							vars.push(r[3][i][1]);
						}
						return new sll_lang.GRule(r[0], r[2], vars, r[6]);
					}
				);
			return g_par(s);
		},
	program:
		function(s) {
			var p_par = 
				p.transform(
					p.and([p.repeat( p.or([sll_parser.frule, sll_parser.grule])), t.eof]),
					function (r) {return new sll_lang.Program(r[0]);}
				);
			return p_par(s);
		},
	parse:
		function(s) {
			return sll_parser.program(s.replace(/\s*/g, ''));
		}
};
