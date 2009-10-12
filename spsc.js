//////////////////////////
//
// Basic Parser Combinator
//
//////////////////////////

var parser = {
	success: function (result, next) {
		return {result: result, next: next, successful: true};
	},
	error: function (s, re) { 
		return {successful: false,
				message: "cannot match '" + s.substring(0, 10) + " ...' against " + re};
	},
	token: function(re) {
		return function(text) {
			var mx = text.match(re);
			if (mx) {
				return parser.success(mx[0], text.substring(mx[0].length))
			} else {
				return parser.error(text, re);
			}
		};
	},
	repeat: function(rule) {
		return function(text) {
			var final_result = [], s = text, pr;
			while (s.length > 0) {
				pr = rule.call(this, s);
				if (pr.successful) {
					final_result.push(pr.result);
					s = pr.next;
				} else {
					break;
				}
			}
			return parser.success(final_result, s);
		};
	},
	repeat_sep: function(rule, del_rule) {
		return function(text) {
			var pr1 = rule.call(this, text);
			if (!pr1.successful) {
				return parser.success([], text);
			}
			var pr2 = parser.repeat(parser.and([del_rule, rule]))(pr1.next);
			var result = [pr1.result];
			for (var i = 0; i < pr2.result.length; i++) {
				result.push(pr2.result[i][1]);
			}
			return parser.success(result, pr2.next);
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
			return parser.success(result, pr.next);
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
			return pr.successful ? parser.success(fn(pr.result), pr.next) : pr;
		};
	}
};

//////////////////////////
//
// SLL Abstract Syntax
//
//////////////////////////

var sll_lang = {
		pattern: function(name, args) {
			return {
				kind: 'Pattern', 
				name: name, 
				args: args,
				toString: function() {
					return this.name + '(' + this.args.join(', ') + ')';
				} 
			};
		},
		variable: function (name) {
			return {
				kind: 'Variable',
				name: name,
				args: [],
				toString: function() {
					return this.name;
				} 
			};
		},
		constructor: function (name, args) {
			return {
				kind: 'Constructor',
				name: name,
				args: args,
				toString: function() {
					return this.name + '(' + this.args.join(', ') + ')';
				}
			};
		},
		fcall: function (name, args) {
			return {
				kind: 'FCall',
				name: name,
				args: args,
				toString: function() {
					return this.name + '(' + this.args.join(', ') + ')';
				}
			};
		},
		gcall: function (name, args) {
			return {
				kind: 'GCall',
				name: name,
				args: args,
				toString: function() {
					return this.name + '(' + this.args.join(', ') + ')';
				}
			};
		},
		let: function (exp, bindings) {
			return {
				kind: 'Let',
				exp: exp,
				bindings: bindings,
				toString: function() {
					var s0 = [];
					for (var i = 0; i < this.bindings.length; i++) {
						s0.push(this.bindings[i].join('='));
					}
					return 'let ' + s0.join(', ') + ' in ' + this.exp; 
				}
			};
		},
		frule: function (name, args, exp) {
			return {
				kind: 'FRule',
				name: name,
				args: args,
				exp: exp,
				toString: function() {
					return this.name + '(' + this.args.join(', ') + ') = ' + this.exp.toString() + ';';
				}
			};
		},
		grule: function (name, pattern, args, exp) {
			return {
				kind: 'GRule',
				name: name,
				pattern: pattern,
				args: args,
				exp: exp,
				toString: function() {
					return this.name + '(' + [this.pattern].concat(this.args).join(', ') + 
								') = ' + this.exp.toString() + ';';
				}
			};
		},
		program: function (rules) {
			var p = {
				kind: 'Program',
				rules: rules,
				f: {}, g: {}, gs: {},
				toString: function() {
					return this.rules.join('\n');
				}
			};
			for (var i = 0; i < rules.length; i++) {
				var rule = rules[i];
				switch (rule.kind) {
				case 'FRule': 
					p.f[rule.name] = rule;
					break;
				case 'GRule':
					p.g[rule.name + '_' + rule.pattern.name] = rule;
					if (!p.gs[rule.name]) {
						p.gs[rule.name] = [];
					}
					p.gs[rule.name].push(rule);
					break;
				}
			}
			return p;
		}
};

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
			return new sll_lang.Variable('v_' + i);
		};
	}(),
	
	// test whether e2 is an instance of e1
	instance_of: function(e1, e2) {
		return this.match_against(e1, e2) != null;
	},
	
	equiv: function(e1, e2) {
		return this.instance_of(e1, e2) && this.instance_of(e2, e1);
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
					p.and([
					       t.c_name, 
					       t.lparen, 
					       p.repeat_sep(sll_parser.vrb, t.comma),
					       t.rparen
					       ]
					      ),
					function(r) {return sll_lang.pattern(r[0], r[2]);}
				);
			return p_par(s);
		},
	vrb: 
		function(s) {
			var v_par = 
				p.transform(
					t.v_name,
					function(r) {return sll_lang.variable(r)}
				);
			return v_par(s);
		},
	ctr:
		function(s) {
			var c_par = 
				p.transform(
					p.and([
					       t.c_name,
					       t.lparen,
					       p.repeat_sep(sll_parser.exp, t.comma),
					       t.rparen
					       ]
					      ),
					function(r) {return sll_lang.constructor(r[0], r[2]);}
				);
			return c_par(s);
		},
	fcall:
		function(s) {
			var f_par = 
				p.transform(
					p.and([
					       t.f_name,
					       t.lparen,
					       p.repeat_sep(sll_parser.exp, t.comma),
					       t.rparen
					       ]
					      ),
					function(r) {return sll_lang.fcall(r[0], r[2]);}
				);
			return f_par(s);
		},
	gcall:
		function(s) {
			var g_par = 
				p.transform(
					p.and([
					       t.g_name,
					       t.lparen,
					       p.repeat_sep(sll_parser.exp, t.comma),
					       t.rparen
					       ]
					      ),
					function(r) {return sll_lang.gcall(r[0], r[2]);}
				);
			return g_par(s);
		},
	exp: 
		function(s) {
			var t_par = p.or([
			                  sll_parser.ctr,
			                  sll_parser.fcall,
			                  sll_parser.gcall,
			                  sll_parser.vrb
			                 ]
			                );
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
					function(r) {return sll_lang.frule(r[0], r[2], r[5]);}
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
						return sll_lang.grule(r[0], r[2], vars, r[6]);
					}
				);
			return g_par(s);
		},
	program:
		function(s) {
			var p_par = 
				p.transform(
					p.and([
					       p.repeat(p.or([sll_parser.frule, sll_parser.grule])),
					       t.eof
					       ]
					      ),
					function (r) {return sll_lang.program(r[0]);}
				);
			return p_par(s);
		},
	parse:
		function(s) {
			return sll_parser.program(s.replace(/\s*/g, ''));
		}
};

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
				var ancs = this.ancestors();
				for (var i = 0; i < ancs.length; i ++) {
					if (ancs[i].exp.kind == 'FCall' && sll_algebra.equiv(this.exp, ancs[i].exp)) {
						return true;
					}
				}
				return false;
			default:
				return false;
			}
		},
		toString: function(indent) {
			var ind = indent || '';
			var chs = [];
			for (var i = 0; i < this.children.length; i++) {
				chs.push(this.children[i].toString('    '));
			}
			return [ind + '|__' + exp.toString()].concat(chs).join('\n ');
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
		toString: function() {
			return this.root.toString();
		}
	};
}
