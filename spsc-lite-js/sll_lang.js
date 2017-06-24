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
				return this.name + '(' + [this.pattern].concat(this.args).join(', ') + ') = ' + this.exp.toString() + ';';
			}
		};
	},
	
	program: function (rules) {
		var p = {
			kind: 'Program',
			rules: rules,
			f: {}, 
			g: {}, 
			gs: {},
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