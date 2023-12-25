import * as Lang from "./sll_lang.js"
// import * as sll_algebra from "./sll_algebra.js"
import { sll_algebra } from "./sll_algebra.js"
import { tree } from "./partial_process_tree.js"
import * as HE from "./he.js"
import * as  MSG from "./msg.js"

function base_supercompiler(program) {
	return {
		program: program,

		drive: function (e) {
			switch (e.kind) {
				case 'Constructor':
					var res = [];
					for (var i = 0; i < e.args.length; i++) {
						res.push([e.args[i], null]);
					}
					return res;
				case 'FCall':
					var f_rule = program.f[e.name];
					var map = {};
					for (var i = 0; i < e.args.length; i++) {
						map[f_rule.args[i].name] = e.args[i];
					}
					return [[sll_algebra.apply_subst(f_rule.exp, map), null]];
				case 'GCall':
					var arg1 = e.args[0];
					switch (arg1.kind) {
						case 'Constructor':
							var g_rule = program.g[e.name + '_' + arg1.name];
							var map = {};
							for (var i = 0; i < arg1.args.length; i++) {
								map[g_rule.pattern.args[i].name] = arg1.args[i];
							}
							for (var i = 0; i < g_rule.args.length; i++) {
								map[g_rule.args[i].name] = e.args[i + 1];
							}
							return [[sll_algebra.apply_subst(g_rule.exp, map), null]];
						case 'Variable':
							var res = [];
							var g_rules = this.program.gs[e.name];
							for (var i = 0; i < g_rules.length; i++) {
								var fp = this.fresh_pattern(g_rules[i].pattern);
								var fc = Lang.constructor(fp.name, fp.args);
								var map = {};
								map[arg1.name] = fc;
								var x = this.drive(sll_algebra.apply_subst(e, map));
								res.push([x[0][0], [arg1, fp]]);
							}
							return res;
						default:
							var inner_drs = this.drive(arg1);
							var res = [];
							for (var i = 0; i < inner_drs.length; i++) {
								var inner_dr = inner_drs[i];
								var gc = Lang.gcall(e.name, [inner_dr[0]].concat(e.args.slice(1)
									.map(function (arg) {
										if (inner_dr[1]) {
											var map = {}
											map[inner_dr[1][0].name] = Lang.constructor(inner_dr[1][1].name, inner_dr[1][1].args);
											return sll_algebra.apply_subst(arg, map);
										} else {
											return arg;
										}
									})));
								res.push([gc, inner_dr[1]]);
							}
							return res;
					}
				case 'Let':
					var res = [[e.exp, null]];
					for (var i = 0; i < e.bindings.length; i++) {
						res.push([e.bindings[i][1], null]);
					}
					return res;
			}
		},

		fresh_pattern: function (p) {
			var new_args = [];
			for (var i = 0; i < p.args.length; i++) {
				new_args.push(sll_algebra.fresh_var());
			}
			return Lang.pattern(p.name, new_args);
		},

		build_tree: function (exp) {
			var t = tree(exp);
			//console.log(t.toString());
			while (t.get_unprocessed_leaf()) {
				//console.log(t);
				var b = t.get_unprocessed_leaf();
				switch (b.exp.kind) {
					case 'FCall':
					case 'GCall':
						var ancs = b.ancestors();
						var a = null;
						for (var i = 0; i < ancs.length; i++) {
							var _a = ancs[i];
							if (_a.exp.kind == b.exp.kind && sll_algebra.instance_of(_a.exp, b.exp)) {
								a = _a;
								break;
							}
						}
						if (a) {
							var map = sll_algebra.match_against(a.exp, b.exp);
							var bindings = [];
							for (var n in map) {
								bindings.push([n, map[n]]);
							}
							var l = Lang.let_(a.exp, bindings);
							t.replace(b, l);
						} else {
							t.add_children(b, this.drive(b.exp));
						}
						break;
					default:
						t.add_children(b, this.drive(b.exp));
				}
				//console.log(t.toString());
			}
			return t;
		}
	};
};

function supercompiler(program) {

	var s = base_supercompiler(program);

	s.build_tree = function (exp) {
		var t = tree(exp);
		//console.log(t.toString());
		while (t.get_unprocessed_leaf()) {
			//console.log(t);
			var b = t.get_unprocessed_leaf();
			switch (b.exp.kind) {
				case 'FCall':
				case 'GCall':
					var ancs = b.ancestors();
					var a = null;
					for (var i = 0; i < ancs.length; i++) {
						var _a = ancs[i];
						if (HE.smart_he(_a.exp, b.exp)) {
							a = _a;
							break;
						}
					}
					if (a) {
						if (sll_algebra.instance_of(a.exp, b.exp)) {
							this.abs(t, b, a);
						} else if (MSG.msg(a.exp, b.exp).exp.kind == 'Variable') {
							this.split(t, b);
						} else {
							this.abs(t, a, b);
						}
					} else {
						t.add_children(b, this.drive(b.exp));
					}
					break;
				default:
					t.add_children(b, this.drive(b.exp));
			}
			//console.log(t.toString());
		}
		return t;
	};

	s.abs = function (t, a, b) {
		var g = MSG.msg(a.exp, b.exp);
		var map = g.m1;
		var bindings = [];
		for (var n in map) {
			bindings.push([n, map[n]]);
		}
		var l = Lang.let_(g.exp, bindings);
		t.replace(a, l);
	};

	s.split = function (t, n) {
		var fresh_vars = [];
		var bindings = [];
		for (var i = 0; i < n.exp.args.length; i++) {
			var fresh_var = sll_algebra.fresh_var();
			fresh_vars.push(fresh_var);
			bindings.push([fresh_var.name, n.exp.args[i]]);
		}
		var l = Lang.let_(sll_algebra.replace_args(n.exp, fresh_vars), bindings);
		t.replace(n, l);
	};

	return s;
};

export { base_supercompiler, supercompiler }
