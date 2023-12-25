import * as Lang from "./sll_lang.js"

function shell_equals(e1, e2) {
	return (e1.kind == e2.kind) && (e1.name == e2.name);
}

function equals(e1, e2) {
	var sh_eq = shell_equals(e1, e2);
	if (!sh_eq) {
		return false;
	}
	if (e1.args.length != e2.args.length) {
		return false;
	}
	for (var i = 0; i < e1.args.length; i++) {
		if (!equals(e1.args[i], e2.args[i])) {
			return false;
		}
	}
	if (e1.pattern && !equals(e1.pattern, e2.pattern)) {
		return false;
	}
	if (e1.exp && !equals(e1.exp, e2.exp)) {
		return false;
	}
	return true;
}

function replace_args(exp, args) {
	return { kind: exp.kind, name: exp.name, args: args, toString: exp.toString };
}

function apply_subst(exp, map) {
	switch (exp.kind) {
		case 'Variable':
			return map[exp.name] || exp;
		default:
			var args = [];
			for (var i = 0; i < exp.args.length; i++) {
				args.push(apply_subst(exp.args[i], map));
			}
			return replace_args(exp, args);
	}
}

function match_against(exp1, exp2) {
	var map = {};
	var walk = function (e1, e2) {
		if (e1.kind == 'Variable') {
			if (map[e1.name]) {
				return equals(map[e1.name], e2);
			} else {
				map[e1.name] = e2;
				return true;
			}
		}
		if (e2.kind == 'Variable') {
			return false;
		}
		if (!shell_equals(e1, e2)) {
			return false;
		}
		for (var i = 0; i < e1.args.length; i++) {
			if (!walk(e1.args[i], e2.args[i])) {
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
}

function subst_equals(sub1, sub2) {
	if (sub1 == null || sub2 == null) {
		return sub1 == sub2;
	}
	for (var n in sub1) {
		if (!sub2[n]) {
			return false;
		}
		if (!equals(sub1[n], sub2[n])) {
			return false;
		}
	}
	for (var n in sub2) {
		if (!sub1[n]) {
			return false;
		}
		if (!equals(sub2[n], sub1[n])) {
			return false;
		}
	}
	return true;
}

const fresh_var = function () {
	var i = 0;
	return function () {
		i++;
		return Lang.variable('v_' + i);
	};
}()

// test whether e2 is an instance of e1
function instance_of(e1, e2) {
	return match_against(e1, e2) != null;
}

function equiv(e1, e2) {
	return instance_of(e1, e2) && instance_of(e2, e1);
}

function vars(exp) {
	switch (exp.kind) {
		case 'Variable':
			return [exp];
		default:
			var all_vars = [];
			for (var i = 0; i < exp.args.length; i++) {
				all_vars = all_vars.concat(vars(exp.args[i]));
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

export {
	shell_equals, equals, replace_args, apply_subst, match_against,
	subst_equals, fresh_var, instance_of, equiv, vars
}
