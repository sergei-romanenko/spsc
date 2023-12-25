import * as Algebra from "./sll_algebra.js"

function smart_he(exp1, exp2) {
	return var_attacked(exp1) == var_attacked(exp2) &&
		he(exp1, exp2);
}

function he(exp1, exp2) {
	return he_by_diving(exp1, exp2) || he_by_coupling(exp1, exp2);
}

function he_by_diving(exp1, exp2) {
	switch (exp2.kind) {
		case 'Constructor':
		case 'FCall':
		case 'GCall':
			for (var i = 0; i < exp2.args.length; i++) {
				if (he(exp1, exp2.args[i])) {
					return true;
				}
			}
			return false;
		default:
			return false;
	}
}

function he_by_coupling(exp1, exp2) {
	if (exp1.kind == 'Let' || exp2.kind == 'Let') {
		return false;
	}
	if (exp1.kind == 'Variable' && exp2.kind == 'Variable') {
		return true;
	}
	if (Algebra.shell_equals(exp1, exp2) && exp1.args.length == exp2.args.length) {
		for (var i = 0; i < exp1.args.length; i++) {
			if (!he(exp1.args[i], exp2.args[i])) {
				return false;
			}
		}
		return true;
	}
	return false;
}

function var_attacked(exp) {
	switch (exp.kind) {
		case 'Variable':
			return true;
		case 'GCall':
			return var_attacked(exp.args[0]);
		default:
			return false;
	}
}

export { smart_he, he, he_by_diving, he_by_coupling, var_attacked }
