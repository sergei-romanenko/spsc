//////////////////////////
//
// Basic Parser Combinator
//
//////////////////////////

function success(result, next) {
	return { result: result, next: next, successful: true };
}

function error(s, re) {
	return {
		successful: false,
		message: "cannot match '" + s.substring(0, 10) + " ...' against " + re
	};
}

function token(re) {
	return function (text) {
		var mx = text.match(re);
		if (mx) {
			return success(mx[0], text.substring(mx[0].length));
		} else {
			return error(text, re);
		}
	};
}

function repeat(rule) {
	return function (text) {
		var final_result = [], s = text, pr;
		while (s.length > 0) {
			pr = rule(s);
			if (pr.successful) {
				final_result.push(pr.result);
				s = pr.next;
			} else {
				break;
			}
		}
		return success(final_result, s);
	};
}

function repeat_sep(rule, del_rule) {
	return function (text) {
		var pr1 = rule(text);
		if (!pr1.successful) {
			return success([], text);
		}
		var pr2 = repeat(and([del_rule, rule]))(pr1.next);
		var result = [pr1.result];
		for (var i = 0; i < pr2.result.length; i++) {
			result.push(pr2.result[i][1]);
		}
		return success(result, pr2.next);
	};
}

function and(rules) {
	return function (text) {
		var result = [], s = text, pr = null;
		for (var i = 0; i < rules.length; i++) {
			pr = rules[i](s);
			if (!pr.successful) {
				return pr;
			}
			result.push(pr.result);
			s = pr.next;
		}
		return success(result, pr.next);
	};
}

function or(rules) {
	return function (text) {
		var pr; // last parse result
		for (var i = 0; i < rules.length; i++) {
			pr = rules[i](text);
			if (pr.successful) { break; }
		}
		return pr;
	};
}

function transform(rule, fn) {
	return function (text) {
		var pr = rule(text);
		return pr.successful ? success(fn(pr.result), pr.next) : pr;
	};
}

export { token, repeat, repeat_sep, and, or, transform }
