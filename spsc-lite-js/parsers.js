//////////////////////////
//
// Basic Parser Combinator
//
//////////////////////////

var parsers = {
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
				return parsers.success(mx[0], text.substring(mx[0].length));
			} else {
				return parsers.error(text, re);
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
			return parsers.success(final_result, s);
		};
	},
	repeat_sep: function(rule, del_rule) {
		return function(text) {
			var pr1 = rule.call(this, text);
			if (!pr1.successful) {
				return parsers.success([], text);
			}
			var pr2 = parsers.repeat(parsers.and([del_rule, rule]))(pr1.next);
			var result = [pr1.result];
			for (var i = 0; i < pr2.result.length; i++) {
				result.push(pr2.result[i][1]);
			}
			return parsers.success(result, pr2.next);
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
			return parsers.success(result, pr.next);
		};
	},	
	or: function(rules) {
		return function(text) {
			var pr; // last parse result
			for (var i = 0; i < rules.length ; i++) {
				pr = rules[i].call(this, text);
				if (pr.successful) {break;}
			}
			return pr;
		};
	},
	transform: function(rule, fn) {
		return function(text) {
			var pr = rule.call(this, text);
			return pr.successful ? parsers.success(fn(pr.result), pr.next) : pr;
		};
	}
};