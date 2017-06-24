//////////////////////////
//
// SLL Parser
//
//////////////////////////

var p = parsers;

var sll_parser = {
	// tokens
	v_name: parsers.token(/^[a-z]\w*/), c_name: parsers.token(/^[A-Z]\w*/),
	g_name: parsers.token(/^g\w*/), f_name: parsers.token(/^f\w*/),
	lparen: parsers.token(/^\(/), rparen: parsers.token(/^\)/),
	eq: parsers.token(/^=/), comma: parsers.token(/^,/),
	semicolon: parsers.token(/^;/), eof: parsers.token(/^$/),
	// parsers
	ptr: 
		function(s) { 
			var p_par = 
				p.transform(
					p.and([
					       sll_parser.c_name, 
					       sll_parser.lparen, 
					       p.repeat_sep(sll_parser.vrb, sll_parser.comma),
					       sll_parser.rparen
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
					sll_parser.v_name,
					function(r) {return sll_lang.variable(r)}
				);
			return v_par(s);
		},
	ctr:
		function(s) {
			var c_par = 
				p.transform(
					p.and([
					       sll_parser.c_name,
					       sll_parser.lparen,
					       p.repeat_sep(sll_parser.exp, sll_parser.comma),
					       sll_parser.rparen
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
					       sll_parser.f_name,
					       sll_parser.lparen,
					       p.repeat_sep(sll_parser.exp, sll_parser.comma),
					       sll_parser.rparen
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
					       sll_parser.g_name,
					       sll_parser.lparen,
					       p.repeat_sep(sll_parser.exp, sll_parser.comma),
					       sll_parser.rparen
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
					p.and([sll_parser.f_name, 
					       sll_parser.lparen, 
							p.repeat_sep(sll_parser.vrb, sll_parser.comma), 
							sll_parser.rparen, 
							sll_parser.eq, 
							sll_parser.exp, 
							sll_parser.semicolon]),
					function(r) {return sll_lang.frule(r[0], r[2], r[5]);}
				);
			return f_par(s);
		},
	grule:
		function(s) {
			var g_par = 
				p.transform(
					p.and([sll_parser.g_name, 
					       sll_parser.lparen, 
							sll_parser.ptr, 
							p.repeat(p.and([sll_parser.comma, sll_parser.vrb])), 
							sll_parser.rparen, 
							sll_parser.eq, 
							sll_parser.exp, 
							sll_parser.semicolon]),
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
					       sll_parser.eof
					       ]
					      ),
					function (r) {return sll_lang.program(r[0]);}
				);
			return p_par(s);
		},
	parse:
		function(s) {
			return sll_parser.program(s.replace(/\s*/g, ''));
		},
	parse_exp:
		function(s) {
			var pr = p.and([this.exp, sll_parser.eof])(s.replace(/\s*/g, ''));
			return pr.result[0];
		}
};