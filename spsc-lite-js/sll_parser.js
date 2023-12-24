//////////////////////////
//
// SLL Parser
//
//////////////////////////

import * as Lang from "./sll_lang.js"
import { parsers as P } from "./parsers.js"

// tokens

const v_name = P.token(/^[a-z]\w*/)
const c_name = P.token(/^[A-Z]\w*/)
const g_name = P.token(/^g\w*/)
const f_name = P.token(/^f\w*/)
const lparen = P.token(/^\(/)
const rparen = P.token(/^\)/)
const eq = P.token(/^=/)
const comma = P.token(/^,/)
const semicolon = P.token(/^;/)
const eof = P.token(/^$/)

// parsers

function ptr(s) {
	var p_par =
		P.transform(
			P.and([
				c_name,
				lparen,
				P.repeat_sep(vrb, comma),
				rparen
			]
			),
			function (r) { return Lang.pattern(r[0], r[2]); }
		);
	return p_par(s);
}

function vrb(s) {
	var v_par =
		P.transform(
			v_name,
			function (r) { return Lang.variable(r) }
		);
	return v_par(s);
}

function ctr(s) {
	var c_par =
		P.transform(
			P.and([
				c_name,
				lparen,
				P.repeat_sep(exp, comma),
				rparen
			]
			),
			function (r) { return Lang.constructor(r[0], r[2]); }
		);
	return c_par(s);
}

function fcall(s) {
	var f_par =
		P.transform(
			P.and([
				f_name,
				lparen,
				P.repeat_sep(exp, comma),
				rparen
			]
			),
			function (r) { return Lang.fcall(r[0], r[2]); }
		);
	return f_par(s);
}

function gcall(s) {
	var g_par =
		P.transform(
			P.and([
				g_name,
				lparen,
				P.repeat_sep(exp, comma),
				rparen
			]
			),
			function (r) { return Lang.gcall(r[0], r[2]); }
		);
	return g_par(s);
}

function exp(s) {
	var t_par = P.or([
		ctr,
		fcall,
		gcall,
		vrb
	]
	);
	return t_par(s);
}

function frule(s) {
	var f_par =
		P.transform(
			P.and([f_name,
				lparen,
				P.repeat_sep(vrb, comma),
				rparen,
				eq,
				exp,
				semicolon]),
			function (r) { return Lang.frule(r[0], r[2], r[5]); }
		);
	return f_par(s);
}

function grule(s) {
	var g_par =
		P.transform(
			P.and([g_name,
				lparen,
				ptr,
				P.repeat(P.and([comma, vrb])),
				rparen,
				eq,
				exp,
				semicolon]),
			function (r) {
				var vars = [];
				for (var i = 0; i < r[3].length; i++) {
					vars.push(r[3][i][1]);
				}
				return Lang.grule(r[0], r[2], vars, r[6]);
			}
		);
	return g_par(s);
}

function program(s) {
	var p_par =
		P.transform(
			P.and([
				P.repeat(P.or([frule, grule])),
				eof
			]
			),
			function (r) { return Lang.program(r[0]); }
		);
	return p_par(s);
}

function parse(s) {
	return program(s.replace(/\s*/g, ''));
}

function parse_exp(s) {
	var pr = P.and([exp, eof])(s.replace(/\s*/g, ''));
	return pr.result[0];
}

export { parse, parse_exp }
