import * as Lang from "./sll_lang.js"
// import * as sll_algebra from "./sll_algebra.js"
import { sll_algebra } from "./sll_algebra.js"
import { sll_parser } from "./sll_parser.js"
import { tree } from "./partial_process_tree.js"
import { base_supercompiler, supercompiler } from "./spsc.js"
import { residuator } from "./residuator.js"
import * as HE from "./he.js"
import * as MSG from "./msg.js"

function assert(exp, message) {
	if (!exp) {
		throw message + ' is Failed';
	} else {
		console.log(message + ' is OK');
	}
}

const test_program = {
	code: [
		'fMain(x, y, z) = gApp(gApp(x, y), z);',
		'gApp(Nil(), vs1) = vs1;',
		'gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));'
	].join('\n'),
	code1: [
		'gD(Z()) = Z();',
		'gD(S(x)) = gD(S(S(x)));'
	].join('\n'),
	fMain: Lang.frule(
		'fMain',
		[
			Lang.variable('x'),
			Lang.variable('y'),
			Lang.variable('z')
		],
		Lang.gcall(
			'gApp',
			[
				Lang.gcall('gApp', [Lang.variable('x'), Lang.variable('y')]),
				Lang.variable('z')
			]
		)
	),
	gApp1: Lang.grule(
		'gApp',
		Lang.pattern('Nil', []),
		[Lang.variable('vs1')],
		Lang.variable('vs1')
	),
	gApp2: Lang.grule(
		'gApp',
		Lang.pattern('Cons', [Lang.variable('u'), Lang.variable('us')]),
		[Lang.variable('vs')],
		Lang.constructor('Cons', [Lang.variable('u'),
		Lang.gcall('gApp', [Lang.variable('us'), Lang.variable('vs')])
		])
	)
};

const test_terms = function () {
	var var_a = Lang.variable('a');
	var var_b = Lang.variable('b');
	var nil = Lang.constructor('Nil', []);
	var cons_a_b = Lang.constructor('Cons', [var_a, var_b]);
	var cons_b_a = Lang.constructor('Cons', [var_b, var_a]);
	var cons_a_nil = Lang.constructor('Cons', [var_a, nil]);
	var cons_b_nil = Lang.constructor('Cons', [var_b, nil]);
	var cons_a_a_nil = Lang.constructor('Cons', [var_a, cons_a_nil]);
	var cons_a_b_nil = Lang.constructor('Cons', [var_a, cons_b_nil]);
	var cons_b_a_nil = Lang.constructor('Cons', [var_b, cons_a_nil]);
	var cons_b_b_nil = Lang.constructor('Cons', [var_b, cons_b_nil]);
	return {
		var_a: var_a, var_b: var_b,
		nil: nil,
		cons_a_b: cons_a_b, cons_b_a: cons_b_a,
		cons_a_nil: cons_a_nil, cons_b_nil: cons_b_nil,
		cons_a_a_nil: cons_a_a_nil,
		cons_a_b_nil: cons_a_b_nil,
		cons_b_a_nil: cons_b_a_nil,
		cons_b_b_nil: cons_b_b_nil
	};
}();

var test_algebra_equals = function () {

	assert(sll_algebra.equals(test_terms.var_a, test_terms.var_a), 'a == a');
	assert(!sll_algebra.equals(test_terms.var_a, test_terms.var_b), 'a != b');

	assert(sll_algebra.equals(test_terms.nil, test_terms.nil), 'Nil() == Nil()');
	assert(!sll_algebra.equals(test_terms.cons_b_nil, test_terms.nil), 'Cons(b, Nil()) != Nil()	');
	assert(!sll_algebra.equals(test_terms.nil, test_terms.cons_b_nil), 'Nil() != Cons(b, Nil())');

	assert(!sll_algebra.equals(test_terms.cons_a_nil, test_terms.cons_b_nil), 'Cons(a, Nil()) != Cons(b, Nil());');
	assert(!sll_algebra.equals(test_terms.cons_b_nil, test_terms.cons_a_nil), 'Cons(b, Nil()) != Cons(a, Nil());');
	assert(sll_algebra.equals(test_terms.cons_a_nil, test_terms.cons_a_nil), 'Cons(a, Nil()) == Cons(a, Nil());');

	assert(sll_algebra.equals(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil), 'Cons(a, Cons(b, Nil())) == Cons(a, Cons(b, Nil()))');
	assert(!sll_algebra.equals(test_terms.cons_a_b_nil, test_terms.cons_b_a_nil), 'Cons(a, Cons(b, Nil())) != Cons(b, Cons(a, Nil()))');

}

const test_parser = function () {
	var pr = sll_parser.parse(test_program.code);
	assert(pr.successful, 'This code should be parsed correctly');
	var program = pr.result;
	assert(sll_algebra.equals(test_program.fMain, program.rules[0]), 'Testing fMain');
	assert(sll_algebra.equals(test_program.gApp1, program.rules[1]), 'Testing gApp1');
	assert(sll_algebra.equals(test_program.gApp2, program.rules[2]), 'Testing gApp2');
}

const test_algebra_replace_args = function () {
	var args1 = [test_terms.var_b, test_terms.nil];
	var res1 = sll_algebra.replace_args(test_terms.cons_a_nil, args1);
	assert(sll_algebra.equals(res1, test_terms.cons_b_nil), 'replace1');
}

const test_algebra_apply_sub = function () {
	var sub1 = { 'a': test_terms.var_b };
	var res1 = sll_algebra.apply_subst(test_terms.cons_a_nil, sub1);
	assert(sll_algebra.equals(res1, test_terms.cons_b_nil), 'sub1');

	var sub2 = {};
	var res2 = sll_algebra.apply_subst(test_terms.cons_a_nil, sub2);
	assert(sll_algebra.equals(res2, test_terms.cons_a_nil), 'sub2');
}

const test_algebra_match_against = function () {
	var actual1 = sll_algebra.match_against(test_terms.cons_a_nil, test_terms.cons_b_nil);
	var expect1 = { a: test_terms.var_b };
	assert(sll_algebra.subst_equals(expect1, actual1), 'match1');

	var actual2 = sll_algebra.match_against(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil);
	var expect2 = { a: test_terms.var_a, b: test_terms.var_b };
	assert(sll_algebra.subst_equals(expect2, actual2), 'match2');

	var actual3 = sll_algebra.match_against(test_terms.cons_a_b_nil, test_terms.cons_b_a_nil);
	var expect3 = { a: test_terms.var_b, b: test_terms.var_a };
	assert(sll_algebra.subst_equals(expect3, actual3), 'match3');

	var actual4 = sll_algebra.match_against(test_terms.cons_a_nil, test_terms.nil);
	var expect4 = null;
	assert(sll_algebra.subst_equals(expect4, actual4), 'match4');

	var actual5 = sll_algebra.match_against(test_terms.nil, test_terms.cons_a_nil);
	var expect5 = null;
	assert(sll_algebra.subst_equals(expect5, actual5), 'match5');
}

const test_algebra_instance_of = function () {
	assert(sll_algebra.instance_of(test_terms.nil, test_terms.nil), 'instance_of_1');
	assert(sll_algebra.instance_of(test_terms.var_a, test_terms.nil), 'instance_of_2');
	assert(!sll_algebra.instance_of(test_terms.nil, test_terms.var_a), 'instance_of_3');
	assert(sll_algebra.instance_of(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil), 'instance_of_4');
	assert(!sll_algebra.instance_of(test_terms.cons_a_a_nil, test_terms.cons_a_b_nil), 'instance_of_5');
	assert(sll_algebra.instance_of(test_terms.cons_a_a_nil, test_terms.cons_b_b_nil), 'instance_of_6');
	assert(sll_algebra.instance_of(test_terms.cons_a_b_nil, test_terms.cons_b_b_nil), 'instance_of_7');
	assert(sll_algebra.instance_of(test_terms.cons_a_b, test_terms.cons_b_b_nil), 'instance_of_8');
	assert(sll_algebra.instance_of(test_terms.cons_a_b, test_terms.cons_a_b_nil), 'instance_of_9');
	assert(!sll_algebra.instance_of(test_terms.cons_a_b_nil, test_terms.cons_a_b), 'instance_of_10');
};

const test_algebra_equiv = function () {
	assert(sll_algebra.equiv(test_terms.nil, test_terms.nil), 'equiv_1');
	assert(!sll_algebra.equiv(test_terms.var_a, test_terms.nil), 'equiv_2');
	assert(!sll_algebra.equiv(test_terms.nil, test_terms.var_a), 'equiv_3');
	assert(sll_algebra.equiv(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil), 'equiv_4');
	assert(!sll_algebra.equiv(test_terms.cons_a_a_nil, test_terms.cons_a_b_nil), 'equiv_5');
	assert(sll_algebra.equiv(test_terms.cons_a_a_nil, test_terms.cons_b_b_nil), 'equiv_6');
	assert(!sll_algebra.equiv(test_terms.cons_a_b_nil, test_terms.cons_a_a_nil), 'equiv_7');
};

// // TODO: make it via asserts
// var test_algebra_vars = function() {
// 	var exp1 = sll_parser.parse_exp('Cons(a, b)');
// 	var vars1 = sll_algebra.vars(exp1);
// 	console.log(vars1);

// 	var exp2 = sll_parser.parse_exp('A(x,B(y,z),a)');
// 	var vars2 = sll_algebra.vars(exp2);
// 	console.log(vars2);

// 	var exp3 = sll_parser.parse_exp('A(x,B(y,x),a)');
// 	var vars3 = sll_algebra.vars(exp3);
// 	console.log(vars3);
// };

var test_tree = function () {
	var t = tree(test_terms.cons_a_b_nil);
	console.log(t.toString());
	t.add_children(t.root, [[test_terms.var_a, null], [test_terms.cons_b_nil, null]]);
	console.log(t.toString());

	t.add_children(t.root.children[0], [[test_terms.var_a, null], [test_terms.cons_b_nil, null]]);
	console.log(t.toString());
	return t;
};

// var test_drive = function() {
// 	var exp1 = sll_parser.parse_exp('Cons(a, b)');
// 	var exp2 = sll_parser.parse_exp('gApp(xs, ys)')

// 	var pr = sll_parser.parse(test_program.code).result;
// 	var bsc = base_supercompiler(pr);

// 	console.log(bsc.drive(exp1));
// 	console.log(bsc.drive(exp2));	
// };
// test_drive();



var test_bsc = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);

	var exp1 = sll_parser.parse_exp('gApp(xs, ys)');
	var t1 = bsc.build_tree(exp1);

	var exp2 = sll_parser.parse_exp('gApp(gApp(xs, ys), zs)')
	var t2 = bsc.build_tree(exp2);

	var exp3 = sll_parser.parse_exp('gApp(gApp(xs, ys), Cons(a, b))')
	var t3 = bsc.build_tree(exp3);


	var pr1 = sll_parser.parse(test_program.code1).result;
	var bsc1 = base_supercompiler(pr1);

	var exp4 = sll_parser.parse_exp('gD(S(x))')
	var t4 = bsc1.build_tree(exp4);
};

const test_bsc1 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(xs, ys)')

	console.log('bsc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('bsc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};


const test_bsc2 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(gApp(xs, ys), zs)')

	console.log('bsc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('bsc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

const test_bsc3 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(gApp(xs, ys), Cons(a, b))')

	console.log('bsc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('bsc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

const test_bsc4 = function () {
	var pr = sll_parser.parse(test_program.code1).result;
	var bsc = base_supercompiler(pr);
	var exp = sll_parser.parse_exp('gD(S(x))');

	console.log('bsc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('bsc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

// var test_he_var_attacked = function() {
// 	var exp;
// 	exp = sll_parser.parse_exp('a');
// 	assert(HE.var_attacked(exp), 'a');

// 	exp = sll_parser.parse_exp('A()');
// 	assert(!HE.var_attacked(exp), 'A()');

// 	exp = sll_parser.parse_exp('f(x)');
// 	assert(!HE.var_attacked(exp), 'f(x)');

// 	exp = sll_parser.parse_exp('g(x, y)');
// 	assert(HE.var_attacked(exp), 'g(x, y)');

// 	exp = sll_parser.parse_exp('g1(g2(x))');
// 	assert(HE.var_attacked(exp), 'g1(g2(x))');

// 	exp = sll_parser.parse_exp('g1(A())');
// 	assert(!HE.var_attacked(exp), 'g1(A())');

// 	exp = sll_parser.parse_exp('g1(f(x))');
// 	assert(!HE.var_attacked(exp), 'g1(f(x))');
// };

const test_he = function () {
	assert(HE.he(sll_parser.parse_exp('v1'),
		sll_parser.parse_exp('v1')),
		'v1 ? v2');

	assert(HE.he(sll_parser.parse_exp('v1'),
		sll_parser.parse_exp('F(v2)')),
		'v1 ? F(v2)');

	assert(!HE.he(sll_parser.parse_exp('F(v2)'),
		sll_parser.parse_exp('v1')),
		'F(v2) ? v1');

	assert(HE.he(sll_parser.parse_exp('F(v1)'),
		sll_parser.parse_exp('F(v1)')),
		'F(v1) ? F(v1)');

	assert(HE.he(sll_parser.parse_exp('F(v1)'),
		sll_parser.parse_exp('F(H(v2))')),
		'F(v1) ? F(H(v2))');

	assert(HE.he(sll_parser.parse_exp('F(v1)'),
		sll_parser.parse_exp('G(v0,F(H(v2)))')),
		'F(v1) ? G(v0,F(H(v2)))');

	assert(HE.he(sll_parser.parse_exp('F(v1,G(v2))'),
		sll_parser.parse_exp('F(H(w1),G(w2))')),
		'F(v1,G(v2)) ? F(H(w1),G(w2))');

	assert(!HE.he(sll_parser.parse_exp('f(v1)'),
		sll_parser.parse_exp('g(w1)')),
		'f(v1) ? g(w1)');
};

var test_msg_1 = function () {
	var exp1 = sll_parser.parse_exp('F(v1)');
	var exp2 = sll_parser.parse_exp('F(v2)');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_msg_2 = function () {
	var exp1 = sll_parser.parse_exp('F(v1)');
	var exp2 = sll_parser.parse_exp('G(v2)');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_msg_3 = function () {
	var exp1 = sll_parser.parse_exp('A(a1,C(a2,a3))');
	var exp2 = sll_parser.parse_exp('A(b1,C(b2,b3))');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_msg_4 = function () {
	var exp1 = sll_parser.parse_exp('f(a1,a2,a1)');
	var exp2 = sll_parser.parse_exp('f(b1,b2,b1)');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_msg_5 = function () {
	var exp1 = sll_parser.parse_exp('f(a,a)');
	var exp2 = sll_parser.parse_exp('f(b,S(b))');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_msg_6 = function () {
	var exp1 = sll_parser.parse_exp('gD(S(x))');
	var exp2 = sll_parser.parse_exp('gD(S(S(x)))');
	var gen = MSG.msg(exp1, exp2);
	console.log(gen);
};

var test_sc_1 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var s = supercompiler(pr);
	var bsc = supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(xs, ys)')

	console.log('sc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('sc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};


var test_sc_2 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(gApp(xs, ys), zs)')

	console.log('sc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('sc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

var test_sc_3 = function () {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = supercompiler(pr);
	var exp = sll_parser.parse_exp('gApp(gApp(xs, ys), Cons(a, b))')

	console.log('sc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('sc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

var test_sc_4 = function () {
	var pr = sll_parser.parse(test_program.code1).result;
	var bsc = supercompiler(pr);
	var exp = sll_parser.parse_exp('gD(S(x))');

	console.log('sc:');
	console.log(exp.toString());
	console.log(pr.toString());

	var t = bsc.build_tree(exp);
	var result = residuator(t).residuate();

	console.log('sc result:');
	console.log(result[0].toString());
	console.log(result[1].toString());

	console.log('---');
};

export const test_all = function () {
	test_algebra_equals();
	test_parser();
	test_algebra_replace_args();
	test_algebra_apply_sub();
	test_algebra_match_against();
	test_algebra_instance_of();
	test_algebra_equiv();
	test_tree();

	test_bsc();
	test_bsc1();
	test_bsc2();
	test_bsc3();
	test_bsc4();
	test_he();

	test_msg_1();
	test_msg_2();
	test_msg_3();
	test_msg_4();
	test_msg_5();
	test_msg_6();

	test_sc_1();
	test_sc_2();
	test_sc_3();
	test_sc_4();
};

test_all();