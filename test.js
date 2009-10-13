function assert(exp, message) {
  if (!exp) {
	  throw message + ' is Failed';
  } else {
	  console.log(message + ' is OK');
  }
}

var test_program = {
	code: [
	   	'fMain(x, y, z) = gApp(gApp(x, y), z);',
		'gApp(Nil(), vs1) = vs1;',
		'gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));'
		].join('\n'),
	fMain: sll_lang.frule(
			'fMain',
			[
			 	sll_lang.variable('x'), 
			 	sll_lang.variable('y'), 
			 	sll_lang.variable('z')
			 ],
			 sll_lang.gcall(
					 'gApp',
					 [
					  	sll_lang.gcall('gApp', [sll_lang.variable('x'), sll_lang.variable('y')]),
					  	sll_lang.variable('z')
					  ]
			 )
			),
	gApp1: sll_lang.grule(
			'gApp',
			sll_lang.pattern('Nil', []),
			[sll_lang.variable('vs1')],
			sll_lang.variable('vs1')
			),
	gApp2: sll_lang.grule(
			'gApp',
			sll_lang.pattern('Cons', [sll_lang.variable('u'), sll_lang.variable('us')]),
			[sll_lang.variable('vs')],
			sll_lang.constructor('Cons', [sll_lang.variable('u'),
			                              sll_lang.gcall('gApp', [sll_lang.variable('us'), sll_lang.variable('vs')])
			                              ])
			)
};

var test_terms = function() {
	var var_a = sll_lang.variable('a');
	var var_b = sll_lang.variable('b');
	var nil = sll_lang.constructor('Nil', []);
	var cons_a_b = sll_lang.constructor('Cons', [var_a, var_b]);
	var cons_b_a = sll_lang.constructor('Cons', [var_b, var_a]);
	var cons_a_nil = sll_lang.constructor('Cons', [var_a, nil]);
	var cons_b_nil = sll_lang.constructor('Cons', [var_b, nil]);
	var cons_a_a_nil = sll_lang.constructor('Cons', [var_a, cons_a_nil]);
	var cons_a_b_nil = sll_lang.constructor('Cons', [var_a, cons_b_nil]);
	var cons_b_a_nil = sll_lang.constructor('Cons', [var_b, cons_a_nil]);
	var cons_b_b_nil = sll_lang.constructor('Cons', [var_b, cons_b_nil]);
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

var test_algebra_equals = function() {
	
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

var test_parser = function() {	
	var pr = sll_parser.parse(test_program.code);
	assert(pr.successful, 'This code should be parsed correctly');
	var program = pr.result;
	assert(sll_algebra.equals(test_program.fMain, program.rules[0]), 'Testing fMain');
	assert(sll_algebra.equals(test_program.gApp1, program.rules[1]), 'Testing gApp1');
	assert(sll_algebra.equals(test_program.gApp2, program.rules[2]), 'Testing gApp2');
}

var test_algebra_replace_args = function() {
	var args1 = [test_terms.var_b, test_terms.nil];
	var res1 = sll_algebra.replace_args(test_terms.cons_a_nil, args1);
	assert(sll_algebra.equals(res1, test_terms.cons_b_nil), 'replace1');
}

var test_algebra_apply_sub = function() {
	var sub1 = {'a': test_terms.var_b};
	var res1 = sll_algebra.apply_subst(test_terms.cons_a_nil, sub1);
	assert(sll_algebra.equals(res1, test_terms.cons_b_nil), 'sub1');
	
	var sub2 = {};
	var res2 = sll_algebra.apply_subst(test_terms.cons_a_nil, sub2);
	assert(sll_algebra.equals(res2, test_terms.cons_a_nil), 'sub2');
}

var test_algebra_match_against = function() {
	var actual1 = sll_algebra.match_against(test_terms.cons_a_nil, test_terms.cons_b_nil);
	var expect1 = {a: test_terms.var_b};
	assert(sll_algebra.subst_equals(expect1, actual1), 'match1');
	
	var actual2 = sll_algebra.match_against(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil);
	var expect2 = {a: test_terms.var_a, b: test_terms.var_b};
	assert(sll_algebra.subst_equals(expect2, actual2), 'match2');
	
	var actual3 = sll_algebra.match_against(test_terms.cons_a_b_nil, test_terms.cons_b_a_nil);
	var expect3 = {a: test_terms.var_b, b: test_terms.var_a};
	assert(sll_algebra.subst_equals(expect3, actual3), 'match3');
	
	var actual4 = sll_algebra.match_against(test_terms.cons_a_nil, test_terms.nil);
	var expect4 = null;
	assert(sll_algebra.subst_equals(expect4, actual4), 'match4');
	
	var actual5 = sll_algebra.match_against(test_terms.nil, test_terms.cons_a_nil);
	var expect5 = null;
	assert(sll_algebra.subst_equals(expect5, actual5), 'match5');
}

var test_algebra_instance_of = function() {
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

var test_algebra_equiv = function() {
	assert(sll_algebra.equiv(test_terms.nil, test_terms.nil), 'equiv_1');
	assert(!sll_algebra.equiv(test_terms.var_a, test_terms.nil), 'equiv_2');
	assert(!sll_algebra.equiv(test_terms.nil, test_terms.var_a), 'equiv_3');
	assert(sll_algebra.equiv(test_terms.cons_a_b_nil, test_terms.cons_a_b_nil), 'equiv_4');
	assert(!sll_algebra.equiv(test_terms.cons_a_a_nil, test_terms.cons_a_b_nil), 'equiv_5');
	assert(sll_algebra.equiv(test_terms.cons_a_a_nil, test_terms.cons_b_b_nil), 'equiv_6');
	assert(!sll_algebra.equiv(test_terms.cons_a_b_nil, test_terms.cons_a_a_nil), 'equiv_7');	
}

var test_tree = function() {
	var t = tree(test_terms.cons_a_b_nil);
	console.log(t.toString());
	t.add_children(t.root, [[test_terms.var_a, null],[test_terms.cons_b_nil, null]]);
	console.log(t.toString());
	
	t.add_children(t.root.children[0], [[test_terms.var_a, null],[test_terms.cons_b_nil, null]]);
	console.log(t.toString());
	return t;
}

var test_drive = function() {
	var exp1 = sll_parser.parse_exp('Cons(a, b)');
	var exp2 = sll_parser.parse_exp('gApp(xs, ys)')
	
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);
	
	console.log(bsc.drive(exp1));
	console.log(bsc.drive(exp2));	
}

var test_bsc = function() {
	var pr = sll_parser.parse(test_program.code).result;
	var bsc = base_supercompiler(pr);
	
	var exp1 = sll_parser.parse_exp('gApp(xs, ys)')
	var t1 = bsc.build_tree(exp1);
}

var test_all = function() {
	test_algebra_equals();
	test_parser();
	test_algebra_replace_args();
	test_algebra_apply_sub();
	test_algebra_match_against();
	test_algebra_instance_of();
	test_algebra_equiv();
	test_tree();
	test_bsc();
}