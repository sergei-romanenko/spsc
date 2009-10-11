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
	fMain: new sll_lang.FRule(
			'fMain',
			[
			 	new sll_lang.Variable('x'), 
			 	new sll_lang.Variable('y'), 
			 	new sll_lang.Variable('z')
			 ],
			 new sll_lang.GCall(
					 'gApp',
					 [
					  	new sll_lang.GCall('gApp', [new sll_lang.Variable('x'), new sll_lang.Variable('y')]),
					  	new sll_lang.Variable('z')
					  ]
			 )
			),
	gApp1: new sll_lang.GRule(
			'gApp',
			new sll_lang.Pattern('Nil', []),
			[
			 	new sll_lang.Variable('vs1')
			 ],
			new sll_lang.Variable('vs1')
			),
	gApp2: new sll_lang.GRule(
			'gApp',
			new sll_lang.Pattern('Cons', [new sll_lang.Variable('u'), new sll_lang.Variable('us')]),
			[new sll_lang.Variable('vs')],
			new sll_lang.Constructor('Cons', [
			                                  new sll_lang.Variable('u'),
			                                  new sll_lang.GCall('gApp', [new sll_lang.Variable('us'), new sll_lang.Variable('vs')])
			                                  ])
			)
};

var test_terms = function() {
	var var_a = new sll_lang.Variable('a');
	var var_b = new sll_lang.Variable('b');
	var nil = new sll_lang.Constructor('Nil', []);
	var cons_a_nil = new sll_lang.Constructor('Cons', [var_a, nil]);
	var cons_b_nil = new sll_lang.Constructor('Cons', [var_b, nil]);
	var cons_a_b_nil = new sll_lang.Constructor('Cons', [var_a, cons_b_nil]);
	var cons_b_a_nil = new sll_lang.Constructor('Cons', [var_b, cons_a_nil]);
	return {
				var_a: var_a, var_b: var_b, 
				nil: nil, 
				cons_a_nil: cons_a_nil, cons_b_nil: cons_b_nil,
				cons_a_b_nil: cons_a_b_nil, cons_b_a_nil: cons_b_a_nil,
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

var test_all = function() {
	test_algebra_equals();
	test_parser();
	test_algebra_replace_args();
	test_algebra_apply_sub();
	test_algebra_match_against();
}