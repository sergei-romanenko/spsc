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

var test_all = function() {
	test_algebra_equals();
	test_parser();
}