samples = {
	append2 : {
		rules : [ 'gApp(Nil(), vs1) = vs1;',
		          'gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));' ],
		goal : 'gApp(gApp(x, y), z)'
	},

	fMain : {
		rules : [ 'fMain(x, y, z) = gApp(gApp(x, y), z);',
		          'gApp(Nil(), vs1) = vs1;',
		          'gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));' ],
		goal : 'fMain(x, y, z)'
	},
	
	revert: {
		rules : [ 'gRev(Nil()) = Nil();',
		          'gRev(Cons(x, xs))=gApp(gRev(xs), Cons(x, Nil()));',
		          'gApp(Nil(), vs1) = vs1;',
		          'gApp(Cons(u, us), vs) = Cons(u, gApp(us, vs));' ],
		goal: 'gRev(x)'
	},
	
	add_acc: {
		rules : ['gAddAcc(Z(), y) = y;',
		         'gAddAcc(S(x), y) = gAddAcc(x, S(y));'],
		goal: 'gAddAcc(a, b)'
	},
	
	f: {
		rules: ['f1(x) = f2(x);',
		        'f2(x) = g1(x);',
		        'f3(x) = f1(x);',
		        'g1(A(a)) = f1(a);',
		        'g1(B(b)) = f1(b);'],
		goal: 'f1(z)' 
	},
	
	eqxx: {
		rules: ['gEq(Z(), y) = gEqZ(y);',
		        'gEq(S(x), y) = gEqS(y, x);',
		        'gEqZ(Z()) = True();',
		        'gEqZ(S(x)) = False();',
		        'gEqS(Z(), x) = False();',
		        'gEqS(S(y), x) = gEq(x, y);',
		        'fEqxx(x) = gEq(x, x);'],
		goal: 'fEqxx(x)'
	}, 
	
	gD: {
		rules: ['gD(Z()) = Z();',
		        'gD(S(x)) = gD(S(S(x)));'],
		goal: 'gD(S(x))'
	}

};