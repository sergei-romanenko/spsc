// Since there is no built-in equality test,
// we define the equality for the alphabet {A, B, C}.

eqSymb(A, y) = eqA(y);
eqSymb(B, y) = eqB(y);
eqSymb(C, y) = eqC(y);

eqA(A) = True;  eqA(B) = False; eqA(C) = False;
eqB(A) = False; eqB(B) = True;  eqB(C) = False;
eqC(A) = False; eqC(B) = False; eqC(C) = True;

or(True, y) = True;
or(False, y) = y;

and(True, y) = y;
and(False, y) = False;

match(p, s) = or(prefix(p, s), next(s, p));

next(Nil, p) = False;
next(Cons(s, ss), p) = match(p, ss);

prefix(Nil, ss) = True;
prefix(Cons(p, ps), ss) = prefixSymb(ss, p, ps);

prefixSymb(Nil, p, ps) = False;
prefixSymb(Cons(s, ss), p, ps) = and(eqSymb(p, s), prefix(ps, ss));

matchAAB(ss) = match(Cons(A, Cons(A, Cons(B, Nil))), ss);  