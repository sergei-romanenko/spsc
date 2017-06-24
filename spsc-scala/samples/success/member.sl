eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

memberSel(Nil, x) = False;
memberSel(Cons(y, ys), x) = memberEq(eq(x, y), x, ys);

memberEq(True, x, ys) = True;
memberEq(False, x, ys) = memberSel(ys, x);

member(x, ys) = memberSel(ys, x);

memberSZ(list) = member(S(Z), list);
memberZSZ(x) = member(x, Cons(Z, Cons(S(Z), Nil)));
