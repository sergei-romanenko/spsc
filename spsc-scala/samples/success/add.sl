eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

add(Z, x) = x;
add(S(x), y) = add(S(Z), add(x,y));

e(x, y) = eq(add(x, y), add(y, x));