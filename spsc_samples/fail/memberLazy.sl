eq(Z, y) = eqZ(y);
eq(S(x), y) = eqS(y, x);
eqZ(Z) = True;
eqZ(S(x)) = False;
eqS(Z, x) = False;
eqS(S(y), x) = eq(x, y);

null(Nil) = True;
null(Cons(x, xs)) = False;

head(Cons(x, xs)) = x;
tail(Cons(x, xs)) = xs;

if(True, x, y) = x;
if(False, x, y) = y;

not(x) = if(x, False, True);
or(x, y) = if(x, True, y);
and(x, y) = if(x, y, False);

member(x, list) = and(not(null(list)), or(eq(x, head(list)), member(x, tail(list))));
