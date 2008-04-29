// Since there is no built-in equality test,
// we define the equality for the alphabet {A, B, C}.

eqSymb(A, y) = eqA(y);
eqSymb(B, y) = eqB(y);
eqSymb(C, y) = eqC(y);

eqA(A) = True;  eqA(B) = False; eqA(C) = False;
eqB(A) = False; eqB(B) = True;  eqB(C) = False;
eqC(A) = False; eqC(B) = False; eqC(C) = True;
eqD(A) = False; eqD(B) = False; eqD(C) = False;

if(True, x, y) = x;
if(False, x, y) = y;

match(p, u) = loop(p, u, p, u);

loop(Nil, u, q, v) = True;
loop(Cons(h, p), u, q, v) = loopSymb(u, h, p, q, v);

loopSymb(Nil, h, p, q, v) = False;
loopSymb(Cons(g, u), h, p, q, v) = if( eqSymb(h, g), loop(p, u, q, v), next(q, v));

next(Nil, p) = False;
next(Cons(g, u), p) = match(p, u);

matchAAB(u) = match(Cons(A, Cons(A, Cons(B, Nil))), u);  