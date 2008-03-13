var programs = new Array();

programs["appendXYaZ"]=
'append(Nil, vs) = vs;\n\
append(Cons(u, us), vs) = Cons(u, append(us, vs));\n\
appendXYaZ(xs, ys, zs) = append(append(xs, ys), zs);'

programs["appendXYaX"]=
"append(Nil, vs) = vs;\n\
append(Cons(u, us), vs) = Cons(u, append(us, vs));\n\
appendXYaX(xs, ys) = append(append(xs, ys), xs);"

programs["appendXaYZ"]=
"append(Nil, vs) = vs;\n\
append(Cons(u, us), vs) = Cons(u, append(us, vs));\n\
appendXaYZ(xs, ys, zs) = append(xs, append(ys, zs));"

programs["takenm"]=
"from(n) = Cons(n, from(S(n)));\n\
head(Cons(x, xs)) = x;\n\
mapAdd1(Nil) = Nil;\n\
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));\n\
tail(Cons(x, xs)) = xs;\n\
take(Z, xs) = Nil;\n\
take(S(n), xs) = Cons(head(xs), take(n, tail(xs)));\n\
takenm(n, m) = take(n, mapAdd1(from(m)));"

programs["mapfrom"]=
"from(n) = Cons(n, from(S(n)));\n\
mapAdd1(Nil) = Nil;\n\
mapAdd1(Cons(x, xs)) = Cons(S(x), mapAdd1(xs));\n\
mapfrom(n) = mapAdd1(from(n));"

programs["abab"]=
"ab(A(x)) = B(ab(x));\n\
ab(B(x)) = A(ab(x));\n\
abab(x) = ab(ab(x));"

programs["reverse"]=
"app(Nil, vs) = vs;\n\
app(Cons(u, us), vs) = Cons(u, app(us, vs));\n\
rev(Nil) = Nil;\n\
rev(Cons(x, xs)) = app(rev(xs), Cons(x, Nil));\n\
reverse(x)=rev(x);"

programs["eqxx"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
eqxx(x) = eq(x, x);"

programs["eqxSx"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
eqxSx(x) = eq(x, S(x));"

programs["eqSxx"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
eqSxx(x) = eq(S(x), x);"

programs["eqSZx"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
eqSZx(x) = eq(S(Z), x);"

programs["mapNotNot"]=
"not(True) = False;\n\
not(False) = True;\n\
mapNot(Nil) = Nil;\n\
mapNot(Cons(x, xs)) = Cons(not(x), mapNot(xs));\n\
\n\
mapNotNot(xs) = mapNot(mapNot(xs));"

programs["mapNotNotNot"]=
"not(True) = False;\n\
not(False) = True;\n\
mapNot(Nil) = Nil;\n\
mapNot(Cons(x, xs)) = Cons(not(x), mapNot(xs));\n\
\n\
mapNotNotNot(xs) = mapNot(mapNot(mapNot(xs)));"

programs["member"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
\n\
memberSel(Nil, x) = False;\n\
memberSel(Cons(y, ys), x) = memberEq(eq(x, y), x, ys);\n\
\n\
memberEq(True, x, ys) = True;\n\
memberEq(False, x, ys) = memberSel(ys, x);\n\
\n\
member(x, ys) = memberSel(ys, x);"

programs["memberSZ"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
\n\
memberSel(Nil, x) = False;\n\
memberSel(Cons(y, ys), x) = memberEq(eq(x, y), x, ys);\n\
\n\
memberEq(True, x, ys) = True;\n\
memberEq(False, x, ys) = memberSel(ys, x);\n\
\n\
member(x, ys) = memberSel(ys, x);\n\
memberSZ(list) = member(S(Z), list);"

programs["memberZSZ"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
\n\
memberSel(Nil, x) = False;\n\
memberSel(Cons(y, ys), x) = memberEq(eq(x, y), x, ys);\n\
\n\
memberEq(True, x, ys) = True;\n\
memberEq(False, x, ys) = memberSel(ys, x);\n\
\n\
member(x, ys) = memberSel(ys, x);\n\
memberZSZ(x) = member(x, Cons(Z, Cons(S(Z), Nil)));"

programs["memberLazy"]=
"eq(Z, y) = eqZ(y);\n\
eq(S(x), y) = eqS(y, x);\n\
eqZ(Z) = True;\n\
eqZ(S(x)) = False;\n\
eqS(Z, x) = False;\n\
eqS(S(y), x) = eq(x, y);\n\
\n\
null(Nil) = True;\n\
null(Cons(x, xs)) = False;\n\
\n\
head(Cons(x, xs)) = x;\n\
tail(Cons(x, xs)) = xs;\n\
\n\
if(True, x, y) = x;\n\
if(False, x, y) = y;\n\
\n\
not(x) = if(x, False, True);\n\
or(x, y) = if(x, True, y);\n\
and(x, y) = if(x, y, False);\n\
\n\
memberLazy(x, list) = and(not(null(list)), or(eq(x, head(list)), memberLazy(x, tail(list))));"

programs["notornot"]=
"if(True, x, y) = x;\n\
if(False, x, y) = y;\n\
not(x) = if(x, False, True);\n\
or(x, y) = if(x, True, y);\n\
and(x, y) = if(x, y, False);\n\
notornot(x, y) = not(or(not(x), not(y)));"

function sample(sampleName){
  var fname = document.getElementById('fname');
  fname.value = sampleName;
  var programText = document.getElementById('programText');
  programText.value = programs[sampleName];
  return false;	
}